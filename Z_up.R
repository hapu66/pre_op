# make start_up.R
#
 
# tools
  library(tidyverse)
  library(magrittr)
  library(gtsummary)    
  library(gt)      
# help
pc1 <- function(x) round( 100*x, 1)
pc2 <- function(x) round( 100*x, 2)
#
# set up  data tables
##################################################################### read data
grunnmappe = "\\\\ihelse.net\\kvalitetsregister\\HBE\\2013-1189\\"
#
#
ddmappe = paste0(grunnmappe, "datadumpar\\")
anlzmappe = paste0(grunnmappe, "Hannu\\")

# Hent datoen til siste tilgjengelege uttrekk
dato_uttrekk = list.dirs(ddmappe, recursive = FALSE, full.names = FALSE) %>%
  sort %>%
  last %>% 
  as.Date
#
# dato_uttrekk <- as.Date("2022-04-01")      ####    overstyr datadump-date
datamappe = paste0(ddmappe, dato_uttrekk, "\\")
avnmappe = paste0(datamappe, "AlleVarNum")
#####  1. read in data       #####                                     ----- 80
#
prc <- function(x) round( 100*x, 1)
setwd(avnmappe)
fil_PBV = paste0("PatBasVarNum.csv")         ## dd-dato = 3. 8.2023                    
fil_Opr = paste0("OperasjonsVarNum.csv")
fil_u6k = paste0("SeksUkerOppfNum.csv")
# k_bok = paste0("SOReg_klokeboken_2023-08-07.csv") # kloke-boka

PBV <-  read_csv2(fil_PBV) # 13990
Opr <-  read_csv2(fil_Opr)
u6k <-  read_csv2(fil_u6k)
# kb <- read_csv2(k_bok)     
AVN <- PBV %>%             # gjenskape AlleVarNum
  left_join(Opr, by = c("p_pasientid", "ForlopsID", "p_opid")) %>% 
  left_join(u6k, by =c("p_pasientid", "ForlopsID", "p_opid"))

fil_Ars =   paste0("SOReg_DatadumpArsrapport_datadump_", dato_uttrekk,".csv")
fil_1 = paste0("SOReg_Arskontrollar1_datadump_", dato_uttrekk,".csv") 
fil_2 = paste0("SOReg_Arskontrollar2_datadump_", dato_uttrekk,".csv") 
fil_5 = paste0("SOReg_Arskontrollar5_datadump_", dato_uttrekk,".csv") 
 
setwd(datamappe)
# ------------------------------------- read in Avn data
# AVN  <- read_csv2(fil_AVN)
Ars  <- read_csv2(fil_Ars)   ## alternativ dd Årsrapport
# ---------------------------## ----------------- read in årskontrolldata -- 80
A1 <- read_csv2(fil_1)
A2 <- read_csv2(fil_2)
A5 <- read_csv2(fil_5)
#
# setwd(anlzmappe) 
setwd(r"(C:\Users\hanlyy\OneDrive - Helse Vest\H-dokumenter\pre_operativ\pre-op-WL-school)")
################################################################### join tables
df<-AVN %>% 
  left_join(A1, by=c("p_pasientid","ForlopsID","p_opid") ) %>% 
  left_join(A2, by=c("p_pasientid","ForlopsID","p_opid") ) %>% 
  left_join(A5, by=c("p_pasientid","ForlopsID","p_opid") )
alle_sh <-unique(df$o_sykehus)

# dg<-Ars %>% 
#   left_join(A1, by=c("PasientID"="p_pasientid","ForlopsID") ) %>%
#   left_join(A2, by=c("PasientID"="p_pasientid","ForlopsID") )

# Legg til info om operasjonsC%r, BMI, fedmerel, etc
df  <- df %>% 
  mutate(op_aar = year(o_dato_op),
         op_mnd = month(o_dato_op),
         op_primar = (o_tidl_fedmeop == 0),
         Sex = ifelse( p_kjonn==2, "F", "M"),
         Female = Sex=="F",
         smoke = (b_royk==1 | b_royk == 3),  # 3  Tilfeldig røykeslutt før operasjon ?
         work = b_ssv_inntarb==1) %>%   
  select(-p_kjonn)
#  Informasjon om revisjonsoperasjonar
df <- df %>% group_by(p_pasientid) %>% mutate(N_revop = n()-1) %>% ungroup()
#--------------------------------------------  BMI, implisitte fedmerelaterte
df$a2_dato_oppf  = as.Date(df$a2_dato_oppf, format(c("%d.%m.%Y")))
df$a5_dato_oppf  = as.Date(df$a5_dato_oppf, format(c("%d.%m.%Y")))

 
df %<>% mutate( b_finans = dplyr::recode(b_finans, "1" = "Public", "2" = "Private", "3" = "Private", .default=NA_character_),
               bmi_baseline = b_ant_vekt/(b_ant_hoyde/100)^2,
               bmi_op = o_ant_vekt/(b_ant_hoyde/100)^2,
               bmi_6v = u6_ant_vekt/(u6_ant_hoyde/100)^2,
               bmi_1a = a1_ant_vekt/(a1_ant_hoyde/100)^2,
               bmi_2a = a2_ant_vekt/(a2_ant_hoyde/100)^2,
               bmi_5a = a5_ant_vekt/(a5_ant_hoyde/100)^2,
               a2_TWL =  100*(b_ant_vekt - a2_ant_vekt)/b_ant_vekt, # %
               a2_ds =  a2_dato_oppf  - o_dato_op,
               a5_ds =  a5_dato_oppf  - o_dato_op,
               a5_TWL =  100*(b_ant_vekt - a5_ant_vekt)/b_ant_vekt, # %
               b_beh_sovnap  =   ifelse(b_beh==0, 0, b_beh_sovnap),    
               b_beh_hypert  =   ifelse(b_beh==0, 0, b_beh_hypert),
               b_beh_diab  =   ifelse(b_beh==0, 0, b_beh_diab),
               b_beh_dyslip  =   ifelse(b_beh==0, 0, b_beh_dyslip),
               b_beh_dyspepsi  =   ifelse(b_beh==0, 0, b_beh_dyspepsi),
               b_beh_depr  =   ifelse(b_beh==0, 0, b_beh_depr),
               b_beh_musk_skjsm  =   ifelse(b_beh==0, 0, b_beh_musk_skjsm),
               a1_beh_sovnap  =   ifelse(a1_beh==0, 0, a1_beh_sovnap),   
               a1_beh_hypert  =   ifelse(a1_beh==0, 0, a1_beh_hypert),
               a1_beh_diab  =   ifelse(a1_beh==0, 0, a1_beh_diab),
               a1_beh_dyslip  =   ifelse(a1_beh==0, 0, a1_beh_dyslip),
               a1_beh_dyspepsi  =   ifelse(a1_beh==0, 0, a1_beh_dyspepsi),
               a1_beh_diare  =   ifelse(a1_beh==0, 0, a1_beh_diare),
               a1_beh_depr  =   ifelse(a1_beh==0, 0, a1_beh_depr),
               a1_beh_musk_skjsm =   ifelse(a1_beh==0, 0, a1_beh_musk_skjsm),
               a2_beh_sovnap  =   ifelse(a2_beh==0, 0, a2_beh_sovnap),   
               a2_beh_hypert  =   ifelse(a2_beh==0, 0, a2_beh_hypert),
               a2_beh_diab  =   ifelse(a2_beh==0, 0, a2_beh_diab),
               a2_beh_dyslip  =   ifelse(a2_beh==0, 0, a2_beh_dyslip),
               a2_beh_dyspepsi  =   ifelse(a2_beh==0, 0, a2_beh_dyspepsi),
               a2_beh_diare  =   ifelse(a2_beh==0, 0, a2_beh_diare),
               a2_beh_depr  =   ifelse(a2_beh==0, 0, a2_beh_depr),
               a2_beh_musk_skjsm  =   ifelse(a2_beh==0, 0, a2_beh_musk_skjsm),
               a5_beh_sovnap  =   ifelse(a5_beh==0, 0, a5_beh_sovnap),   
               a5_beh_hypert  =   ifelse(a5_beh==0, 0, a5_beh_hypert),
               a5_beh_diab  =   ifelse(a5_beh==0, 0, a5_beh_diab),
               a5_beh_dyslip  =   ifelse(a5_beh==0, 0, a5_beh_dyslip),
               a5_beh_dyspepsi  =   ifelse(a5_beh==0, 0, a5_beh_dyspepsi),
               a5_beh_diare  =   ifelse(a5_beh==0, 0, a5_beh_diare),
               a5_beh_depr  =   ifelse(a5_beh==0, 0, a5_beh_depr),
               a5_beh_musk_skjsm  =   ifelse(a5_beh==0, 0, a5_beh_musk_skjsm),  
               opr =  case_when( o_opmetode==6 ~ "S", 
                                 (o_opmetode==1 & o_gbp_type == 1)|(o_opmetode==1 & is.na(o_gbp_type)) ~"B" )  )
#  o_opmetode==1 & o_gbp_type == 2 ~"O"))

# -----------------------  variables for table 2
df %<>% mutate( vt_pr = b_ant_kmi - bmi_op,     # d BMI  
                TWL_pr = (b_ant_vekt - o_ant_vekt)/ b_ant_vekt *100,
                vent = o_dato_op - b_dato_henv,
                ligg = u6_dato_ut - o_dato_op,  #  -u6_pop_ligg
                ligg_mn4 = ligg>3,
                reinn = u6_innl_sykeh_d0_30 == 1,    #  0 Nei 1 Ja    2 Vet ikke
                alv_komp = u6_komp_alvgrad > 3,             #    4. Grad III-b: Signifikant intervensjon i narkose.
                alv_komp_na = !alv_komp | is.na(alv_komp),  #  not serious compl.
                alv_kmp = !alv_komp_na,
                subst = a5_sub,
                depr = b_beh_depr,
                vtap = a5_TWL,
                dBMI = bmi_5a - bmi_baseline )
df$vent = as.integer(df$vent)
df$ligg = as.integer(df$ligg)
 
#2023-09-11:: For Stavanger og Bergen kan du sette alle pasientar til «Nei» på vektskule. 
df <- df %>% mutate(o_preop_vektskole = ifelse(o_sykehus %in% c("Helse Stavanger HF", "Helse Bergen HF"), 0, o_preop_vektskole))
####################################     FILTER ------------------------------
## F1 primary operation?
d_prim <- df %>% filter(op_primar)  #   primær
d_revop <- df %>% filter(!op_primar)

dt =  d_prim %>% 
  mutate(u6_fu =  u6_ferdigstill == 1 & u6_oppf_type %in% c(1, 2, 3),
         a5_fu = a5_ferdigstill == 1, 
         a5_nt =  a5_ferdigstill == 1 & a5_dato_oppf - o_dato_op < 2007 & a5_dato_oppf - o_dato_op > 1642 &
         a5_oppf_type %in%  c(1,2,3) &
   !is.na(a5_ant_vekt)  & a5_ant_vekt > 0,   # fu a5 i normtid,   3 st wrong with a5_ant_vekt == 0
         trt =   case_when(
           o_preop_vektskole==1   ~ "EPEP",
           o_preop_vektskole==0   ~ "SPEP"   ))

d =    dt %>% filter(!is.na(o_preop_vektskole), o_opmetode %in% c(1, 6)) %>%
  select(p_pasientid, ForlopsID, o_sykehus,  o_dato_op, p_alder_v_op, Sex, Female, 
         o_preop_vektskole, o_preop_vektprog, o_opmetode, smoke, work,
         b_finans,  u6_ferdigstill, u6_oppf_type, a5_oppf_type,
         contains("bmi_"),  contains("b_beh"),  vt_pr, TWL_pr,
         vent,   ligg_mn4,   alv_kmp,  subst,  N_revop,
         reinn,  depr,   vtap,   dBMI,   u6_fu, a5_fu, a5_nt, trt,
         o_dato_op, a5_ferdigstill, a5_ant_vekt, a5_dato_oppf, bmi_5a,  a5_TWL) %>% 
  mutate(bmi_0 = bmi_baseline, bmi_0o = bmi_op, bmi_0u6 = bmi_6v) %>%
  select(-bmi_baseline, -bmi_op, -bmi_6v)

eo_dato_d30 = as.Date("2023-06-30")
eo_dato_a5 = as.Date("2018-04-15")

#  Sys.Date() - years(5) - months(6)
#  [1] NA         on 2023-10-31
#
## d_elig    =  d %>% filter(o_dato_op <  Sys.Date() - years(5) - months(6))  
## Setting up data tables:
d_elig    =  d %>% filter(o_dato_op <  Sys.Date() - days(2007))  # -5.5 yr prb: 2023-10-31

d_elig_GS  = d_elig  %>% filter(o_opmetode == 6)
d_elig_GB  = d_elig  %>% filter(o_opmetode == 1)

d_elig_d30 = d %>% filter(o_dato_op <  Sys.Date()   - months(3))  # - 3 m
d_elig_d30_GS = d_elig_d30 %>% filter(o_opmetode == 6)
d_elig_d30_GB = d_elig_d30 %>% filter(o_opmetode == 1)

d_act_d30 = d_elig_d30 %>% filter(u6_fu == 1)  # follow-up 30d
d_act_d30_GS = d_act_d30 %>% filter(o_opmetode == 6)
d_act_d30_GB = d_act_d30 %>% filter(o_opmetode == 1)

d_act_a5 = d %>% filter(a5_nt)  # follow-up 5 yr
d_act_a5_GS = d_act_a5 %>% filter(o_opmetode == 6)  # OBS normtid = +- 6 months
d_act_a5_GB = d_act_a5 %>% filter(o_opmetode == 1)
## -----

# df |> filter(o_sykehus == "Vestre Viken HF", !is.na(a5_dato_oppf))  |> pull(o_opmetode) |> table()

# 1   6 
# 106  49 


 


