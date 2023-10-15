######################### -------------------------------    Tool libraries
library(tidyverse)
library(lubridate)
#  
library(magrittr)
library(rapwhale)  # local
library(pointblank)
library(ggdist)
library(table1)
library(flextable)
library(gt)
library(gtsummary)
library(officer)
#
##################################################################### purpose
#       create canonical set-up
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
# Mappe til dei siste datafila
#
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
k_bok = paste0("SOReg_klokeboken_2023-08-07.csv") # kloke-boka

PBV <-  read_csv2(fil_PBV) # 13990
Opr <-  read_csv2(fil_Opr)
u6k <-  read_csv2(fil_u6k)
kb <- read_csv2(k_bok)     
AVN <- PBV %>%             # gjenskape AlleVarNum
  left_join(Opr, by = c("p_pasientid", "ForlopsID", "p_opid")) %>% 
  left_join(u6k, by =c("p_pasientid", "ForlopsID", "p_opid"))

fil_Ars =   paste0("SOReg_DatadumpArsrapport_datadump_", dato_uttrekk,".csv")
fil_1 = paste0("SOReg_Arskontrollar1_datadump_", dato_uttrekk,".csv") 
fil_2 = paste0("SOReg_Arskontrollar2_datadump_", dato_uttrekk,".csv") 
fil_5 = paste0("SOReg_Arskontrollar5_datadump_", dato_uttrekk,".csv") 
klke = read_csv2(k_bok)

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
setwd(r"(C:\Users\hanlyy\OneDrive - Helse Vest\H-dokumenter\pre_operativ)")
################################################################### join tables
df<-AVN %>% 
  left_join(A1, by=c("p_pasientid","ForlopsID","p_opid") ) %>% 
  left_join(A2, by=c("p_pasientid","ForlopsID","p_opid") ) %>% 
  left_join(A5, by=c("p_pasientid","ForlopsID","p_opid") )
alle_sh <-unique(df$o_sykehus)

dg<-Ars %>% 
   left_join(A1, by=c("PasientID"="p_pasientid","ForlopsID") ) %>%
   left_join(A2, by=c("PasientID"="p_pasientid","ForlopsID") )

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
#  re-kode fedmerelaterte sykdommar,  pakke=dplyr !!
df %<>% mutate(  a1_beh  =  dplyr::recode(a1_beh , "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_beh_diare =  dplyr::recode(a1_beh_diare, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_ann_sykd =  dplyr::recode(a1_ann_sykd, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh  =  dplyr::recode(a2_beh , "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh_diare =  dplyr::recode(a2_beh_diare, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_ann_sykd =  dplyr::recode(a2_ann_sykd, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_beh  =  dplyr::recode(a5_beh , "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_beh_diare =  dplyr::recode(a5_beh_diare, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_ann_sykd =  dplyr::recode(a5_ann_sykd, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_beh_sovnap =  dplyr::recode(a1_beh_sovnap, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_beh_hypert = dplyr::recode(a1_beh_hypert, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_beh_diab = dplyr::recode(a1_beh_diab, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_beh_dyslip = dplyr::recode(a1_beh_dyslip, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_beh_dyspepsi = dplyr::recode(a1_beh_dyspepsi, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_beh_depr = dplyr::recode(a1_beh_depr, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_beh_musk_skjsm = dplyr::recode(a1_beh_musk_skjsm, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh_sovnap = dplyr::recode(a2_beh_sovnap, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh_hypert = dplyr::recode(a2_beh_hypert, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh_diab = dplyr::recode(a2_beh_diab, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh_dyslip = dplyr::recode(a2_beh_dyslip, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh_dyspepsi = dplyr::recode(a2_beh_dyspepsi, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh_depr = dplyr::recode(a2_beh_depr, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh_musk_skjsm = dplyr::recode(a2_beh_musk_skjsm, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_beh_sovnap = dplyr::recode(a5_beh_sovnap, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_beh_hypert = dplyr::recode(a5_beh_hypert, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_beh_diab = dplyr::recode(a5_beh_diab, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_beh_dyslip = dplyr::recode(a5_beh_dyslip, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_beh_dyspepsi = dplyr::recode(a5_beh_dyspepsi, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_beh_depr = dplyr::recode(a5_beh_depr, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_beh_musk_skjsm = dplyr::recode(a5_beh_musk_skjsm, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_)
)
#--------------------------------------------  BMI, implisitte fedmerelaterte
df %<>% mutate(bmi_baseline = b_ant_vekt/(b_ant_hoyde/100)^2,
               bmi_op = o_ant_vekt/(b_ant_hoyde/100)^2,
               bmi_6v = u6_ant_vekt/(u6_ant_hoyde/100)^2,
               bmi_1a = a1_ant_vekt/(a1_ant_hoyde/100)^2,
               bmi_2a = a2_ant_vekt/(a2_ant_hoyde/100)^2,
               bmi_5a = a5_ant_vekt/(a5_ant_hoyde/100)^2,
               a2_TWL =  100*(b_ant_vekt - a2_ant_vekt)/b_ant_vekt, # %
               a2_ds = a2_dato_oppf - o_dato_op,
               a5_ds = a5_dato_oppf - o_dato_op,
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

# -----------------------  variablene for tabell 2
df %<>% mutate( pr_vt = b_ant_vekt - o_ant_vekt,  
                vent = o_dato_op - b_dato_henv,
                ligg = u6_dato_ut - o_dato_op,  #  -u6_pop_ligg
                reinn = u6_innl_sykeh_d0_30 == 1,    #  0 Nei 1 Ja    2 Vet ikke
                alv_komp = u6_komp_alvgrad > 3,             #    4. Grad III-b: Signifikant intervensjon i narkose.
                  alv_komp_na = !alv_komp | is.na(alv_komp),  #  not serious compl.
                    alv_kmp = !alv_komp_na,
                subst = u6_sub,
                depr = b_beh_depr,
                vtap = a5_TWL,
                dBMI = bmi_5a - bmi_baseline )


df$vent = as.integer(df$vent)
df$ligg = as.integer(df$ligg)
# ----



prc <- function(x) round( 100*x, 1)
pc <- function(x) round( 100*x, 2)

base_chr <- function(df){
  df %>% mutate(F = Sex=="F", 
                S = o_opmetode==6, 
                B = (o_opmetode==1 & o_gbp_type == 1)|(o_opmetode==1 & is.na(o_gbp_type)) , 
                O =  o_opmetode==1 & o_gbp_type == 2) %>%  summarise(n= n(),
                                                                     Female = pc( mean(F)), 
                                                                     Age = mean(p_alder_v_op, na.rm = T), 
                                                                     bmi_b =   mean(bmi_baseline),
                                                                     GS = pc(mean(S, na.rm = T)),
                                                                     GB = pc(mean(B, na.rm = T)),
                                                                     OA = pc(mean(O, na.rm = T)))
}


####################################     FILTER ------------------------------
## F0 ferdigstilt?
d_u6 <- df %>% filter( p_ferdigstill ==1, b_ferdigstill==1, u6_ferdigstill==1)
d_a1 <- d_u6 %>% filter( a1_ferdigstill==1)
d_a2 <- d_a1 %>% filter(  a2_ferdigstill==1)
d_a5 <- d_a2 %>% filter(   a5_ferdigstill==1)  # 3941

d_a5_only <- df %>% filter(   a5_ferdigstill==1)  # samme 3941 ?

df %>% filter( (a1_ferdigstill !=1 | a2_ferdigstill !=1 ) & a5_ferdigstill ==1)  # 0 st
df %>% filter( a1_ferdigstill !=1  & a2_ferdigstill ==1)
df %>% filter( u6_ferdigstill !=1  & a1_ferdigstill ==1)
## F1 primary operation?
    d_prim <- df %>% filter(op_primar)  # ferdigstilt?
    d_revop <- df %>% filter(!op_primar)
## F2 operation year(s)?
    d_3aar <- df %>% filter(op_aar %in% 2019:2021)
## F3 operation age, sex?
    d_Fem <- df %>% filter(Sex == "F")
## F4 RHF?
    d_vest <- df %>% filter(o_sykehus %in% c("Helse Bergen HF", "Helse Stavanger HF",
                                             "Helse Førde HF", "Helse Fonna HF"))
## F5 peri-operative variables?
##    o_sg_b_stor, o_sg_pylor_cm, o_sg_his_cm
## F6 follow-up time(s) a1,a2?
## F7 other filters?
    d2_prim <- d_prim %>% filter( p_ferdigstill ==1, b_ferdigstill==1, u6_ferdigstill==1,
                                   a1_ferdigstill==1, a2_ferdigstill==1)
    d5_prim <- d_prim %>% filter( p_ferdigstill ==1, b_ferdigstill==1, u6_ferdigstill==1,
                                   a1_ferdigstill==1, a2_ferdigstill==1, a5_ferdigstill==1)
    
    dt =  d_prim %>% 
      mutate(a5_fu = a5_ferdigstill == 1, 
              trt =   case_when(
                        o_preop_vektskole==1 & o_opmetode ==1 ~ "RYGB school",
                        o_preop_vektskole==0 & o_opmetode ==1 ~ "RYGB norm",
                        o_preop_vektskole==1 & o_opmetode ==6 ~ "GS school",
                        o_preop_vektskole==0 & o_opmetode ==6 ~ "GS norm"))
    
    dt %<>% mutate( vent_a5 = ifelse(a5_fu, vent, NA_integer_),
                    ligg_a5 = ifelse(a5_fu, ligg, NA_integer_),
                    alv_kmp_a5 = ifelse(a5_fu, alv_kmp, NA),
                    subst_a5 = ifelse(a5_fu, subst, NA),
                    reinn_a5 = ifelse(a5_fu, reinn, NA),
                   depr_a5 = ifelse(a5_fu, depr, NA),
                   vtap_a5 = ifelse(a5_fu, vtap, NA_real_),
                   dBMI_a5 = ifelse(a5_fu, dBMI, NA_real_)
    )
    
##    ligg, alv_kmp, subst,
##    reinn, depr, vtap, dBMI
    
  #####################################     ANALYZE ----------------------------
 
    dt %>% filter(!is.na(o_preop_vektskole), o_opmetode %in% c(1, 6)) %>% 
      group_by(o_preop_vektskole, o_opmetode) %>% 
      summarise(N = n(),              
                v_b = mean(b_ant_vekt, na.rm = TRUE), v_b_sd = sd(b_ant_vekt, na.rm = TRUE),
                v_o = mean(o_ant_vekt, na.rm = TRUE), v_b_sd = sd(o_ant_vekt, na.rm = TRUE),
                v_a2 = mean(a2_ant_vekt, na.rm = TRUE), v_a2_sd = sd(a2_ant_vekt, na.rm = TRUE),
                v_a5 = mean(a5_ant_vekt, na.rm = TRUE), v_a5_sd = sd(a5_ant_vekt, na.rm = TRUE))
 
    # bmi
    dt %>% filter(!is.na(o_preop_vektskole), o_opmetode %in% c(1, 6)) %>% 
      group_by(o_preop_vektskole, o_opmetode) %>% 
      summarise(N = n(),              
                b_bmi = median(bmi_baseline, na.rm = TRUE), b_bmi_sd = sd(bmi_baseline, na.rm = TRUE),
                o_bmi = median(bmi_op, na.rm = TRUE), o_bmi_sd = sd(bmi_op, na.rm = TRUE),
                a2_bmi = median(bmi_2a, na.rm = TRUE), a2_bmi_sd = sd(bmi_2a, na.rm = TRUE),
                a5_bmi = median(bmi_5a, na.rm = TRUE), a5_bmi_sd = sd(bmi_5a, na.rm = TRUE))
    
    dt %>% filter(!is.na(o_preop_vektskole), o_opmetode %in% c(1, 6)) %>% 
      group_by(o_preop_vektskole, o_opmetode) %>% 
      summarise(N = n(),              
                b_bmi = mean(bmi_baseline, na.rm = TRUE), b_bmi_sd = sd(bmi_baseline, na.rm = TRUE),
                o_bmi = mean(bmi_op, na.rm = TRUE), o_bmi_sd = sd(bmi_op, na.rm = TRUE),
                a2_bmi = mean(bmi_2a, na.rm = TRUE), a2_bmi_sd = sd(bmi_2a, na.rm = TRUE),
                a5_bmi = mean(bmi_5a, na.rm = TRUE), a5_bmi_sd = sd(bmi_5a, na.rm = TRUE),
                b_bmi_l = b_bmi - 1.96 * b_bmi_sd,  b_bmi_u = b_bmi + 1.96 * b_bmi_sd,  
                o_bmi_l = o_bmi - 1.96 * o_bmi_sd,  o_bmi_u = o_bmi + 1.96 * o_bmi_sd,
                a2_bmi_l = a2_bmi - 1.96 * a2_bmi_sd,  a2_bmi_u = a2_bmi + 1.96 * a2_bmi_sd,
                a5_bmi_l = a5_bmi - 1.96 * a5_bmi_sd,  a5_bmi_u = a5_bmi + 1.96 * a5_bmi_sd  )

    ########################################    select variables  
    
d =    dt %>% filter(!is.na(o_preop_vektskole), o_opmetode %in% c(1, 6)) %>%
      select(p_pasientid, ForlopsID, o_sykehus,  o_dato_op, p_alder_v_op, Sex, Female, 
             o_preop_vektskole, o_preop_vektprog, o_opmetode, smoke, work,
             b_finans,  u6_ferdigstill, u6_oppf_type, a5_oppf_type,
             contains("bmi_"),  contains("b_beh"), vent, vent_a5, ligg, ligg_a5, alv_kmp, alv_kmp_a5, subst, subst_a5,
             reinn, reinn_a5, depr, depr_a5, vtap, vtap_a5, dBMI, dBMI_a5, a5_fu, 
            o_dato_op, a5_dato_oppf,  trt, a5_ferdigstill, bmi_5a,  a5_TWL) %>% 
      mutate(bmi_0 = bmi_baseline, bmi_0o = bmi_op, bmi_0u6 = bmi_6v) %>%
      select(-bmi_baseline, -bmi_op, -bmi_6v)
    
d_elig = d %>% filter(o_dato_op <  Sys.Date() - years(5) - months(6))      

d_act_d30 = d_elig %>% filter(u6_ferdigstill == 1)

d_act = d %>% filter(o_dato_op <  Sys.Date() - years(5) - months(6), 
                      a5_ferdigstill == 1,  
                      !is.na( bmi_5a))  #     2023-10-09: 3731      
                                                                           # 3 pas have vekt =0 ??
d_act_nt6  = d_act %>% filter(a5_dato_oppf - o_dato_op < 2007, a5_dato_oppf - o_dato_op > 1642)
d_act_nt12 = d_act %>% filter(a5_dato_oppf - o_dato_op < 2190,  a5_dato_oppf - o_dato_op > 1460)


d_a5_fu = d_elig %>% filter(!is.na(bmi_5a), bmi_5a > 10)                 #                 2679 
d_a5_no = d_elig %>% filter(is.na(bmi_5a))                               #                 1049


 

d_elig %>% group_by(o_preop_vektskole, o_opmetode) %>%
  summarise(N= n(),age =  mean(p_alder_v_op, na.rm = TRUE), Female= mean(Sex=="F", na.rm = TRUE),  BMI = mean(bmi_0, na.rm = TRUE),
            Db =  mean(b_beh_diab, na.rm = TRUE), 
            Dl =   mean(b_beh_dyslip, na.rm = TRUE)  , 
            Rx =   mean(b_beh_dyspepsi, na.rm = TRUE) , 
            Ht =    mean(b_beh_hypert, na.rm = TRUE) , 
            Dp = mean(b_beh_depr, na.rm = TRUE)  , 
            Ms =   mean(b_beh_musk_skjsm, na.rm = TRUE) )

MN <- function(v){mean(v, na.rm=TRUE)}  # +purrr  map
ST <- function(v){tibble(n()/nrowmean(v, na.rm=TRUE),sd(v, na.rm = TRUE))}  # +purrr  map

# 
# 
# 
# tbl_summary(d, by = o_opmetode) %>% 
#   add_p() %>% 
# #  add_q() %>% 
#   add_overall() %>% 
#   add_n() %>% 
#  # add_ci() %>% 
#   add_stat_label()
# 
# d %>%
#   tbl_summary(
#     by        = o_opmetode,
#     statistic = gtsummary::all_continuous()  ~ "{mean} ({sd})") %>%
# #  add_p(test  = list(
# #    gtsummary::all_continuous()  ~ "t.test", 
# #    gtsummary::all_categorical() ~ "fisher.test")) %>%
#   add_n() %>%
#   add_overall()

# res = d %>% mutate(trt =   case_when(o_preop_vektskole==1 & o_opmetode ==1 ~ "RYGB school",
#                                        o_preop_vektskole==0 & o_opmetode ==1 ~ "RYGB norm",
#                                        o_preop_vektskole==1 & o_opmetode ==6 ~ "GS school",
#                                        o_preop_vektskole==0 & o_opmetode ==6 ~ "GS norm"))

tGS = d_act_nt6   %>%
  filter(o_opmetode == 6) %>%
  select(p_alder_v_op, Female, bmi_0, b_beh_diab, b_beh_hypert, b_beh_dyslip, b_beh_dyspepsi,
         b_beh_hypert, b_beh_musk_skjsm, b_beh_depr, b_beh_sovnap, smoke, work, trt) %>%
  tbl_summary(
    by        = trt,
    label = list(p_alder_v_op ~ "Age", bmi_0 ~ "BMI", smoke ~ "Smoking", 
                 work ~ "Working" , b_beh_musk_skjsm ~ "Muskular-sceletal pain",
                 b_beh_diab ~ "Diabetes", b_beh_hypert ~ "Hypertension", 
                 b_beh_dyslip ~ "Dyslipidemi", b_beh_dyspepsi ~ "GERD", b_beh_sovnap ~ "Sleep apnoea",  
                 b_beh_depr ~ "Depression"), 
    statistic = gtsummary::all_continuous()  ~ "{mean} ({sd})",
    digits = list(p_alder_v_op ~ c(1, 1)) ) %>%
  add_p(test  = list(
    gtsummary::all_continuous()  ~ "t.test", 
    gtsummary::all_categorical() ~ "fisher.test") ) %>%
  modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                text_interpret ="md")  
  
tGB =  d_act_nt6   %>%
    filter(o_opmetode == 1) %>%
  select(p_alder_v_op, Female, bmi_0, b_beh_diab, b_beh_hypert, b_beh_dyslip, b_beh_dyspepsi,
         b_beh_hypert, b_beh_musk_skjsm, b_beh_depr, b_beh_sovnap, smoke, work, trt) %>%
  tbl_summary(
      by        = trt,
      label = list(p_alder_v_op ~ "Age", bmi_0 ~ "BMI", smoke ~ "Smoking", 
                   work ~ "Working" , b_beh_musk_skjsm ~ "Muskular-sceletal pain",
                   b_beh_diab ~ "Diabetes", b_beh_hypert ~ "Hypertension", 
                   b_beh_dyslip ~ "Dyslipidemi", b_beh_dyspepsi ~ "GERD", b_beh_sovnap ~ "Sleep apnoea",  
                   b_beh_depr ~ "Depression"), 
      statistic = gtsummary::all_continuous()  ~ "{mean} ({sd})",
      digits = list(p_alder_v_op ~ c(1, 1)) ) %>%
    add_p(test  = list(
      gtsummary::all_continuous()  ~ "t.test", 
      gtsummary::all_categorical() ~ "fisher.test") )  %>%
  modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                text_interpret ="md")  

#--------------------  tables 2
#    statistic = gtsummary::all_continuous()  ~ "{mean} ({sd})",
#   digits = list(p_alder_v_op ~ c(1, 1)),
# digits = list(ligg ~ c(2,3,4))  

cnt <- function(trt, ...) {
  tal = d_elig %>% group_by(trt) %>% summarise(n())
  names(tal) = c("treat", "n_beh")
  tal %>% filter(treat == trt) %>% pull(n_beh) %>% as_tibble()
  }

my_cnt <- function(data, ...) {
  a5_fl  <- sum(data$a5_fu, na.rm = TRUE)
  dplyr::tibble(   a5_flw = a5_fl )
}

tGS2_elig = d_elig %>% mutate(a5_no = !a5_fu) %>%
  filter(o_opmetode == 6) %>%
  select(trt, a5_no ) %>% 
  tbl_summary(
    by = trt,
    statistic =  list(all_categorical() ~ "{n} / {N} ({p}%)" ) ,
     label = list(a5_no ~ "not Follow-up 5 yrs"),
    missing_text = "Missing data"  ) %>%  
  add_p() %>%
   modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                text_interpret ="md")

tGS2_ny = d_elig %>%  
  filter(o_opmetode == 6) %>%
  select(trt ) %>% 
  tbl_summary(
    by = trt ) %>% 
  modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                text_interpret ="md")

nevn = d_elig %>% pull(trt) %>% table

tGB2_elig = d_elig %>% mutate(a5_no = !a5_fu) %>%
  filter(o_opmetode == 1) %>%
  select(trt, a5_no ) %>% 
  tbl_summary(
    by = trt,
    statistic =  all_categorical() ~ "{n} / {N} ({p}%)",
    label = list(a5_no ~ "not Follow-up 5 yrs") ,
    missing_text = "Missing data" ) %>%   add_p() %>%
  modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                text_interpret ="md")

# 
# tb_GS = d_elig %>% filter(o_opmetode == 6) %>% select(trt, o_preop_vektskole) %>%  tbl_summary(by=trt)  
# 
# tb_GB = d_elig %>% filter(o_opmetode == 1) %>% select(trt, o_preop_vektskole) %>%  tbl_summary(by=trt)  
# 
# ###############################################  final table
# tA = tbl_stack(list(tb_GS, tGS2_act))
# tB = tbl_stack(list(tb_GB, tGB2_act))
# 
# tbl_merge(list(tA,tB), tab_spanner = c("**Gastric Sleeve**", "**Gastric Bypass**" ) )  %>%
#   modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
#                 text_interpret ="md") %>% 
#   as_gt() %>%  gt::gtsave("tabell2.docx")
# 
# # %>% as_flex_table()  
# tbl_merge(list(tA,tB), tab_spanner = c("**Gastric Sleeve**", "**Gastric Bypass**" ) )  %>%
#   modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
#                 text_interpret ="md") %>%
#   remove_row_type(variables = o_preop_vektskole, type =  c("all"), level_value = NULL) %>% 
#   as_gt() %>%  gt::gtsave("tabell2.docx")
# 
# tb_N = d_elig %>% filter(o_opmetode %in% c(1,6)) %>% select(trt, o_preop_vektskole) %>%  
#   tbl_summary(by = trt)
# 


tbl_stack(  list(tb_N, tbl_merge(  list(tGS2_act, tGB2_act) ))) %>% 
  remove_row_type(o_preop_vektskole, type = "header")
  
  tbl_summary(by =   trt,
              label = trt ~ "behandling",
              statistic = list(all_categorical() ~ "{n}"))

tGS2_act = d_act_nt6 %>%
  filter(o_opmetode == 6) %>%
  select(trt, a5_fu,    vent_a5,   ligg_a5,   reinn_a5,
           alv_kmp_a5,  subst_a5,   depr_a5,   vtap_a5,   dBMI_a5) %>% 
  tbl_summary(
    by = trt,
    statistic = list( a5_fu ~"{n}", 
                       reinn_a5  ~ "{n} / {N} ({p}%)"  ,
                      alv_kmp_a5~ "{n} / {N} ({p}%)"  ,
                        subst_a5~ "{n} / {N} ({p}%)"  ,
                        depr_a5~ "{n} / {N} ({p}%)" ),
 #   stat_fns = a5_fu ~my_cnt,
#    statistic = a5_fu ~ "{n}/ {a5_flw}  ",
 #   type = list(a5_fu ~ 'continuous'),
    label = list(a5_fu ~"Follow-up 5 yrs",
                 vent_a5 ~"Waiting time ", 
                 ligg_a5 ~"Postoperative days in hospital ", 
                 reinn_a5 ~"Readmission ", 
                 alv_kmp_a5 ~"Severe complication (30d) ", 
                 subst_a5 ~"Substitution ", 
                 depr_a5 ~"Depression ", 
                 vtap_a5 ~"%TWL ", 
                 dBMI_a5  ~"d BMI "),
missing_text = "Missing data" ) %>%  
  add_p(test  = list(
    gtsummary::all_continuous()  ~ "t.test", 
    gtsummary::all_categorical() ~ "fisher.test") ) %>%
  modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                text_interpret ="md")


tbl_stack( list(nevn[1:2],  tGS2_act) )

tbl_stack( list(tGS2_act, tGS2_elig )  )

tbl_merge(  list(
tbl_stack( list(tb_GS,  tGS2_act) ),
tbl_stack( list(tb_GB, tGB2_act)  )
))

tGB2_act = d_act_nt6 %>%
  filter(o_opmetode == 1) %>%
  select(trt, a5_fu,    vent_a5,   ligg_a5,   reinn_a5,
         alv_kmp_a5,  subst_a5,   depr_a5,   vtap_a5,   dBMI_a5) %>% 
  tbl_summary(
    by = trt,
    statistic = list( a5_fu ~"{n}", 
                      reinn_a5  ~ "{n} / {N} ({p}%)"  ,
                      alv_kmp_a5~ "{n} / {N} ({p}%)"  ,
                      subst_a5~ "{n} / {N} ({p}%)"  ,
                      depr_a5~ "{n} / {N} ({p}%)" ),
    #   stat_fns = a5_fu ~my_cnt,
    #    statistic = a5_fu ~ "{n}/ {a5_flw}  ",
    #   type = list(a5_fu ~ 'continuous'),
    label = list(a5_fu ~"Follow-up 5 yrs",
                 vent_a5 ~"Waiting time ", 
                 ligg_a5 ~"Postoperative days in hospital ", 
                 reinn_a5 ~"Readmission ", 
                 alv_kmp_a5 ~"Severe complication (30d) ", 
                 subst_a5 ~"Substitution ", 
                 depr_a5 ~"Depression ", 
                 vtap_a5 ~"%TWL ", 
                 dBMI_a5  ~"d BMI "),
    missing_text = "Missing data" ) %>%  
  add_p(test  = list(
    gtsummary::all_continuous()  ~ "t.test", 
    gtsummary::all_categorical() ~ "fisher.test") ) %>%
  modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                text_interpret ="md")



tGS2_proper = d_elig %>%
  filter(o_opmetode == 6) %>%
  select(trt, a5_fu,    vent_a5,   ligg_a5,   reinn_a5,
         alv_kmp_a5,  subst_a5,   depr_a5,   vtap_a5,   dBMI_a5) %>% 
  tbl_summary(
    by = trt,
       #  stat_fns = a5_fu ~ my_cnt,
        statistic = list(  # a5_fu ~ "{n} / { a5_flw }  ",
                          all_categorical() ~ "{n} / {N} ({p}%)"  ),
      # type = list(a5_fu ~ 'continuous'),
    label = list(a5_fu ~ "Follow-up 5 yrs",
                 vent_a5 ~ "Waiting time ", 
                 ligg_a5 ~ "Postoperative days in hospital ", 
                 reinn_a5 ~ "Readmission ", 
                 alv_kmp_a5 ~ "Severe complication (30d) ", 
                 subst_a5 ~ "Substitution ", 
                 depr_a5 ~ "Depression ", 
                 vtap_a5 ~ "%TWL ", 
                 dBMI_a5  ~"d BMI ")   ) %>%  
  add_p(test  = list(
    gtsummary::all_continuous()  ~ "t.test", 
    gtsummary::all_categorical() ~ "fisher.test") ) %>%
  modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                text_interpret ="md")



tGB2 = d_act %>%
  filter(o_opmetode == 1) %>%
  select(trt, a5_fu, 
         vent, vent_a5, ligg, ligg_a5, reinn, reinn_a5,
         alv_kmp, alv_kmp_a5, subst, subst_a5, depr, depr_a5, vtap, vtap_a5, dBMI, dBMI_a5) %>% 
  tbl_summary(
    by = trt,
  #  stat_fns = ~cnt,
    label = list(a5_fu ~"Follow-up 5 yrs",
                 vent_a5 ~"Waiting time ", 
                 ligg_a5 ~"Postoperative days in hospital ", 
                 reinn_a5 ~"Readmission ", 
                 alv_kmp_a5 ~"Severe complication (30d) ", 
                 subst_a5 ~"Substitution ", 
                 depr_a5 ~"Depression ", 
                 vtap_a5 ~"%TWL ", 
                 dBMI_a5  ~"d BMI "),
    statistic = a5_fu ~ "{n}/ {N} ({p}%) ") %>%  
  add_p(test  = list(
    gtsummary::all_continuous()  ~ "t.test", 
    gtsummary::all_categorical() ~ "fisher.test") ) %>%
  modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                text_interpret ="md")

T2 =  tbl_merge(tbls =  list( tGS2, tGB2),
                tab_spanner = c("Gastric Sleeve", 
                                "Gastric Bypass" ) )  %>% 
  as_flex_table()  

   
 
te = d_elig %>% 
  select(p_alder_v_op, Female, bmi_0, b_beh_diab, b_beh_hypert, b_beh_dyslip, b_beh_dyspepsi,
         b_beh_hypert, b_beh_musk_skjsm, b_beh_depr, b_beh_sovnap, smoke, work ) %>%
  tbl_summary(
    label = list(p_alder_v_op ~ "Age", bmi_0 ~ "BMI", smoke ~ "Smoking", 
                 work ~ "Working" , b_beh_musk_skjsm ~ "Muskular-sceletal pain",
                 b_beh_diab ~ "Diabetes", b_beh_hypert ~ "Hypertension", 
                 b_beh_dyslip ~ "Dyslipidemi", b_beh_dyspepsi ~ "GERD", b_beh_sovnap ~ "Sleep apnoea",  
                 b_beh_depr ~ "Depression"), 
 #   statistic = gtsummary::all_continuous()  ~ "{mean} ({sd})",
    digits = list(p_alder_v_op ~ c(1, 1)) ) 
 
tfu= d_a5_fu %>% 
  select(p_alder_v_op, Female, bmi_0, b_beh_diab, b_beh_hypert, b_beh_dyslip, b_beh_dyspepsi,
         b_beh_hypert, b_beh_musk_skjsm, b_beh_depr, b_beh_sovnap, smoke, work ) %>%
  tbl_summary(
    label = list(p_alder_v_op ~ "Age", bmi_0 ~ "BMI", smoke ~ "Smoking", 
                 work ~ "Working" , b_beh_musk_skjsm ~ "Muskular-sceletal pain",
                 b_beh_diab ~ "Diabetes", b_beh_hypert ~ "Hypertension", 
                 b_beh_dyslip ~ "Dyslipidemi", b_beh_dyspepsi ~ "GERD", b_beh_sovnap ~ "Sleep apnoea",  
                 b_beh_depr ~ "Depression"), 
    statistic = gtsummary::all_continuous()  ~ "{mean} ({sd})",
    digits = list(p_alder_v_op ~ c(1, 1)) )  

#########
set_flextable_defaults(table.layout = "autofit", font.size = 8)
# get_flextable_defaults()          # sjekk

  # tbl_merge(tbls =  list(te, tfu, tGS, tGB),
  #   tab_spanner = c("Eligible for  \n 5 year follow-up", "Actual   \n 5 year follow-up", "Gastric Sleeve", "Gastric Bypass") )  %>% 
  #     as_flex_table() %>%  
  #   flextable::save_as_docx(path = paste0(anlzmappe,"tabell7.docx"), 
  #                          pr_section =  prop_section(
  #                           page_size = page_size(orient = "landscape" )   ))
 # relocate("norm GB", .before = "school GB") 
# add_n() %>%

T1 =  tbl_merge(tbls =  list(te, tfu, tGS, tGB),
               tab_spanner = c("Eligible for  \n 5 year follow-up",
                               "Actual   \n 5 year follow-up", 
                               "Gastric Sleeve", 
                               "Gastric Bypass") )  %>% 
            as_flex_table()  

  
  
# tbl_merge(tbls =  list(te, tfu, tGS, tGB),
#            tab_spanner = c("Eligible for  \n 5 year follow-up", "Actual   \n 5 year follow-up", "Gastric Sleeve", "Gastric Bypass") )  %>%
#    as_gt() %>%  # opt_footnote_marks(marks = "letters") %>%
#    tab_options(footnotes.marks = "letters") %>%
#    gt::gtsave("table_1.docx")
##  Error: pandoc document conversion failed with error 22
 
 
# d %>% filter(trt == "GS norm") %>% summarise(N= n(),age =  mean(p_alder_v_op, na.rm = TRUE), Female= mean(Sex=="F", na.rm = TRUE),  BMI = mean(bmi_0, na.rm = TRUE),
#                       Db =  mean(b_beh_diab, na.rm = TRUE), 
#                               Dl =   mean(b_beh_dyslip, na.rm = TRUE)  , 
#                               Rx =   mean(b_beh_dyspepsi, na.rm = TRUE) , 
#                               Ht =    mean(b_beh_hypert, na.rm = TRUE) , 
#                               Dp = mean(b_beh_depr, na.rm = TRUE)  , 
#                               Ms =   mean(b_beh_musk_skjsm, na.rm = TRUE) )

dl_elig  = d_elig %>%  pivot_longer(cols = contains("bmi"), names_to = "time", values_to = "value")
dl_a5_fu = d_a5_fu %>%  pivot_longer(cols = contains("bmi"), names_to = "time", values_to = "value")

# 
#####################################     graph ------------------------------

 dl =    d %>% pivot_longer(cols = contains("bmi"), names_to = "time", values_to = "value")   

 dl_SG = d %>% filter(o_opmetode == 6) %>% pivot_longer(cols = contains("bmi"), names_to = "time", values_to = "value")  
 dl_GB = d %>% filter(o_opmetode == 1) %>% pivot_longer(cols = contains("bmi"), names_to = "time", values_to = "value")  
 
 dl_0 = dl %>% filter(o_preop_vektskole == 0)
 dl_1 = dl %>% filter(o_preop_vektskole == 1)
 
 ggplot(dl, aes(time, value)) +
   geom_violin() +
   geom_boxplot(width = 0.1, outlier.colour = "blue") +
   theme_classic()
    # mutate(bmi_b_sd = sd(bmi_baseline, na.rm = T),
    #        bmi_op_sd = sd(bmi_op, na.rm = T),
    #        bmi_1a_sd = sd(bmi_1a, na.rm = T),
    #        bmi_2a_sd = sd(bmi_2a, na.rm = T),
    #        bmi_5a_sd = sd(bmi_5a, na.rm = T),) %>%
      
 ggplot(data = dl, aes(x=time, y=value, group = o_preop_vektskole, fill = o_preop_vektskole)) +
   geom_bar(position="dodge", stat = "identity" ) +
   geom_jitter()
 
 dl %>% ggplot(aes(x=time, y=value, fill=factor(o_preop_vektskole)))+
   geom_boxplot(notch = TRUE, outlier.colour = "red", outlier.shape = 1) +
   labs(fill ="skole") +
   geom_point(position = position_jitterdodge(), size= 0.2, alpha=0.3)+
   scale_fill_manual(values = c("lightyellow","lightblue")) +
   theme_bw(base_size = 16)
 
 dl_SG %>% ggplot(aes(x=time, y=value, fill=factor(o_preop_vektskole)))+
   geom_boxplot(notch = TRUE, outlier.colour = "red", outlier.shape = 1) +
   labs(fill ="skole") +
   geom_point(position = position_jitterdodge(), size= 0.2, alpha=0.3)+
   scale_fill_manual(values = c("lightyellow","blue")) +
   theme_bw(base_size = 16)

  dl_GB %>% ggplot(aes(x=time, y=value, fill=factor(o_preop_vektskole)))+
   geom_boxplot(notch = FALSE, outlier.colour = "red", outlier.shape = 1) +
   labs(fill ="skole") +
   geom_point(position = position_jitterdodge(), size= 0.2, alpha=0.3)+
   scale_fill_manual(values = c("lightyellow","green")) +
   theme_bw(base_size = 16)
  
  
  dl %>% ggplot(aes(x=time, y=value, fill=factor(o_preop_vektskole)))+
    geom_violin(show.legend = FALSE, draw_quantiles = c(0.05,0.5,0.95)) +
 #   stat_summary_bin(fun = mean, geom = "pointrange", colour = "red", width = 0.3) +
    geom_dotplot(binaxis = "y", binwidth = 0.1, stackdir = "center", position = "dodge") +
    labs(fill ="skole") +
    scale_fill_manual(values = c("lightyellow","lightblue")) +
    theme_bw(base_size = 16)

# library(see) 
#   dl %>% ggplot(aes(x=time, y=value, fill=factor(o_preop_vektskole)))+
#     geom_violinhalf(show.legend = FALSE, draw_quantiles = c(0.05,0.5,0.95))+
#     geom_boxplot(width= 0.2, notch = TRUE, outlier.colour = "red", outlier.shape = 1) +
#     labs(fill ="skole") +
#   #  geom_point(position = position_jitterdodge(), size= 0.2, alpha=0.3)+
#     scale_fill_manual(values = c("lightyellow","lightblue")) +
#     theme_modern(base_size = 16)
  
#  , color= c("lightyellow","lightblue")
  theme_set(theme_ggdist())
  
plt_1 =  dl %>% ggplot(aes(x=time, y=value, 
                    fill=factor(o_preop_vektskole)))+
    labs(fill ="skole") +  
     stat_dotsinterval(data=dl_0, side = "bottom",scale=0.5, quantiles = 600, point_interval = mode_hdci  ) +
    stat_dotsinterval(data=dl_1,  side = "top", scale=0.5, quantiles = 600, point_interval = mode_hdci  ) +
    geom_boxplot(notch = TRUE, width=0.3, outlier.colour = "red", outlier.shape = 1) +
       #  geom_point(position = position_jitterdodge(), size= 0.2, alpha=0.3)+
    scale_fill_manual(values = c("lightyellow",  "lightgreen")) +
    scale_y_continuous(limits=c(15,60))+
    theme_bw(base_size = 16)
  plt_1
#####################################     communicate ------------------------
    #
    #####   results: tables  
    #
  dt = dt %>% filter(!is.na(opr))
t1  = table1(~Sex + p_alder_v_op + b_ant_vekt + b_ant_vekt + b_ant_kmi + b_ant_midjemal + b_beh_diab | opr, data = dt)
table1(~opr + p_alder_v_op +  b_ant_kmi+ b_beh_diab | Sex, data = dt)

t2  = table1(~Sex + p_alder_v_op + b_ant_vekt + b_ant_vekt + b_ant_kmi +  
               b_beh_diab + b_beh_hypert + b_beh_dyslip + b_beh_sovnap + b_beh_dyspepsi+ b_beh_depr | opr, data = dt)
    #
    #####   results: graphs    

    # esquisse::
 