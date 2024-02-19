library(lme4)
# library(lmerTest)
# library(conflicted)
# conflicts_prefer(dplyr::filter())
# conflicts_prefer(dplyr::select())

N_op_d30  <- d_elig_d30 |>  group_by(trt) |> summarise(N_opr = n())  
N_op_a5  <- d_elig |>  group_by(trt) |> summarise(N_opr = n())  

cnt_d30 = d_elig_d30 %>% tbl_summary(by = trt, 
                                        include = c(u6_fu), 
                                        label = u6_fu ~ "Actual 30d follow-up") 

cnt_a5 = d_elig %>% tbl_summary(by = trt, 
                                include = c(a5_nt), 
                                label = a5_nt ~ "Actual 5 yr follow-up") 

#     Construct the upper part of T3 -------------------------------------------
up_d30 = function(tb) { tb |> 
    select(trt,  vent, vtap_30,   ligg_mn4, reinn, alv_kmp) |>  
    tbl_summary( 
      by = trt,
      type = list( vent ~  "continuous",
                   vtap_30 ~ "continuous",  # 
                  # vt_pr ~ "continuous",
                   ligg_mn4 ~ "dichotomous",
                   reinn   ~ "dichotomous",
                   alv_kmp ~ "dichotomous"  ),
      statistic = list( vent ~ "{median} ({p25}, {p75})",
                        vtap_30 ~ "{mean} ({sd})",
                       # vt_pr ~ "{mean} ({sd})",
                        ligg_mn4 ~ "{n} / {N} ({p}%)",
                        reinn ~ "{n} / {N} ({p}%)",
                        alv_kmp ~ "{n} / {N} ({p}%)"),      #  digits = list(ligg ~ 2), 
      digits = all_continuous() ~ 1,
      label = list( vent ~ "Waiting time (d)",
                    vtap_30 ~ "%TWL preop",
                   # vt_pr ~ "Pre-operative BMI loss (kg/m^2)",
                    ligg_mn4 ~ "Hospital stay > 3 days",
                    reinn ~ "Readmission",
                    alv_kmp ~ "Severe complications (30 d)"),
      missing_text = "Missing data" 
    )  }

 
#     Construct the lower part of T3 -------------------------------------------
dw_a5 = function(tb) {tb |> 
  select(trt, vtap,  subst) |>  # dBMI
  tbl_summary( 
  by = trt,
type = list(vtap ~ "continuous", 
  #  dBMI ~ "continuous" , 
    subst ~ "dichotomous"),    
statistic = list( 
  vtap ~ "{mean} ({sd})",
#  dBMI ~ "{mean} ({sd})",
  subst~ "{n} / {N} ({p}%)"),
digits = all_continuous() ~ 1,
label = list( vtap ~ "%TWL", 
#  dBMI  ~ "Five year BMI loss (kg/m^2)",
  subst ~ "Substitution"),
missing_text = "Missing data")  
}
 


tbl3 = tbl_stack(list(cnt_d30, up_d30(d_elig_d30 |> filter(u6_fu)), 
                      cnt_a5, dw_a5(d_elig |> filter(a5_nt))))

T3 = tbl3 |>  as_gt() |>  
  rows_add( .list = rlang::list2("label" =  "Eligible for 30 d follow-up",
                                                    "stat_1" = as.character(N_op_d30$N_opr[1]),
                                                    "stat_2" = as.character(N_op_d30$N_opr[2])),
                               .before = 1 ) |> 
  rows_add( .list = rlang::list2("label" =  "Eligible for 5 yrs follow-up",
                                 "stat_1" = as.character(N_op_a5$N_opr[1]),
                                 "stat_2" = as.character(N_op_a5$N_opr[2])),
            .before = 9 ) |> 
  rows_add( .n_empty = 1, .before = 9)


#  T3 |> opt_footnote_marks(marks = "letters") %>% gtsave("T3_wo_just_diff.docx")

# ---- interaction EPEP -- opmetode
m_opm =  lm( formula = a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0 + o_preop_vektskole*o_opmetode +  b_beh_diab + smoke  , data = filter(d_elig, a5_nt))
summary(m_opm)

p_opm = summary(m_opm)$coefficients["o_preop_vektskole:o_opmetode", "Pr(>|t|)"]
p_opm = signif(p_opm, 2)

m_vtpr =  lm( formula = a5_TWL ~ vt_pr * o_preop_vektskole + p_alder_v_op + Female + bmi_0  + o_opmetode +  b_beh_diab + smoke  , data = filter(d_elig, a5_nt))
summary(m_vtpr)

p_vtpr = summary(m_vtpr)$coefficients["vt_pr:o_preop_vektskole", "Pr(>|t|)"]
p_vtpr = signif(p_vtpr, 2)
# ---------------------- 5 yr linear model ------------------------------------  LME Models -----------------------------
m_00   <- lm(formula = a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0   + 
                o_preop_vektskole + b_beh_diab + smoke  ,  
              data = d_elig |> filter(a5_nt))
summary(m_00)

# 
m_0   <- lmer(formula = a5_TWL ~   p_alder_v_op + Female + bmi_0   + o_opmetode +
                        o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  
               data = d_elig |> filter(a5_nt))  # d_act_a5 has NOT -5.5år -filter
summary(m_0)
REff  = ranef(m_0)
RE_tbbl = as_tibble(REff)
library(lattice)
dotplot( ranef(m_0))

# 2024-02-16
#
m_lm   <- lm(formula = a5_TWL ~   p_alder_v_op + Female + bmi_0   + o_opmetode +
                o_preop_vektskole + b_beh_diab + smoke  ,  
              data = d_elig |> filter(a5_nt))  # d_act_a5 has NOT -5.5år -filter
summary(m_lm)

##

library(CIplot)

Follow_up_a5 = glm( formula = a5_nt  ~ 
   p_alder_v_op + Female + bmi_0   + o_opmetode + o_preop_vektskole + b_beh_diab + smoke, 
   data =  d_elig,  
   family = binomial)

# For odds ratio
exp(Follow_up_a5$coefficients)
 
Subst = glm( formula = subst  ~ 
                      p_alder_v_op + Female + bmi_0   + o_opmetode + o_preop_vektskole + b_beh_diab + smoke, 
                    data =  d_elig,  
                    family = binomial)

# For odds ratio
exp(Subst$coefficients)


ORci(Follow_up_a5, conf.level = 0.95)
ORci(Subst, conf.level = 0.95)
# -----------------------------------------------------------------------------

plot( m_0) # residuals

# df |> group_by(o_sykehus) |> summarise(n()) |> print(n=22)
library(lattice)
dotplot(ranef(m_0))

#  30d models
m_u6  <- lmer(formula = TWL_pr ~ vt_pr + p_alder_v_op + Female + bmi_0   + 
                o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  
              data = d_act_d30)
summary(m_u6)
REff_u6  = ranef(m_u6)
RE_tbbl_u6 = as_tibble(REff_u6)
plot(m_u6)


m_u6b  <- lmer(formula = TWL_pr ~ vt_pr + p_alder_v_op + Female + bmi_0   + 
                o_preop_vektskole +(1|o_sykehus),  
              data = d_act_d30)
summary(m_u6b)
REff_u6b  = ranef(m_u6b)
RE_tbbl_u6 = as_tibble(REff_u6b)
plot(m_u6b)

m_u6c  <- lmer(formula = TWL_pr ~ vt_pr + p_alder_v_op + bmi_0 + o_preop_vektskole +(1|o_sykehus),  
               data = d_act_d30)
summary(m_u6c)
REff_u6c  = ranef(m_u6c)
RE_tbbl_u6 = as_tibble(REff_u6c)

plot(m_u6c)
### ---------------------------------
m_u6d  <- lmer(formula = TWL_pr ~   p_alder_v_op + bmi_0 + o_preop_vektskole +(1|o_sykehus),  
               data = d_act_d30)
summary(m_u6d)
REff_u6d  = ranef(m_u6d)
RE_tbbl_u6 = as_tibble(REff_u6d)

plot(m_u6d)

m_u6e  <- lmer(formula = TWL_pr ~  p_alder_v_op + Female + bmi_0   + 
                 o_preop_vektskole   + smoke +(1|o_sykehus),  
               data = d_act_d30)
summary(m_u6e)
REff_u6e  = ranef(m_u6e)
RE_tbbl_u6 = as_tibble(REff_u6e)

plot(m_u6e)


just_u6 = function(o_sykehus){
  RE_tbbl_u6 |> filter(grp == o_sykehus) |> pull(condval)
}


d_d30_j = d_elig_d30 |> filter(u6_fu) |> 
  mutate(u6_TWL_j =  TWL_pr - o_sykehus |> map( just_u6) |> unlist(),
         vtap_30 = u6_TWL_j)

#        ----------------------------- adjusting RE hospitals
just = function(o_sykehus){
  RE_tbbl |> filter(grp == o_sykehus) |> pull(condval)
}

d_a5_j = d_elig |> filter(a5_nt) |> 
  mutate(a5_TWL_j =  a5_TWL - o_sykehus |> map( just) |> unlist(),
         vtap = a5_TWL_j)

tbl3_j = tbl_stack(list(cnt_d30, up_d30(d_elig_d30 |> filter(u6_fu)), 
                        cnt_a5, dw_a5(d_a5_j )))

T3_j = tbl3_j |>  as_gt() |>  
  rows_add( .list = rlang::list2("label" =  "Eligible for 30 d follow-up",
                                 "stat_1" = as.character(N_op_d30$N_opr[1]),
                                 "stat_2" = as.character(N_op_d30$N_opr[2])),
            .before = 1 ) |> 
  rows_add( .list = rlang::list2("label" =  "Eligible for 5 yrs follow-up",
                                 "stat_1" = as.character(N_op_a5$N_opr[1]),
                                 "stat_2" = as.character(N_op_a5$N_opr[2])),
            .before = 12 ) |> 
  rows_add( .n_empty = 1, .before = 12)

# T3_j |> opt_footnote_marks(marks = "letters") %>% gtsave("T3_w_just_diff.docx")  # ------------------------------------------- 
  # 
  # rows_add( .list = rlang::list2("label" =  "Eligible for 5 yrs follow up",
  #                                "stat_1" = as.character(N_op_a5$N_opr[1]),
  #                                "stat_2" = as.character(N_op_a5$N_opr[2])),
  #           .before = 11 ) |> 
  # rows_add( .n_empty = 1, .before = 11)

# S1 = tbl_stack(list(cnt_d30, up_d30(d_elig_d30 |> filter(u6_fu))))
# S2 = tbl_stack( list(cnt_a5, dw_a5(d_elig |> filter(a5_nt ))))
# tbl3_uj =  tbl_stack(list(S1,S2)) 

tbl3_uj = tbl_stack(list(cnt_d30, up_d30(d_elig_d30 |> filter(u6_fu)), cnt_a5, dw_a5(d_elig |> filter(a5_nt )) ))

Tb3_uj = tbl3_uj |>  as_gt()  |>
  rows_add( .list = rlang::list2(
    "label" =  "Eligible for 5 yrs follow up",
    "stat_1" = as.character(N_op_a5$N_opr[1]),
    "stat_2" = as.character(N_op_a5$N_opr[2])),
    .before = 11 )  |>  
  rows_add( .n_empty = 1, .before = 11)
 

### Tb3_uj |> opt_footnote_marks(marks = "letters") %>% gtsave("T3_wo_just_diff.docx")

## ---- both d30 and a5 justert

tbl3_bj = tbl_stack(list(cnt_d30, up_d30(d_d30_j), cnt_a5, dw_a5(d_a5_j) ))

final_T3_bj = tbl3_bj |>  as_gt() |>  
  rows_add( .list = rlang::list2("label" =  "Eligible for 30 d follow-up",
                                 "stat_1" = as.character(N_op_d30$N_opr[1]),
                                 "stat_2" = as.character(N_op_d30$N_opr[2])),
            .before = 1 ) |> 
  rows_add( .list = rlang::list2("label" =  "Eligible for 5 yrs follow up",
                                 "stat_1" = as.character(N_op_a5$N_opr[1]),
                                 "stat_2" = as.character(N_op_a5$N_opr[2])),
            .before = 10 ) |> 
  rows_add( .n_empty = 1, .before = 10)

### final_T3_bj |>opt_stylize(style = 6) |> opt_footnote_marks(marks = "letters") %>% gtsave("T2_w_just_diff.docx")

library(rstatix)
n_elig_d30 = nrow(d_elig_d30)
n_elig = nrow(d_elig)

n_act_30 = nrow(d_act_d30)
n_act = nrow(d_act_a5)
 
n_GS_e30 = nrow(d_elig_d30_GS)
n_GS_ea5  =  nrow(d_elig_GS)
n_GB_e30 = nrow(d_elig_d30_GB)
n_GB_ea5  =   nrow(d_elig_GB)

n_tot_d30 = n_GS_e30 + n_GB_e30
n_tot_a5 = n_GS_ea5 + n_GB_ea5

n_GS_a30  = nrow(d_act_d30_GS)
n_GS_a30EP = sum(d_act_d30_GS$o_preop_vektskole)
n_GB_a30  = nrow(d_act_d30_GB)
n_GB_a30EP = sum(d_act_d30_GB$o_preop_vektskole)

n_GS_a5 = nrow(d_act_a5_GS)
n_GS_a5EP = sum(d_act_a5_GS$o_preop_vektskole)
n_GB_a5  = nrow(d_act_a5_GB)
n_GB_a5EP = sum(d_act_a5_GB$o_preop_vektskole)

n_EPEP =  table(d_elig_d30$o_preop_vektskole)[[2]]
n_SPEP =  table(d_elig_d30$o_preop_vektskole)[[1]]


# ligg_p  =  p_round( Tb3_uj$`_data`$p.value[8], digits = 2)
# readm_p =  p_round(  Tb3_uj$`_data`$p.value[9])
# compl_p =  p_round(  Tb3_uj$`_data`$p.value[10], digits = 2)
# fu5_p =    p_round(  Tb3_uj$`_data`$p.value[13])

n_EPEP_30 = n_GS_a30EP + n_GB_a30EP            # Tb3_uj$`_data`$stat_1[1]
n_SPEP_30 = n_GS_a30 + n_GB_a30 - n_EPEP_30    # Tb3_uj$`_data`$stat_2[1]

n_PEP  =  n_EPEP_30 + n_SPEP_30


p_EPEP_a5 = Tb3_uj$`_data`$stat_1[12]
p_SPEP_a5 =  Tb3_uj$`_data`$stat_2[12]
n_EPEP_a5 =  n_GS_a5EP + n_GB_a5EP  
n_SPEP_a5 =    n_act - n_EPEP_a5

TWL_EPEP_pr = Tb3_uj$`_data`$stat_1[4]
TWL_SPEP_pr = Tb3_uj$`_data`$stat_2[4]
dBMI_EPEP_pr = Tb3_uj$`_data`$stat_1[6]
dBMI_SPEP_pr = Tb3_uj$`_data`$stat_2[6]

TWL_EPEP_j_d30 =   final_T3_bj$`_data`$stat_1[5] %>% substring(1,3) 
TWL_SPEP_j_d30 =   final_T3_bj$`_data`$stat_2[5] %>% substring(1,3) 

TWL_EPEP_j_a5 = final_T3_bj$`_data`$stat_1[11]  %>% substring(1,4) 
TWL_SPEP_j_a5 = final_T3_bj$`_data`$stat_2[11]  %>% substring(1,4) 


TWL_EPEP =  Tb3_uj$`_data`$stat_1[14]
TWL_SPEP =  Tb3_uj$`_data`$stat_2[14]
dBMI_EPEP =  Tb3_uj$`_data`$stat_1[15]
dBMI_SPEP =  Tb3_uj$`_data`$stat_2[15]

# p_TWL_a5 = round(Tb3_uj$`_data`$p.value[14], 4)
 
TWL_GS = signif(mean(d_elig_GS$a5_TWL, na.rm = T), 3)
TWL_GB = signif(mean(d_elig_GB$a5_TWL, na.rm = T), 3)

# PEPjust_p = final_T3_bj$`_data`$p.value[11]

# p_opm

