library(lme4)
library(lmerTest)

N_op_a5  <- d_elig |>  group_by(trt) |> summarise(N_opr = n())  


cnt_d30 = d_elig_d30 %>% tbl_summary(by = trt, 
                                        include = c(u6_fu), 
                                        label = u6_fu ~ "Followed up, 30d") %>% add_p()

cnt_a5 = d_elig %>% tbl_summary(by = trt, 
                                include = c(a5_nt), 
                                label = a5_nt ~ "Follow up 5 yrs") %>% add_p()

#     Construct the upper part of T3 -------------------------------------------
up_d30 = function(tb) { tb |> 
    select(trt,  vent, TWL_pr, vt_pr, ligg_mn4, reinn, alv_kmp) |>  
    tbl_summary( 
      by = trt,
      type = list( vent ~  "continuous",
                   TWL_pr ~ "continuous",
                   vt_pr ~ "continuous",
                   ligg_mn4 ~ "dichotomous",
                   reinn   ~ "dichotomous",
                   alv_kmp ~ "dichotomous"  ),
      statistic = list( TWL_pr ~ "{mean} ({sd})",
                        vt_pr ~ "{mean} ({sd})",
                        ligg_mn4 ~ "{n} / {N} ({p}%)",
                        reinn ~ "{n} / {N} ({p}%)",
                        alv_kmp ~ "{n} / {N} ({p}%)"),      #  digits = list(ligg ~ 2), 
      label = list( vent ~ "Waiting time (d)",
                    TWL_pr ~ "%TWL preop",
                    vt_pr ~ "Pre-operative BMI loss (kg/m^2)",
                    ligg_mn4 ~ "Over 3 postoperative days in hospital",
                    reinn ~ "Readmission",
                    alv_kmp ~ "Severe complications (30 d)"),
      missing_text = "Missing data" 
    ) |>    add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))  }

#     Construct the lower part of T3 -------------------------------------------
dw_a5 = function(tb) { e_del = paste0("Delta BMI (kg/m^2)");
tb |> 
  select(trt, vtap, dBMI,   subst) |>  
  tbl_summary( 
    by = trt,
    type = list( c( subst) ~ "dichotomous"),    
    statistic = list( vtap ~ "{mean} ({sd})",
                      dBMI ~ "{mean} ({sd})",
                      subst~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    label = list(subst ~ "Substitution", 
                 vtap ~ "%TWL", 
                 dBMI  ~ e_del),
    #     missing = "no",  #  remove TWL BMI but ?keep substitution--sol: add later?
    missing_text = "Missing data" 
  ) |>
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))  
}

tbl3 = tbl_stack(list(cnt_d30, up_d30(d_elig_d30 |> filter(u6_fu)), 
                      cnt_a5, dw_a5(d_elig |> filter(a5_nt))))

tbl3 |>  as_gt() |>  
  rows_add( .list = rlang::list2("label" =  "Eligible for 5 yrs follow-up",
                                 "stat_1" = as.character(N_op_a5$N_opr[1]),
                                 "stat_2" = as.character(N_op_a5$N_opr[2])),
            .before = 13 ) |> 
  rows_add( .n_empty = 1, .before = 13)

# ------------------------------------  LME Models -----------------------------
#  5 yr model
m_0   <- lmer(formula = a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0   + 
                        o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  
               data = d_act_a5)
summary(m_0)
REff  = ranef(m_0)
RE_tbbl = as_tibble(REff)

#  30d models
m_u6  <- lmer(formula = TWL_pr ~ vt_pr + p_alder_v_op + Female + bmi_0   + 
                o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  
              data = d_act_d30)
summary(m_u6)
REff_u6  = ranef(m_u6)
RE_tbbl_u6 = as_tibble(REff_u6)

m_u6b  <- lmer(formula = TWL_pr ~ vt_pr + p_alder_v_op + Female + bmi_0   + 
                o_preop_vektskole +(1|o_sykehus),  
              data = d_act_d30)
summary(m_u6b)
REff_u6b  = ranef(m_u6b)
RE_tbbl_u6 = as_tibble(REff_u6b)

m_u6c  <- lmer(formula = TWL_pr ~ vt_pr + p_alder_v_op + bmi_0 + o_preop_vektskole +(1|o_sykehus),  
               data = d_act_d30)
summary(m_u6c)
REff_u6c  = ranef(m_u6c)
RE_tbbl_u6 = as_tibble(REff_u6c)

just_u6 = function(o_sykehus){
  RE_tbbl_u6 |> filter(grp == o_sykehus) |> pull(condval)
}


d_d30_j = d_elig_d30 |> filter(u6_fu) |> 
  mutate(u6_TWL_j =  TWL_pr - o_sykehus |> map( just_u6) |> unlist(),
         vtap = u6_TWL_j)

#        ----------------------------- adjusting RE hospitals
just = function(o_sykehus){
  RE_tbbl |> filter(grp == o_sykehus) |> pull(condval)
}


d_a5_j = d_elig |> filter(a5_nt) |> 
  filter(o_sykehus != "Vestre Viken HF") |> 
  mutate(a5_TWL_j =  a5_TWL - o_sykehus |> map( just) |> unlist(),
         vtap = a5_TWL_j)

tbl3_j = tbl_stack(list(cnt_d30, up_d30(d_elig_d30 |> filter(u6_fu)), 
                        cnt_a5, dw_a5(d_a5_j )))

T3_j = tbl3_j |>  as_gt() |>  
  rows_add( .list = rlang::list2("label" =  "Eligible for 5 yrs follow up",
                                 "stat_1" = as.character(N_op_a5$N_opr[1]),
                                 "stat_2" = as.character(N_op_a5$N_opr[2])),
            .before = 11 ) |> 
  rows_add( .n_empty = 1, .before = 11)

T3_j |> opt_footnote_marks(marks = "letters") %>% gtsave("T3c.docx")

## ---- both d30 and a5 justert

tbl3_bj = tbl_stack(list(cnt_d30, up_d30(d_d30_j), 
                        cnt_a5, dw_a5(d_a5_j )))

final_T3_bj = tbl3_bj |>  as_gt() |>  
  rows_add( .list = rlang::list2("label" =  "Eligible for 5 yrs follow up",
                                 "stat_1" = as.character(N_op_a5$N_opr[1]),
                                 "stat_2" = as.character(N_op_a5$N_opr[2])),
            .before = 11 ) |> 
  rows_add( .n_empty = 1, .before = 11)

