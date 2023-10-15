#
source("set_up.R")
#


d_elig_d30 |>
  filter(o_opmetode == 6) |>
  select(trt, b_finans, p_alder_v_op, Female, bmi_0o,               # BMI 3x?
        b_beh_musk_skjsm, b_beh_depr, b_beh_diab, b_beh_hypert, b_beh_dyslip, 
        b_beh_sovnap, b_beh_dyspepsi, smoke, work) |> tbl_summary(by = trt) |>
  add_overall()

d_act_d30 = d_elig_d30 %>% filter(u6_ferdigstill == 1)


t_e_d30 = d_elig_d30 %>% 
  select(p_alder_v_op, Female, bmi_0, b_beh_diab, b_beh_hypert, b_beh_dyslip, b_beh_dyspepsi,
         b_beh_hypert, b_beh_musk_skjsm, b_beh_depr, b_beh_sovnap, smoke, work ) %>%
  tbl_summary(
    label = list(p_alder_v_op ~ "Age", bmi_0 ~ "BMI", smoke ~ "Smoking", 
                 work ~ "Working" , b_beh_musk_skjsm ~ "Muskular-sceletal pain",
                 b_beh_diab ~ "Diabetes", b_beh_hypert ~ "Hypertension", 
                 b_beh_dyslip ~ "Dyslipidemi", b_beh_dyspepsi ~ "GERD", b_beh_sovnap ~ "Sleep apnoea",  
                 b_beh_depr ~ "Depression"), 
    statistic = gtsummary::all_continuous()  ~ "{mean} ({sd})",
    digits = list(p_alder_v_op ~ c(1, 1)) ) #  %>%
#  as_gt %>% opt_footnote_marks(marks = "letters") 

t_fu_d30 = d_act_d30 %>% 
  select(p_alder_v_op, Female, bmi_0, b_beh_diab, b_beh_hypert, b_beh_dyslip, b_beh_dyspepsi,
         b_beh_hypert, b_beh_musk_skjsm, b_beh_depr, b_beh_sovnap, smoke, work ) %>%
  tbl_summary(
    label = list(p_alder_v_op ~ "Age", bmi_0 ~ "BMI", smoke ~ "Smoking", 
                 work ~ "Working" , b_beh_musk_skjsm ~ "Muskular-sceletal pain",
                 b_beh_diab ~ "Diabetes", b_beh_hypert ~ "Hypertension", 
                 b_beh_dyslip ~ "Dyslipidemi", b_beh_dyspepsi ~ "GERD", b_beh_sovnap ~ "Sleep apnoea",  
                 b_beh_depr ~ "Depression"), 
    statistic = gtsummary::all_continuous()  ~ "{mean} ({sd})",
    digits = list(p_alder_v_op ~ c(1, 1)) ) #  %>%

tGS_d30 = d_act_d30   %>%
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

tGB_d30 =  d_act_d30   %>%
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


T1 =  tbl_merge(tbls =  list(t_e_d30, t_fu_d30, tGS_d30, tGB_d30),
                tab_spanner = c("Eligible for  \n 30 d follow-up",
                                "Actual   \n 30 d follow-up", 
                                "Gastric Sleeve", 
                                "Gastric Bypass") )  %>% 
  as_flex_table()  

 
cnt_N =   function(tb)  { tb %>% 
  select(p_alder_v_op, Female, bmi_0, b_beh_diab, b_beh_hypert, b_beh_dyslip, b_beh_dyspepsi,
         b_beh_hypert, b_beh_musk_skjsm, b_beh_depr, b_beh_sovnap, smoke, work ) %>%
  tbl_summary(
    label = list(p_alder_v_op ~ "Age", bmi_0 ~ "BMI", smoke ~ "Smoking", 
                 work ~ "Working" , b_beh_musk_skjsm ~ "Muskular-sceletal pain",
                 b_beh_diab ~ "Diabetes", b_beh_hypert ~ "Hypertension", 
                 b_beh_dyslip ~ "Dyslipidemi", b_beh_dyspepsi ~ "GERD", b_beh_sovnap ~ "Sleep apnoea",  
                 b_beh_depr ~ "Depression"), 
    statistic = gtsummary::all_continuous()  ~ "{mean} ({sd})",
    digits = list(p_alder_v_op ~ c(1, 1)) ) } #  

opr_N =   function(tb, op)  { 
  
  om = case_when( op == "GS" ~ 6,
                  op == "GB" ~ 1,
                  TRUE ~ 99)
T =  tb   %>%
  filter(o_opmetode == om) %>%
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
T }


# tGS2_act = d_act_nt6 %>%
#   filter(o_opmetode == 6) %>%
#   select(trt, a5_fu,    vent_a5,   ligg_a5,   reinn_a5,
#          alv_kmp_a5,  subst_a5,   depr_a5,   vtap_a5,   dBMI_a5) %>% 
#   tbl_summary(
#     by = trt,
#     statistic = list( a5_fu ~"{n}", 
#                       reinn_a5  ~ "{n} / {N} ({p}%)"  ,
#                       alv_kmp_a5~ "{n} / {N} ({p}%)"  ,
#                       subst_a5~ "{n} / {N} ({p}%)"  ,
#                       depr_a5~ "{n} / {N} ({p}%)" ),
#     label = list(a5_fu ~"Follow-up 5 yrs",
#                  vent_a5 ~"Waiting time ", 
#                  ligg_a5 ~"Postoperative days in hospital ", 
#                  reinn_a5 ~"Readmission ", 
#                  alv_kmp_a5 ~"Severe complication (30d) ", 
#                  subst_a5 ~"Substitution ", 
#                  depr_a5 ~"Depression ", 
#                  vtap_a5 ~"%TWL ", 
#                  dBMI_a5  ~"d BMI "),
#     missing_text = "Missing data" ) %>%  
#   add_p(test  = list(
#     gtsummary::all_continuous()  ~ "t.test", 
#     gtsummary::all_categorical() ~ "fisher.test") ) %>%
#   modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
#                 text_interpret ="md")
# 