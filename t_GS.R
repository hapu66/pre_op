#
source("set_up.R")
#

d_elig_d30 |>
  filter(o_opmetode == 6) |>
  select(trt, b_finans, p_alder_v_op, Female, bmi_0o,                 # BMI 3x?
        b_beh_musk_skjsm, b_beh_depr, b_beh_diab, b_beh_hypert, b_beh_dyslip, 
        b_beh_sovnap, b_beh_dyspepsi, smoke, work) |> tbl_summary(by = trt) |>
  add_overall()

d_act_d30

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
