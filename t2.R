
  source("set_up.R")

# d_act_GS_d30 %>% 
#   select(trt, vent, vt_pr, ligg, reinn,  alv_kmp) %>%
#   tbl_summary(
#     by = trt,
#     type = all_continuous() ~ "continuous2",
#     statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{mean}, {sd}"),
#     label  =  vent ~ "Waiting time (d)",
#     missing_text = "Missing data" ) %>%
#   add_p() %>%
#   add_ci()

shrt_res = function(tb) {tb %>% 
  select(trt, vent, vt_pr, ligg, reinn,  alv_kmp) %>%
  tbl_summary(
    by = trt,
    type = list( vent ~ "continuous2",
                 vt_pr ~ "continuous",
                 ligg ~  "continuous",
                 reinn ~ "dichotomous",
                 alv_kmp ~ "dichotomous")              ,
    statistic = list(        vent ~ c("{median} ({p25}, {p75})", "{mean}, ({sd}) "),
                             vt_pr ~ "{mean} ({sd})",
                             ligg  ~ "{mean} ({sd})",
                              reinn ~ "{n} / {N} ({p}%)",
                              alv_kmp ~ "{n} / {N} ({p}%)") ,
    label  = list( vent ~ "Waiting time (d)",
                   vt_pr ~ "Pre-operative weight loss",
                   ligg ~ "Postoperative days in hospital",
                   reinn ~ "Readmission",
                   alv_kmp ~ "Severe complications (30 d)"),
 #   missing = "no",
    missing_text = "Missing data" ) %>%
  add_p() %>%
  add_ci()}


lng_res = function(tb) {tb %>%
  select(trt, a5_fu,    vent_a5,   ligg_a5,   reinn_a5,
         alv_kmp_a5,  subst_a5,   depr_a5,   vtap_a5,   dBMI_a5) %>% 
  tbl_summary(
    by = trt,
    statistic = list( a5_fu ~"{n}", 
                      reinn_a5  ~ "{n} / {N} ({p}%)"  ,
                      alv_kmp_a5~ "{n} / {N} ({p}%)"  ,
                      subst_a5~ "{n} / {N} ({p}%)"  ,
                      depr_a5~ "{n} / {N} ({p}%)" ),
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
                text_interpret ="md")}



shrt_res(  d_act_GS_d30 )
shrt_res(  d_act_GB_d30 )

lng_res(  d_act_GS_nt6 )
lng_res(  d_act_GB_nt6 )


  
tGS2_act = d_act_GS_nt6 %>%
  select(trt, a5_fu,    vent_a5,   ligg_a5,   reinn_a5,
         alv_kmp_a5,  subst_a5,   depr_a5,   vtap_a5,   dBMI_a5) %>% 
  tbl_summary(
    by = trt,
    statistic = list( a5_fu ~"{n}", 
                      reinn_a5  ~ "{n} / {N} ({p}%)"  ,
                      alv_kmp_a5~ "{n} / {N} ({p}%)"  ,
                      subst_a5~ "{n} / {N} ({p}%)"  ,
                      depr_a5~ "{n} / {N} ({p}%)" ),
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


tGB2_act = d_act_GB_nt6 %>%
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


tb_GS = d_elig %>% filter(o_opmetode == 6)  %>% select(trt, o_preop_vektskole) %>%  tbl_summary(by=trt)  

tb_GB = d_elig %>% filter(o_opmetode == 1) %>% select(trt, o_preop_vektskole) %>%  tbl_summary(by=trt)  

###############################################  final table
tA = tbl_stack(list(tb_GS, tGS2_act))
tB = tbl_stack(list(tb_GB, tGB2_act))

tbl_merge(list(tA,tB), tab_spanner = c("**Gastric Sleeve**", "**Gastric Bypass**" ) )  %>%
  modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                text_interpret ="md") %>% 
  remove_row_type(variables = o_preop_vektskole, type =  c("all"), level_value = NULL)  # %>% 
#  as_gt() %>%  gt::gtsave("tabell2.docx")
 
