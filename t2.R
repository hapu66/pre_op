
#  source("set_up.R")

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

# Example 2 ----------------------------------
# Use `data[[variable]]` to access the current variable
mean_ci <- function(data, variable, ...) {
  test <- t.test(data[[variable]])
  dplyr::tibble(
    mean = test$estimate,
    conf.low = test$conf.int[1],
    conf.high = test$conf.int[2]
  )
}

shrt_res = function(tb) {tb %>% 
    select(trt, vent, vt_pr, ligg, reinn,  alv_kmp) %>%
    tbl_summary(
      by = trt,
      type = list( vent ~ "continuous2",
                   vt_pr ~ "continuous",
                   ligg ~  "continuous",
                   reinn ~ "dichotomous",
                   alv_kmp ~ "dichotomous") ,
      statistic = list(        vent ~ c("{median} ({p25}, {p75})", 
                                        "{mean} [{min}; {max}]"),  #  CI?
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
    add_p() }  #  %>%  add_ci(include=vent) } #  }  add_ci :  extra col.


shrt_res_cstm = function(tb) {tb %>% 
  select(trt, vent, vt_pr, ligg, reinn,  alv_kmp) %>%
  tbl_custom_summary(
    by = trt,
    type = list( vent ~ "continuous2",
                 vt_pr ~ "continuous",
                 ligg ~  "continuous",
                 reinn ~ "dichotomous",
                 alv_kmp ~ "dichotomous") ,
    stat_fns = ~mean_ci,
    statistic = list(        vent ~ c("{median} ({p25}, {p75})", 
                                      "{mean} [{conf.low}; {conf.high}]"),
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
  add_p() }  #  %>%  add_ci(include=vent) } #  }  add_ci :  extra col.


cstm_res = d_act_d30 |>
  tbl_custom_summary(
    include = c("vent"),
    by = trt,
    stat_fns = ~mean_ci,
    statistic = ~"{mean} [{conf.low}; {conf.high}]"
  )


tbl_custom_summary_ex2 <-
  trial %>%
  tbl_custom_summary(
    include = c("marker", "ttdeath"),
    by = "trt",
    stat_fns = ~mean_ci,
    statistic = ~"{mean} [{conf.low}; {conf.high}]"
  ) %>%
  add_overall(last = TRUE) %>%
  modify_footnote(
    update = all_stat_cols() ~ "mean [95% CI]"
  )




lng_res = function(tb) {tb %>%
  select(trt, a5_fu, N_revop, vtap, dBMI, depr, subst, depr) %>% 
  tbl_summary(
    by = trt,
    statistic = list( a5_fu ~ "{n}", 
                      depr~ "{n} / {N} ({p}%)",
                      subst~ "{n} / {N} ({p}%)" ),
    label = list(a5_fu ~ "Follow-up 5 yrs",
                 N_revop ~ "Revisions",
                 depr ~ "Depression ", 
                 subst ~ "Substitution ", 
                 vtap ~ "%TWL ", 
                 dBMI  ~"d BMI "),
    missing_text = "Missing data" ) %>%  
  add_p(  test  =   all_dichotomous() ~ "fisher.test",
          include =  all_dichotomous()   ) %>%
  modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                text_interpret ="md")}



shrt_res(  d_act_GS_d30 )
shrt_res(  d_act_GB_d30 )

lng_res(  d_act_GS_nt6 )
lng_res(  d_act_GB_nt6 )

#######
T_GS_GB = tbl_stack(list(shrt_res(d_act_d30), lng_res(d_act_nt6)))
#  a) footnote

#####  
library(officer)
library(gto)
library(gt)

gt_tbl <- gt(head(exibble))

doc <- read_docx()
doc <- body_add_gt(doc, value = gt_tbl)
fileout <-   "exbl.docx"
print(doc, target = fileout)

######

# -------------  Who has NOT been followed up?
# d30  
d_elig_d30 |> select(trt, u6_ferdigstill, a5_ferdigstill) |>
  tbl_summary(by = trt, 
              statistic = contains("ferdigstill") ~ "{n} / {N} ({p}%)",
              label = list(u6_ferdigstill ~ "u6 follow-up", 
                           a5_ferdigstill ~ "a5 follow-up"
                           ))
## a5
d_elig |>  select(trt, u6_ferdigstill, a5_ferdigstill) |>
  tbl_summary(by = trt, 
              statistic = contains("ferdigstill") ~ "{n} / {N} ({p}%)",
              label = list(u6_ferdigstill ~ "u6 follow-up", 
                           a5_ferdigstill ~ "a5 follow-up"
              ))

  
tGS2_act = d_act_GS_nt6 %>%
  select(trt, a5_fu,    vent,   ligg,   reinn,
         alv_kmp,  subst,   depr,   vtap,   dBMI) %>% 
  tbl_summary(
    by = trt,
    statistic = list( a5_fu ~"{n}", 
                      reinn  ~ "{n} / {N} ({p}%)"  ,
                      alv_kmp~ "{n} / {N} ({p}%)"  ,
                      subst~ "{n} / {N} ({p}%)"  ,
                      depr~ "{n} / {N} ({p}%)" ),
    label = list(a5_fu ~"Follow-up 5 yrs",
                 vent ~"Waiting time ", 
                 ligg ~"Postoperative days in hospital ", 
                 reinn ~"Readmission ", 
                 alv_kmp ~"Severe complication (30d) ", 
                 subst ~"Substitution ", 
                 depr ~"Depression ", 
                 vtap ~"%TWL ", 
                 dBMI  ~"d BMI "),
    missing_text = "Missing data" ) %>%  
  add_p(test  = list(
    gtsummary::all_continuous()  ~ "t.test", 
    gtsummary::all_categorical() ~ "fisher.test") ) %>%
  modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                text_interpret ="md")


tGB2_act = d_act_GB_nt6 %>%
  select(trt, a5_fu,    vent,   ligg,   reinn,
         alv_kmp,  subst,   depr,   vtap,   dBMI) %>% 
  tbl_summary(
    by = trt,
    statistic = list( a5_fu ~"{n}", 
                      reinn  ~ "{n} / {N} ({p}%)"  ,
                      alv_kmp~ "{n} / {N} ({p}%)"  ,
                      subst~ "{n} / {N} ({p}%)"  ,
                      depr~ "{n} / {N} ({p}%)" ),
    #   stat_fns = a5_fu ~my_cnt,
    #    statistic = a5_fu ~ "{n}/ {a5_flw}  ",
    #   type = list(a5_fu ~ 'continuous'),
    label = list(a5_fu ~"Follow-up 5 yrs",
                 vent ~"Waiting time ", 
                 ligg ~"Postoperative days in hospital ", 
                 reinn ~"Readmission ", 
                 alv_kmp ~"Severe complication (30d) ", 
                 subst ~"Substitution ", 
                 depr ~"Depression ", 
                 vtap ~"%TWL ", 
                 dBMI  ~"d BMI "),
    missing_text = "Missing data" ) %>%  
  add_p(test  = list(
    gtsummary::all_continuous()  ~ "t.test", 
    gtsummary::all_categorical() ~ "fisher.test") ) %>%
  modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                text_interpret ="md")

 


tb_GS = d_elig_d30 %>% filter(o_opmetode == 6)  %>% select(trt, o_preop_vektskole) %>%  tbl_summary(by=trt)  

tb_GB = d_elig_d30 %>% filter(o_opmetode == 1) %>% select(trt, o_preop_vektskole) %>%  tbl_summary(by=trt)  

###############################################  final table
tA = tbl_stack(list(tb_GS, tGS2_act))
tB = tbl_stack(list(tb_GB, tGB2_act))

tbl_merge(list(tA,tB), tab_spanner = c("**Gastric Sleeve**", "**Gastric Bypass**" ) )  %>%
  modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                text_interpret ="md") %>% 
  remove_row_type(variables = o_preop_vektskole, type =  c("all"), level_value = NULL)  # %>% 
#  as_gt() %>%  gt::gtsave("tabell2.docx")
 
