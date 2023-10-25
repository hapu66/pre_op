#
# source("set_up.R")
#

  #   Count the n- column(s) for cohort(s)  eligible/ follow up
n_cl = function(ch) { ch |> 
    select(bi_finans, p_alder_v_op, Female, bmi_0, b_beh_musk_skjsm, b_beh_depr,
           b_beh_diab,  b_beh_hypert, b_beh_dyslip,  b_beh_sovnap, b_beh_dyspepsi,
           smoke, work) |>
    tbl_summary(
      label = list(bi_finans ~ "Financing",
                   p_alder_v_op ~ "Age",
                   bmi_0 ~ "BMI",
                   b_beh_musk_skjsm ~ "Muscular-sceletal pain",
                   b_beh_depr ~ "Depression",
                   b_beh_diab ~ "T2DM",
                   b_beh_hypert ~ "Hypertension",
                   b_beh_dyslip ~ "Dyslipidemi",
                   b_beh_sovnap ~ "Sleep apnoea",
                   b_beh_dyspepsi ~ "GERD",
                   smoke ~ "Smoking",
                   work ~ "Working"),
      statistic = list(all_continuous()  ~ "{mean} ({sd})",
                       all_dichotomous() ~ "{n} / {N} ({p}%)"),
      digits = list(p_alder_v_op ~ c(1, 1)),
      missing_text = "Missing data"
    )  
    }

  #   Count the n- column(s) for cohort(s)  eligible/ follow up
n_opr = function(ch, op) {  
    
    om = case_when( op == "GS" ~ 6,  # str_detect()?
                    op == "GB" ~ 1,
                    TRUE ~ NA_integer_)
    T = ch |>
      filter(o_opmetode == om) |>
      select(bi_finans, p_alder_v_op, Female, bmi_0, b_beh_musk_skjsm, b_beh_depr,
             b_beh_diab,  b_beh_hypert, b_beh_dyslip,  b_beh_sovnap, b_beh_dyspepsi,
             smoke, work, trt) |>
      tbl_summary( 
        by = trt,
        label = list(bi_finans ~ "Financing",
                     p_alder_v_op ~ "Age",
                     bmi_0 ~ "BMI",
                     b_beh_musk_skjsm ~ "Muscular-sceletal pain",
                     b_beh_depr ~ "Depression",
                     b_beh_diab ~ "T2DM",
                     b_beh_hypert ~ "Hypertension",
                     b_beh_dyslip ~ "Dyslipidemi",
                     b_beh_sovnap ~ "Sleep apnoea",
                     b_beh_dyspepsi ~ "GERD",
                     smoke ~ "Smoking",
                     work ~ "Working"),
        statistic = list(all_continuous()  ~ "{mean} ({sd})",
                         all_dichotomous() ~ "{n} / {N} ({p}%)"),
        digits = list(p_alder_v_op ~ c(1, 1)),
        missing_text = "Missing data"
      ) |>
      modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                    text_interpret ="md")
    T
}

   # put two columns and opr results together
cb_chs  = function(ch_e, ch_fu, op){
  
  # op_str = ifelse(str_detect(ch_fu, "GS"),
  #                 c("Eligible for  \n 30 d follow-up",  # time
  #                   "Actual   \n 30 d follow-up", 
  #                   "Gastric Sleeve" ) , 
  #             ifelse(str_detect(ch_fu, "GB"),
  #                    c("Eligible for  \n 5 yr follow-up",
  #                      "Actual   \n 5 yr follow-up", 
  #                      "Gastric Bypass" ), 
  #                                             "neither GS nor GB ?!")
  #                 )
 # fix  similar solution for time d30  a5
 #  
   
  cl_1  = n_cl(ch_e)
  cl_2  = n_cl(ch_fu)
  cl_34 = n_opr(ch_fu, op)
  T = tbl_merge( tbls = list(cl_1, cl_2, cl_34) )
  T
}

#n_fu   = function(ch){ ch |> --------------------------------------------------
#  
#} 

cnt_N =   function(tb)  { tb %>% 
    select(bi_finans, p_alder_v_op, Female, bmi_0, b_beh_diab, b_beh_hypert, b_beh_dyslip, b_beh_dyspepsi,
           b_beh_hypert, b_beh_musk_skjsm, b_beh_depr, b_beh_sovnap, smoke, work ) %>%
    tbl_summary(
      label = list(bi_finans ~ "Financing", p_alder_v_op ~ "Age", bmi_0 ~ "BMI", smoke ~ "Smoking", 
                   work ~ "Working" , b_beh_musk_skjsm ~ "Muscular-sceletal pain",
                   b_beh_diab ~ "Diabetes", b_beh_hypert ~ "Hypertension", 
                   b_beh_dyslip ~ "Dyslipidemi", b_beh_dyspepsi ~ "GERD", b_beh_sovnap ~ "Sleep apnoea",  
                   b_beh_depr ~ "Depression"), 
      #    statistic = gtsummary::all_continuous()  ~ "{mean} ({sd})",
      digits = list(p_alder_v_op ~ c(1, 1)),
      missing_text = "Missing data") } #  


opr_N =   function(tb, op)  { 
  
  om = case_when( op == "GS" ~ 6,
                  op == "GB" ~ 1,
                  TRUE ~ NA_integer_)
  T =  tb   %>%
    filter(o_opmetode == om) %>%
    select(bi_finans, p_alder_v_op, Female, bmi_0, b_beh_diab, b_beh_hypert, b_beh_dyslip, b_beh_dyspepsi,
           b_beh_hypert, b_beh_musk_skjsm, b_beh_depr, b_beh_sovnap, smoke, work, trt) %>%
    tbl_summary(
      by        = trt,
      label = list(bi_finans ~ "Financing", p_alder_v_op ~ "Age", bmi_0 ~ "BMI", smoke ~ "Smoking", 
                   work ~ "Working" , b_beh_musk_skjsm ~ "Muscular-sceletal pain",
                   b_beh_diab ~ "Diabetes", b_beh_hypert ~ "Hypertension", 
                   b_beh_dyslip ~ "Dyslipidemi", b_beh_dyspepsi ~ "GERD", b_beh_sovnap ~ "Sleep apnoea",  
                   b_beh_depr ~ "Depression"), 
      #    statistic = gtsummary::all_continuous()  ~ "{mean} ({sd})",
      digits = list(p_alder_v_op ~ c(1, 1)),
      missing_text = "Missing data") %>%
    ##    add_overall() %>% 
    # add_p(test  = list(
    #   gtsummary::all_continuous()  ~ "t.test", 
    #   gtsummary::all_categorical() ~ "fisher.test") ) %>%
    modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                  text_interpret ="md") 
  T }



d_elig_d30 |>
  filter(o_opmetode == 6) |>
  select(trt, bi_finans, p_alder_v_op, Female, bmi_0o,               # BMI 3x?
        b_beh_musk_skjsm, b_beh_depr, b_beh_diab, b_beh_hypert, b_beh_dyslip, 
        b_beh_sovnap, b_beh_dyspepsi, smoke, work) |> tbl_summary(by = trt) |>
  add_overall()

d_act_d30 = d_elig_d30 %>% filter(u6_ferdigstill == 1)


t_e_d30 = d_elig_d30 %>% 
  select(bi_finans, p_alder_v_op, Female, bmi_0, b_beh_diab, b_beh_hypert, b_beh_dyslip, b_beh_dyspepsi,
         b_beh_hypert, b_beh_musk_skjsm, b_beh_depr, b_beh_sovnap, smoke, work ) %>%
  tbl_summary(
    label = list(bi_finans ~ "Financing", p_alder_v_op ~ "Age", bmi_0 ~ "BMI", smoke ~ "Smoking", 
                 work ~ "Working" , b_beh_musk_skjsm ~ "Muskular-sceletal pain",
                 b_beh_diab ~ "Diabetes", b_beh_hypert ~ "Hypertension", 
                 b_beh_dyslip ~ "Dyslipidemi", b_beh_dyspepsi ~ "GERD", b_beh_sovnap ~ "Sleep apnoea",  
                 b_beh_depr ~ "Depression"), 
    statistic = gtsummary::all_continuous()  ~ "{mean} ({sd})",
    digits = list(p_alder_v_op ~ c(1, 1)),
    missing_text = "Missing data") #  %>%
#  as_gt %>% opt_footnote_marks(marks = "letters") 

t_fu_d30 = d_act_d30 %>% 
  select(bi_finans, p_alder_v_op, Female, bmi_0, b_beh_diab, b_beh_hypert, b_beh_dyslip, b_beh_dyspepsi,
         b_beh_hypert, b_beh_musk_skjsm, b_beh_depr, b_beh_sovnap, smoke, work ) %>%
  tbl_summary(
    label = list(bi_finans ~ "Financing", p_alder_v_op ~ "Age", bmi_0 ~ "BMI", smoke ~ "Smoking", 
                 work ~ "Working" , b_beh_musk_skjsm ~ "Muskular-sceletal pain",
                 b_beh_diab ~ "Diabetes", b_beh_hypert ~ "Hypertension", 
                 b_beh_dyslip ~ "Dyslipidemi", b_beh_dyspepsi ~ "GERD", b_beh_sovnap ~ "Sleep apnoea",  
                 b_beh_depr ~ "Depression"), 
    statistic = gtsummary::all_continuous()  ~ "{mean} ({sd})",
    digits = list(p_alder_v_op ~ c(1, 1)),
    missing_text = "Missing data") #  %>%

tGS_d30 = d_act_d30   %>%
  filter(o_opmetode == 6) %>%
  select(bi_finans, p_alder_v_op, Female, bmi_0, b_beh_diab, b_beh_hypert, b_beh_dyslip, b_beh_dyspepsi,
         b_beh_hypert, b_beh_musk_skjsm, b_beh_depr, b_beh_sovnap, smoke, work, trt) %>%
  tbl_summary(
    by        = trt,
    label = list(bi_finans ~ "Financing", p_alder_v_op ~ "Age", bmi_0 ~ "BMI", smoke ~ "Smoking", 
                 work ~ "Working" , b_beh_musk_skjsm ~ "Muskular-sceletal pain",
                 b_beh_diab ~ "Diabetes", b_beh_hypert ~ "Hypertension", 
                 b_beh_dyslip ~ "Dyslipidemi", b_beh_dyspepsi ~ "GERD", b_beh_sovnap ~ "Sleep apnoea",  
                 b_beh_depr ~ "Depression"), 
    statistic = gtsummary::all_continuous()  ~ "{mean} ({sd})",
    digits = list(p_alder_v_op ~ c(1, 1)),
    missing_text = "Missing data") %>%
  add_p(test  = list(
    gtsummary::all_continuous()  ~ "t.test", 
    gtsummary::all_categorical() ~ "fisher.test") ) %>%
  modify_header(update = all_stat_cols() ~ "**{level}**  \n N = {n}",
                text_interpret ="md")  

tGB_d30 =  d_act_d30   %>%
  filter(o_opmetode == 1) %>%
  select(bi_finans, p_alder_v_op, Female, bmi_0, b_beh_diab, b_beh_hypert, b_beh_dyslip, b_beh_dyspepsi,
         b_beh_hypert, b_beh_musk_skjsm, b_beh_depr, b_beh_sovnap, smoke, work, trt) %>%
  tbl_summary(
    by        = trt,
    label = list(bi_finans ~ "Financing", p_alder_v_op ~ "Age", bmi_0 ~ "BMI", smoke ~ "Smoking", 
                 work ~ "Working" , b_beh_musk_skjsm ~ "Muskular-sceletal pain",
                 b_beh_diab ~ "Diabetes", b_beh_hypert ~ "Hypertension", 
                 b_beh_dyslip ~ "Dyslipidemi", b_beh_dyspepsi ~ "GERD", b_beh_sovnap ~ "Sleep apnoea",  
                 b_beh_depr ~ "Depression"), 
    statistic = gtsummary::all_continuous()  ~ "{mean} ({sd})",
    digits = list(p_alder_v_op ~ c(1, 1)),
    missing_text = "Missing data") %>%
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

 




c1 = cnt_N(d_elig_GS_d30)
c2 = cnt_N(d_act_GS_d30)
c34 = opr_N(d_act_GS_d30, "GS")


c_1 = cnt_N(d_elig_GS)
c_2 = cnt_N(d_act_GS_nt6)
c_34 = opr_N(d_act_GS_nt6, "GS")

T_GS_d30 =  tbl_merge(tbls =  list(c1,c2,c34),
                tab_spanner = c("Eligible for  \n 30 d follow-up",
                                "Actual   \n 30 d follow-up", 
                                "Gastric Sleeve" ) )  # %>%   as_flex_table()  

T_GS_a5 =  tbl_merge(tbls =  list(c_1,c_2,c_34), 
                      tab_spanner = c("Eligible for  \n 5 yr follow-up",
                                      "Actual   \n 5 yr follow-up", 
                                      "Gastric Sleeve" ) ) # %>%  as_flex_table()  
d_1 = cnt_N(d_elig_GB)
d_2 = cnt_N(d_act_GB_nt6)
d_34 = opr_N(d_act_GB_nt6,"GB")

T_GB_a5 =  tbl_merge(tbls =  list(d_1,d_2,d_34), 
                     tab_spanner = c("Eligible for  \n 5 yr follow-up",
                                     "Actual   \n 5 yr follow-up", 
                                     "Gastric Bypass" ) ) # %>%  as_flex_table()  

b1 = cnt_N(d_elig_GB_d30)
b2 = cnt_N(d_act_GB_d30)
b34 = opr_N(d_act_GB_d30, "GB")


b_1 = cnt_N(d_elig)
b_2 = cnt_N(d_act_nt6)
b_34 = opr_N(d_act_nt6,"GB")

T_GB_d30 =  tbl_merge(tbls =  list(b1,b2,b34),
                      tab_spanner = c("Eligible for  \n 30 d follow-up",
                                      "Actual   \n 30 d follow-up", 
                                      "Gastric Bypass" ) ) # %>%   as_flex_table()  

T_GB_a5 =  tbl_merge(tbls =  list(b_1,b_2,b_34),
                     tab_spanner = c("Eligible for  \n 5 yr follow-up",
                                     "Actual   \n 5 yr follow-up", 
                                     "Gastric Bypass" ) )  # %>%   as_flex_table()  
##############
T_GS = tbl_merge(tbls = list(T_GS_d30, T_GS_a5), tab_spanner = c("**30 d**","**a 5**"))
T_GB = tbl_merge(tbls = list(T_GB_d30, T_GB_a5), tab_spanner = c("**30 d**","**a 5**"))
###############

TA =  T_GS %>% as_gt %>% opt_footnote_marks(marks = "letters") 
TB =  T_GB %>% as_gt %>% opt_footnote_marks(marks = "letters")
TC =  T_GS_GB %>% as_gt %>% opt_footnote_marks(marks = "letters") 

library(officer) 
library(gto)
library(gt)

#  gt_tbl <- gt(head(exibble))

doc <- read_docx()
doc <- body_add_gt(doc, value = TA)  # T_GB  T_GS_GB  TA TB TC
fileout <-   "TA.docx"
print(doc, target = fileout)

#################### 


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
