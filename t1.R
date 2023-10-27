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
    ) # |>
#    modify_header(update = all_stat_cols() ~ "Sleeve \n operated  \n N = {n}",
#                  text_interpret ="md") 
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
   #  # string function for titlene grunnet opr.type GS/GB  og oppfølgningstid d30 / a5
titl =  function(op){
          case_when( op == "GS" ~  c("Eligible for  \n 30 d follow-up",         # time
                                                        "Actual   \n 30 d follow-up", 
                                                        "Gastric Sleeve" ),     # str_detect()?
                     op == "GB" ~ c("Eligible for  \n 30 d follow-up",         # time
                                    "Actual   \n 30 d follow-up", 
                                    "Gastric Bypass" ),
           TRUE ~ NA_character_)}

   # put two columns and opr results together
cb_chs  = function(ch_e, ch_fu){   # , op
  
#  ch_e = d_elig_GS_d30
#  ch_fu = d_act_GS_d30
  
  el_nm = deparse(substitute(ch_e))  # name of the eligible tb
  fu_nm = deparse(substitute(ch_fu)) #             follow-up
  
  fu_t = ifelse(str_detect(fu_nm, "d30"), "d30", 
          ifelse(str_detect(fu_nm, "a5"),
                "a5",  "ERROR!"))
  opr_mt =  ifelse(str_detect(fu_nm, "GS"), "GS", 
                   ifelse(str_detect(fu_nm, "GB"),
                          "GB",  "ERROR!"))
#  ttl_str = str_glue("Eligible for \n {fu_t} follow-up, {opr_mt}")
  # tb_span =  glue(c("Eligible for  \n {fu_t} follow-up",
  #                       "Actual        \n {fu_t} follow-up",
  #                       "{opr_mt}"))
  
  tb_sp1 = str_glue("Eligible for  \n {fu_t} follow-up")
  tb_sp2 = str_glue("Actual        \n {fu_t} follow-up")
  tb_sp3 = str_glue("{opr_mt}")   
  
  tb_span = c(tb_sp1, tb_sp2, tb_sp3)
###  case_when()
  #  
   
  cl_1  = n_cl(ch_e)
  cl_2  = n_cl(ch_fu)
  cl_34 = n_opr(ch_fu,  opr_mt )
  T = tbl_merge( tbls = list(cl_1, cl_2, cl_34), tab_spanner =  tb_span )
  T
}

#   remove over-titles!
# tbl_merge( list( cb_chs(d_elig_GS_d30, d_act_GS_d30 ) , cb_chs(d_elig_GB_d30, d_act_GB_d30 )), tab_spanner = FALSE)


tb1_GS =  tbl_merge( list(
cb_chs(d_elig_GS_d30, d_act_GS_d30 ),
cb_chs(d_elig_GS, d_act_GS_a5 )
), tab_spanner = c("**Results for 30 days**", "**Results for 5 years**"))

T1S =  tbl_merge( list(
  cb_chs(d_elig_GS_d30, d_act_GS_d30 ),
  cb_chs(d_elig_GS, d_act_GS_a5 )
), tab_spanner = FALSE)

T1B =  tbl_merge( list(
  cb_chs(d_elig_GB_d30, d_act_GB_d30 ),
  cb_chs(d_elig_GB, d_act_GB_a5 )
), tab_spanner = FALSE)


tb2_GB =  tbl_merge( list(
  cb_chs(d_elig_GB_d30, d_act_GB_d30 ),
  cb_chs(d_elig_GB, d_act_GB_a5 )
), tab_spanner = c("**Results for 30 days**", "**Results for 5 years**"))


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

# theme_gtsummary_journal(journal = "jama")
# 
# theme_gtsummary
# theme_gtsummary_compact()
# theme_gtsummary_printer(print_engine=)
# theme_gtsummary_mean_sd()
# 
# theme_gtsummary_language("nl")

# reset_gtsummary_theme()