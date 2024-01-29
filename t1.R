#
 # source("set_up.R")
#

lz2_str =  function(ch, ag){
  ch_nm = deparse(substitute(ch))
    sts  = ifelse(str_detect(ch_nm, "elig"), "elig", ifelse(str_detect(ch_nm, "act"),  "act",  "status ERROR!"))
    tim  = ifelse(str_detect(ch_nm, "d30"), "d30", ifelse(str_detect(ch_nm, "a5"),  "a5",  "t ERROR!"))
    opm  = ifelse(str_detect(ch_nm, "GS"), "GS", ifelse(str_detect(ch_nm, "GB"),  "GB",  "opm ERROR!"))
    
    str = case_when( ag == "st" ~  sts,
                     ag == "ti" ~ tim,
                     ag == "om" ~ opm,
               TRUE ~  "Error: ag = st/ti/om") 
    str
}

lz_str =  function(ch, arg){
  ch_nm = deparse(substitute(ch))
    str = case_when(arg == "st" ~   str_split(ch_nm, "_")[[1]][2],
                    arg == "ti" ~   str_split(ch_nm, "_")[[1]][3],
                    arg == "om" ~   str_split(ch_nm, "_")[[1]][4])
    str
}

#  str_split("d_elig_d30_GS","_")[[1]][3]
# [1] "d30"

  #   Count the n- column(s) for cohort(s)  eligible/ follow up
n_cl = function(ch) { 
  ch_nm = deparse(substitute(ch))
  
  lz_str =  function(ch, arg){
    str = case_when(arg == "st" ~   str_split(ch_nm, "_")[[1]][2],
                    arg == "ti" ~   str_split(ch_nm, "_")[[1]][3],
                    arg == "om" ~   str_split(ch_nm, "_")[[1]][4])
    str  }
  
  sts = lz_str(ch , "st")
  tim = lz_str(ch , "ti")
  opm = lz_str(ch , "om")
#  tb_sp = str_glue("{sts} for  \n {tim} {opm} follow-up \n N = {n}")
    ch |> 
    select(b_finans, p_alder_v_op, Female, bmi_0, b_beh_musk_skjsm, b_beh_depr,
           b_beh_diab,  b_beh_hypert, b_beh_dyslip,  b_beh_sovnap, b_beh_dyspepsi,
           smoke, work) |>
    tbl_summary(
      label = list(b_finans ~ "Financing",
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
    )  |>
    modify_header(update = all_stat_cols() ~  "N = {n}" ,
                  text_interpret ="md") 
    }

  #   Count the n- column(s) for cohort(s)  eligible/ follow up
n_opr = function(ch, op) {  
    
    om = case_when( op == "GS" ~ 6,  # str_detect()?
                    op == "GB" ~ 1,
                    TRUE ~ NA_integer_)
    T = ch |>
      filter(o_opmetode == om) |>
      select(b_finans, p_alder_v_op, Female, bmi_0, b_beh_musk_skjsm, b_beh_depr,
             b_beh_diab,  b_beh_hypert, b_beh_dyslip,  b_beh_sovnap, b_beh_dyspepsi,
             smoke, work, trt) |>
      tbl_summary( 
        by = trt,
        label = list(b_finans ~ "Financing",
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
  
#  ch_e = d_elig_d30_GS
#  ch_fu = d_act_d30_GS
  
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
# tbl_merge( list( cb_chs(d_elig_d30_GS, d_act_d30_GS ) , cb_chs(d_elig_d30_GB, d_act_d30_GB )), tab_spanner = FALSE)


tb1_GS =  tbl_merge( list(
cb_chs(d_elig_d30_GS, d_act_d30_GS ),
cb_chs(d_elig_GS, d_act_a5_GS )
), tab_spanner = c("**Results for 30 days**", "**Results for 5 years**"))

T1S =  tbl_merge( list(
  cb_chs(d_elig_d30_GS, d_act_d30_GS ),
  cb_chs(d_elig_GS, d_act_a5_GS )
), tab_spanner = FALSE)

T1B =  tbl_merge( list(
  cb_chs(d_elig_d30_GB, d_act_d30_GB ),
  cb_chs(d_elig_GB, d_act_a5_GB )
), tab_spanner = FALSE)


tb2_GB =  tbl_merge( list(
  cb_chs(d_elig_d30_GB, d_act_d30_GB ),
  cb_chs(d_elig_GB, d_act_a5_GB )
), tab_spanner = c("**Results for 30 days**", "**Results for 5 years**"))



# T1S %>% as_gt() %>% opt_footnote_marks(marks = "letters") %>% gtsave("t1_GS.docx")
# T1B %>% as_gt() %>% opt_footnote_marks(marks = "letters") %>% gtsave("t2_GB.docx")

n_op = function(ch) {  #   put both GS and GB in the same table
  T = ch |>
# filter(o_opmetode == om) |>
    select(b_finans, p_alder_v_op, Female, bmi_0, b_beh_musk_skjsm, b_beh_depr,
           b_beh_diab,  b_beh_hypert, b_beh_dyslip,  b_beh_sovnap, b_beh_dyspepsi,
           smoke, work, trt) |>
    tbl_summary( 
      by = trt,
      label = list(b_finans ~ "Financing",
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


cmb = function(ch_e, ch_fu){
  el_nm = deparse(substitute(ch_e))  # name of the eligible tb
  fu_nm = deparse(substitute(ch_fu)) #             follow-up
  opr_mt = "Pre-operative program"
  
  fu_t = ifelse(str_detect(fu_nm, "d30"), "d30", 
                ifelse(str_detect(fu_nm, "a5"), "a5",  "ERROR!"))
  tb_sp1 = str_glue("Eligible for  \n {fu_t} follow-up")
  tb_sp2 = str_glue("Actual        \n {fu_t} follow-up")
  tb_sp3 = str_glue("{opr_mt}")   
  
  tb_span = c(tb_sp1, tb_sp2, tb_sp3)
  cl_1  = n_cl(ch_e)
  cl_2  = n_cl(ch_fu)
  cl_34 = n_op(ch_fu)
  T = tbl_merge( tbls = list(cl_1, cl_2, cl_34), tab_spanner =  tb_span )
  T
}


# cmb(d_elig_d30, d_act_d30)
# cmb(d_elig, d_act_a5)


T1 = tbl_merge( list(
  cmb(d_elig_d30, d_act_d30),
  cmb(d_elig, d_act_a5)
), tab_spanner = c("**Results for 30 days**", "**Results for 5 years**"))

T2 = tbl_merge( list(
  cmb(d_elig_d30, d_act_d30),
  cmb(d_elig, d_act_a5)
), tab_spanner = FALSE)


# T1  %>% as_gt() %>% opt_footnote_marks(marks = "letters") %>% gtsave("t1a.docx")
# T2  %>% as_gt() %>% opt_footnote_marks(marks = "letters") %>% gtsave("t1b.docx")




#n_fu   = function(ch){ ch |> --------------------------------------------------
#  
#} 

cnt_N =   function(tb)  { tb %>% 
    select(b_finans, p_alder_v_op, Female, bmi_0, b_beh_diab, b_beh_hypert, b_beh_dyslip, b_beh_dyspepsi,
           b_beh_hypert, b_beh_musk_skjsm, b_beh_depr, b_beh_sovnap, smoke, work ) %>%
    tbl_summary(
      label = list(b_finans ~ "Financing", p_alder_v_op ~ "Age", bmi_0 ~ "BMI", smoke ~ "Smoking", 
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
    select(b_finans, p_alder_v_op, Female, bmi_0, b_beh_diab, b_beh_hypert, b_beh_dyslip, b_beh_dyspepsi,
           b_beh_hypert, b_beh_musk_skjsm, b_beh_depr, b_beh_sovnap, smoke, work, trt) %>%
    tbl_summary(
      by        = trt,
      label = list(b_finans ~ "Financing", p_alder_v_op ~ "Age", bmi_0 ~ "BMI", smoke ~ "Smoking", 
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