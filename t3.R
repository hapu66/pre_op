
  # denominator for follow-up %
N_op_30d <- d_elig_d30 |>  group_by(trt) |> summarise(N_opr = n())
N_op_a5  <- d_elig |>  group_by(trt) |> summarise(N_opr = n())  
    
    
  # short term results, cohort: d_act_GS_d30,  d_act_GB_d30
sht_res  = function(tb) { tb |> 
    select(trt, vent, vt_pr, ligg, reinn, alv_kmp) |>
    tbl_summary( 
      by = trt,
      type = list(vent ~  "continuous2",
                  vt_pr ~ "continuous",
                  ligg ~ "continuous",
                  reinn   ~ "dichotomous",
                  alv_kmp ~ "dichotomous"      ),
      statistic = list(vent ~ c("{median} ({p25}, {p75})", 
                                "{mean} [{min}; {max}]"),    #  !! fix CI
                       vt_pr ~ "{mean} ({sd})", 
                       ligg ~ "{mean} ({sd})",
                       reinn ~ "{n} / {N} ({p}%)", 
                       alv_kmp ~"{n} / {N} ({p}%)"),
      label = list( vent ~ "Waiting time (d)",
                    vt_pr ~ "Pre-operative weight loss",
                    ligg ~ "Postoperative days in hospital",
                    reinn ~ "Readmission",
                    alv_kmp ~ "Severe complications (30 d)"),
      missing_text = "Missing data" 
        ) |>
    add_p()
  }
    
  #  long term results, cohort: d_act_GS_nt6,  d_act_GB_nt6     
lng_res  = function(tb) { tb |> 
    select(trt, a5_fu, N_revop, vtap, dBMI, depr, subst, depr) |>
    tbl_summary( 
      by = trt,
      type = list( c(a5_fu, depr, subst) ~ "dichotomous"),   ##  c()
      statistic = list(a5_fu ~ "{n}", 
                       depr~ "{n} / {N} ({p}%)",
                       subst~ "{n} / {N} ({p}%)"),
      label = list(a5_fu ~ "Follow-up 5 yrs",
                   N_revop ~ "Revisions",
                   depr ~ "Depression ", 
                   subst ~ "Substitution ", 
                   vtap ~ "%TWL ", 
                   dBMI  ~"d BMI "),
      missing_text = "Missing data" 
    ) |>
    add_p()
  }
  
  
  # fix in the denominator with gt
  
  #   stack tables  tbl_stack  
  
  #   merge tables