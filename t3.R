
  # denominator for follow-up %
N_op_d30 <- d_elig_d30 |>  group_by(trt) |> summarise(N_opr = n())
N_op_a5  <- d_elig |>  group_by(trt) |> summarise(N_opr = n())  
 
N_act_d30 <- d_act_d30 |>  group_by(trt) |> summarise(N_opr = n())
N_act_a5  <- d_act_a5 |>  group_by(trt) |> summarise(N_opr = n())
    
fup_d30 =  N_act_d30$N_opr / N_op_d30$N_opr
fup_a5 =  N_act_a5$N_opr / N_op_a5$N_opr


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
                   dBMI  ~ "d BMI "),
      missing_text = "Missing data" 
    ) |>
    add_p()  # remove for a5_fu?
  }
  
s_GS =  sht_res(d_act_GS_d30)
l_GS =  lng_res(d_act_GS_nt6)
s_GB =  sht_res(d_act_GB_d30)
l_GB =  lng_res(d_act_GB_nt6)

# fix in the denominator with gt 
#  .list = rlang::list2( 

l_GS |> as_gt() |> rows_add(.list = rlang::list2(   "label" = "sdf",
                              "stat_1" = N_op_a5[1,2],
                              "stat_2" =  N_op_a5[2,2], 
                              "p.value" =0.05 ),   
                              .before = 2 )

# l_GS$table_body 
l_GS |> as_gt() |> 
  rows_add(   "label" = "potential 5 yr",
              "stat_1" = as.character( N_op_a5[1,2]),
              "stat_2" = as.character( N_op_a5[2,2]),
              "p.value" = NA ,   
              .before = 2 )  
  
l_GS |> as_gt() |> 
  rows_add( .list = rlang::list2(  "label" = "potential 5 yr",
              "stat_1" = as.character( N_op_a5[1,2]),
              "stat_2" = as.character( N_op_a5[2,2]),
              "p.value" = NA ),   
              .before = 2 )  |>
  rows_add( .list = rlang::list2(  "label" = "follow-up %; 5 yrs",
                                   "stat_1" = as.character( fup_a5[1]),
                                   "stat_2" = as.character( fup_a5[2]),
                                   "p.value" = NA ),   
            .before = 3 )

                          
             # as_tibble_row("label"= "rw", "stat_1" = N_op_a5[1,2], "stat_2" =  N_op_a5[2,2], "p.value" =0.05  ),
                  #          .before = 2    #   N_op_a5[1:2,2]
  #   stack tables  tbl_stack  
# tbl_stack( list(sht_res(d_act_GS_d30), lng_res(d_act_GS_nt6)))
  #   merge tables

tibble("label"= "rw", "stat_1" = N_op_a5[1,2], "stat_2" =  N_op_a5[2,2], "p.value" =0.05)
