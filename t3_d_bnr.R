# denominator for follow-up %,  numbers 
N_op_d30 <- d_elig_d30 |>  group_by(trt) |> summarise(N_opr = n())
N_op_a5  <- d_elig |>  group_by(trt) |> summarise(N_opr = n())  

N_op_nt  <- d_act_a5 |>  group_by(trt) |> summarise(N_opr = n())  

N_act_d30 <- d_act_d30 |> group_by(trt) |> summarise(N_opr = n())
N_act_a5  <- d_act_a5 |> group_by(trt) |> summarise(N_opr = n())

fup_d30 =  N_act_d30$N_opr / N_op_d30$N_opr
fup_a5 =  N_act_a5$N_opr / N_op_a5$N_opr
#
cnt_d30 =    d_elig_d30 %>% tbl_summary(by = trt, include = c(u6_fu), label = u6_fu ~ "Followed up, 30d") %>% add_p()
cnt_d30_GS = d_elig_d30_GS %>% tbl_summary(by = trt, include = c(u6_fu), label = u6_fu ~ "Followed up, 30d")
cnt_d30_GB = d_elig_d30_GB %>% tbl_summary(by = trt, include = c(u6_fu), label = u6_fu ~ "Followed up, 30d")

#     Construct the upper part of T3 -------------------------------------------
up_d30 = function(tb) { tb |> 
    select(trt,  vent, vt_pr, ligg_mx3, reinn, alv_kmp) |>  
    tbl_summary( 
      by = trt,
      type = list( vent ~  "continuous",
                   vt_pr ~ "continuous",
                   ligg_mx3 ~ "dichotomous",
                   reinn   ~ "dichotomous",
                   alv_kmp ~ "dichotomous"      ),
      statistic = list( vent ~ c("{median} [{p25}, {p75}] "),
                        vt_pr ~ "{mean} ({sd})", 
                        ligg_mx3 ~ "{n} / {N} ({p}%)",
                        reinn ~ "{n} / {N} ({p}%)", 
                        alv_kmp ~ "{n} / {N} ({p}%)"),      #  digits = list(ligg ~ 2), 
      label = list(# u6_fu ~ "Follow-up 30 d",
        vent ~ "Waiting time (d)",
        vt_pr ~ "Pre-operative BMI loss (kg/m^2)",
        ligg_mx3 ~ "Max 3 postoperative days in hospital",
        reinn ~ "Readmission",
        alv_kmp ~ "Severe complications (30 d)"),
      missing_text = "Missing data" 
    ) |>    add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) #|>
         # add_ci(include=c("vent" ), pattern = "{stat} ({ci})") 
  #    #    add_ci(include=c("vent", "ligg"), pattern = "{stat} ({ci})")
    #  include=c("vent") , pattern = "{stat} ({ci})"
    }

cnt_a5 =    d_elig %>% tbl_summary(by = trt, include = c(a5_nt), label =    a5_nt ~ "Followed up, 5yr +- 6m") %>% add_p
cnt_a5_GS = d_elig_GS %>% tbl_summary(by = trt, include = c(a5_nt), label = a5_nt ~ "Followed up, 5yr +- 6m") %>% add_p
cnt_a5_GB = d_elig_GB %>% tbl_summary(by = trt, include = c(a5_nt), label = a5_nt ~ "Followed up, 5yr +- 6m") %>% add_p


#     Construct the lower part of T3 -------------------------------------------
dw_a5 = function(tb) { e_del = paste0("Delta BMI (kg/m^2)");
    tb |> 
    select(trt, vtap, dBMI,   subst) |>  #  a5_fu, N_revop, 
    tbl_summary( 
      by = trt,
      type = list( c(    subst) ~ "dichotomous"),   ##  c()
      statistic = list(# a5_fu ~ "{n}", 
                       # depr~ "{n} / {N} ({p}%)",
                       subst~ "{n} / {N} ({p}%)"),
      label = list(# a5_fu ~ "Follow-up 5 yrs",
                   #  N_revop ~ "Revisions",
                   # depr ~ "Depression ", 
                   subst ~ "Substitution ", 
                   vtap ~ "%TWL ", 
                   dBMI  ~ e_del),
#     missing = "no",  #  remove TWL BMI but ?keep substitution--sol: add later?
      missing_text = "Missing data" 
    ) |>
    add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))  #   include = list(c(vtap, dBMI, depr, subst))
}

tbl_merge( list(
tbl_stack(list(cnt_d30_GS, up_d30(d_elig_d30_GS), cnt_a5_GS, dw_a5(d_elig_GS) )),
tbl_stack(list(cnt_d30_GB, up_d30(d_elig_d30_GB), cnt_a5_GB, dw_a5(d_elig_GB) )) 
))


epep =   tbl_stack(list(cnt_d30, up_d30(d_elig_d30), cnt_a5, dw_a5(d_elig)))

epep |>  as_gt() |>  
  rows_add( .list = rlang::list2("label" =  "Opr.  >5.5 yr earlier",
                                 "stat_1" = as.character(N_op_a5$N_opr[1]),
                                 "stat_2" = as.character(N_op_a5$N_opr[2])),
            .before = 11 ) |> 
  rows_add( .n_empty = 1, .before = 11)





# M_GS_l = matrix(c(690,688,1236,1088),
#                 nrow = 2)
# Fi = fisher.test(M_GS_l)  #  p-value = 0.0713
# ch = chisq.test(M_GS_l)   #  p-value = 0.0722



lft  =  tbl_stack(list(cnt_d30_GS, up_d30(d_elig_d30_GS), cnt_a5_GS, dw_a5(d_elig_GS) ))
rgt =  tbl_stack(list(cnt_d30_GB, up_d30(d_elig_d30_GB), cnt_a5_GB, dw_a5(d_elig_GB) )) 

t3_GS <-
lft |> as_gt() |>  
  rows_add( .list = rlang::list2("label" =  "GS  >5.5 yr earlier",
                                 "stat_1" = as.character(N_op_a5$N_opr[1]),
                                 "stat_2" = as.character(N_op_a5$N_opr[2])),
                  .before = 13 ) |> 
  rows_add( .n_empty = 1, .before = 13)
#  "p.value" =  round( Fi$p.value, 2)),  # <double>, next row

t3_GB <-  
rgt |> as_gt() |>  
  rows_add( .list = rlang::list2("label" =  "GB >5.5 yr earlier", 
                                 "stat_1" = as.character(N_op_a5$N_opr[3]),
                                 "stat_2" = as.character(N_op_a5$N_opr[4])),
            .before = 13 ) |> 
  rows_add( .n_empty = 1, .before = 13)

# t3_GS %>% opt_footnote_marks(marks = "letters") %>% gtsave("t3GS.docx")
# t3_GB %>% opt_footnote_marks(marks = "letters") %>% gtsave("t3GB.docx")

# as_gt %>% opt_footnote_marks(marks = "letters")
 


FT = tbl_merge(list(lft, rgt), tab_spanner = c("**Gastric sleeve**", "**Gastric bypass**"))

ft = FT |> as_gt() |>
  rows_add( .list = rlang::list2("label" =  "GB >5.5 yr earlier", 
                                 "stat_1_1" = as.character(N_op_a5$N_opr[1]),
                                 "stat_2_1" = as.character(N_op_a5$N_opr[2]),
                                 "stat_1_2" = as.character(N_op_a5$N_opr[3]),
                                 "stat_2_2" = as.character(N_op_a5$N_opr[4])),
                                               .before = 13) |>
  rows_add( .n_empty = 1, .before = 13)

# add Substitution missing data?  after last row

#    https://stackoverflow.com/questions/77069718/how-to-add-row-with-confidence-intervals-for-each-column-using-gtsummary
#  long term results, cohort: d_act_GS_nt6,  d_act_GB_nt6     

# ft |> opt_footnote_marks(marks = "letters") %>% gtsave("t3_begge.docx")

 
tbl_stack( list(
tbl_merge( list(
tbl_stack(list(cnt_d30_GS, up_d30(d_elig_d30_GS))), 
tbl_stack(list(cnt_d30_GB, up_d30(d_elig_d30_GB)))
)), cnt_a5)
)




  
s_GS =  up_d30(d_act_d30_GS)
 l_GS =  dw_a5(d_act_a5_GS)
s_GB =  up_d30(d_act_d30_GB)
 l_GB =  dw_a5(d_act_a5_GB)




# cnt_a5  =  d_act_nt6 %>% tbl_summary(by = trt, include = c(a5_fu))
cnt = d_elig %>% 
  tbl_summary(by = trt, include = c(a5_nt), label = a5_nt ~ "Followed up, 5 yr +- 6m")

cnt_a5  =  d_elig %>% filter(!is.na(a5_nt)) %>%
    tbl_summary(by = trt, include = c(a5_nt), label = a5_nt ~ "Followed up, 5 yr +- 6m")

  cnt_a5_GS  =  d_elig_GS %>% 
    filter(!is.na(a5_fu), 
           a5_dato_oppf - o_dato_op < 2007,
           a5_dato_oppf - o_dato_op > 1642 ) %>% # norm-tid?
    tbl_summary(by = trt, include = c(a5_fu), label = a5_fu ~ "Followed up, 5 yr +- 6m",
                statistic = all_dichotomous() ~ "{n}")
  
  cnt_a5_GB  =  d_elig_GB %>% 
    filter(!is.na(a5_fu), 
           a5_dato_oppf - o_dato_op < 2007,
           a5_dato_oppf - o_dato_op > 1642 ) %>% # norm-tid?
    tbl_summary(by = trt, include = c(a5_fu), label = a5_fu ~ "Followed up, 5 yr +- 6m",
                statistic = all_dichotomous() ~ "{n}")
  
  
  
  
cnt_a5 |> as_gt() |>  
  rows_add( .list = rlang::list2("label" =  "a5 opr", 
                                 "stat_1" = as.character(N_op_a5$N_opr[1]),
                                 "stat_2" = as.character(N_op_a5$N_opr[2]),
                                 "stat_3" = as.character(N_op_a5$N_opr[3]),
                                 "stat_4" = as.character(N_op_a5$N_opr[4])),
                                 .before = 1 ) |>
  rows_add( .list = rlang::list2("label" =  "% fu", 
                                 "stat_1" = as.character( pc1( fup_a5[1])),
                                 "stat_2" = as.character( pc1(fup_a5[2])),
                                 "stat_3" = as.character( pc1(fup_a5[3])),
                                 "stat_4" = as.character( pc1(fup_a5[4])) )) 

            
# cnt_act_a5  =  d_act_a5 %>% tbl_summary(by = trt, include = c(a5_fu))

tbl_stack(list(cnt_d30, cnt_a5))

tbl_stack(list(cnt_d30_GS, up_d30(d_elig_d30_GS)))

#tbl_stack(list(cnt_d30, up_d30(d_elig_d30), cnt_a5, dw_a5(d_act_a5) ))
#  only 1 col p-value 


# l_GS$table_body 
l_GS |> as_gt() |> 
  rows_add(   "label" = "potential 5 yr",
              "stat_1" = as.character( N_op_a5[1,2]),
              "stat_2" = as.character( N_op_a5[2,2]),
              "p.value" = NA ,   
              .before = 2 )  
 # --------------------------  Follow-up %:s   format

s_GS_gt  = s_GS |> as_gt() |> 
  rows_add( .list = rlang::list2(  "label" = "Potential 30 d follow-up",
                                   "stat_1" = as.character( N_op_d30[1,2]),
                                   "stat_2" = as.character( N_op_d30[2,2]),
                                   "p.value" = NA ),   
            .before = 1 )  |>
  rows_add( .list = rlang::list2(  "label" = "Follow-up %; 30 d",
                                   "stat_1" = as.character(round( 100*s_GS$df_by$n[1]/ N_op_d30[1,2], 1)),
                                   "stat_2" = as.character(round( 100*s_GS$df_by$n[2]/ N_op_d30[2,2], 1)),
                                   "p.value" = NA),   
            .before = 3 )

s_GB_gt  = s_GB |> as_gt() |> 
  rows_add( .list = rlang::list2(  "label" = "Potential 30 d follow-up",
                                   "stat_1" = as.character( N_op_d30[3,2]),
                                   "stat_2" = as.character( N_op_d30[4,2]),
                                   "p.value" = NA ),   
            .before = 1 )  |>
  rows_add( .list = rlang::list2(  "label" = "Follow-up %; 30 d",
                                   "stat_1" = as.character(round( 100*s_GB$df_by$n[1]/ N_op_d30[3,2], 1)),
                                   "stat_2" = as.character(round( 100*s_GB$df_by$n[2]/ N_op_d30[4,2], 1)),
                                   "p.value" = NA),   
            .before = 3 )


l_GS_gt  = l_GS |> as_gt() |> 
  rows_add( .list = rlang::list2(  "label" = "Potential 5 yr follow-up",
              "stat_1" = as.character( N_op_a5[1,2]),
              "stat_2" = as.character( N_op_a5[2,2]),
              "p.value" = NA ),   
              .before = 1 )  |>
  rows_add( .list = rlang::list2(  "label" = "Follow-up %; 5 yrs",
                                   "stat_1" = as.character(round( 100*l_GS$df_by$n[1]/ N_op_a5[1,2], 1)),
                                   "stat_2" = as.character(round( 100*l_GS$df_by$n[2]/ N_op_a5[2,2], 1)),
                                   "p.value" = 0.07479 ),   
            .before = 3 ) 

l_GB_gt  = l_GB |> as_gt() |> 
  rows_add( .list = rlang::list2(  "label" = "Potential 5 yr follow-up",
                                   "stat_1" = as.character( N_op_a5[3,2]),
                                   "stat_2" = as.character( N_op_a5[4,2]),
                                   "p.value" = NA ),   
            .before = 1 )  |>
  rows_add( .list = rlang::list2(  "label" = "Follow-up %; 5 yrs",
                                   "stat_1" = as.character(round( 100*l_GB$df_by$n[1]/ N_op_a5[3,2], 1)),
                                   "stat_2" = as.character(round( 100*l_GB$df_by$n[2]/ N_op_a5[4,2], 1)),
                                   "p.value" = 0.07479 ),   
            .before = 3 )


# M_GS = matrix(c(685, 676, 1222, 1066),
#               nrow = 2)
# fisher.test(M_GS)
#--


l_GB_gt  = l_GB |> as_gt() |> 
  rows_add( .list = rlang::list2(  "label" = "Potential 5 yr follow-up",
                                   "stat_1" = as.character( N_op_a5[3,2]),
                                   "stat_2" = as.character( N_op_a5[4,2]),
                                   "p.value" = NA ),   
            .before = 1 )  |>
  rows_add( .list = rlang::list2(  "label" = "Follow-up %; 5 yrs",
                                   "stat_1" = as.character(round( 100*l_GB$df_by$n[1]/ N_op_a5[3,2], 1)),
                                   "stat_2" = as.character(round( 100*l_GB$df_by$n[2]/ N_op_a5[4,2], 1)),
                                   "p.value" = 0.5624 ),   
            .before = 3 )  # |>
  # fmt_number(
  #   rows = 3,
  #   decimals = 2
  # )

# 
# M_GB = matrix(c(353, 791, 522, 1229),
#                          nrow = 2)
# fisher.test(M_GB)

# ------
tab3_GS =  tbl_stack(list(s_GS, l_GS))
tab3_GB =  tbl_stack(list(s_GB, l_GB))

M_GS_s = matrix(c(2178, 3217, 2353, 3392),
              nrow = 2)
fisher.test(M_GS_s)

## ------------------------------       GS
#                               |------------> insert rows only in   as_gt()
tab3_GS  |> as_gt() |> 
  rows_add( .list = rlang::list2(  "label" = "potential 30 d",
                                   "stat_1" = as.character( N_op_d30[1,2]),
                                   "stat_2" = as.character( N_op_d30[2,2]),
                                   "p.value" = NA ),   
            .before = 1 )  |>
  rows_add( .list = rlang::list2(  "label" = "follow-up %; 30 d",
                                   "stat_1" = as.character( s_GS$df_by$n[1]/ N_op_d30[1,2]),
                                   "stat_2" = as.character( s_GS$df_by$n[2]/ N_op_d30[2,2]),
                                   "p.value" = 0.5369 ),   
            .before = 2 ) |>
  rows_add( .list = rlang::list2(  "label" = "potential 5 yr",
                                   "stat_1" = as.character( N_op_a5[1,2]),
                                   "stat_2" = as.character( N_op_a5[2,2]),
                                   "p.value" = NA ),   
            .before = 13 )  |>
  rows_add( .list = rlang::list2(  "label" = "follow-up %; 5 yrs",
                                   "stat_1" = as.character( l_GS$df_by$n[1]/ N_op_a5[1,2]),
                                   "stat_2" = as.character( l_GS$df_by$n[2]/ N_op_a5[2,2]),
                                   "p.value" = 0.07479  ),   
            .before = 14 )  

tab3_GS  |> as_gt() |> 
  rows_add( .list = rlang::list2(  "label" = "potential 30 d",
                                   "stat_1" = as.character( N_op_d30[1,2]),
                                   "stat_2" = as.character( N_op_d30[2,2]),
                                   "p.value" = NA ),   
            .before = 1 )  |>
  rows_add( .list = rlang::list2(  "label" = "follow-up %; 30 d",
                                   "stat_1" = as.character( s_GS$df_by$n[1]/ N_op_d30[1,2]),
                                   "stat_2" = as.character( s_GS$df_by$n[2]/ N_op_d30[2,2]),
                                   "p.value" = 0.5369 ),   
            .before = 2 )


##-----------------------------------  GB

tab3_GB  |> as_gt() |> 
  rows_add( .list = rlang::list2(  "label" = "potential 30 d",
                                   "stat_1" = as.character( N_op_d30[3,2]),
                                   "stat_2" = as.character( N_op_d30[4,2]),
                                   "p.value" = NA ),   
            .before = 1 )  |>
  rows_add( .list = rlang::list2(  "label" = "follow-up %; 30 d",
                                   "stat_1" = as.character( s_GB$df_by$n[1]/ N_op_d30[3,2]),
                                   "stat_2" = as.character( s_GB$df_by$n[2]/ N_op_d30[4,2]),
                                   "p.value" = 0.7999 ),   
            .before = 2 ) |>
  rows_add( .list = rlang::list2(  "label" = "potential 5 yr",
                                   "stat_1" = as.character( N_op_a5[3,2]),
                                   "stat_2" = as.character( N_op_a5[4,2]),
                                   "p.value" = NA ),   
            .before = 13 )  |>
  rows_add( .list = rlang::list2(  "label" = "follow-up %; 5 yrs",
                                   "stat_1" = as.character( l_GB$df_by$n[1]/ N_op_a5[3,2]),
                                   "stat_2" = as.character( l_GB$df_by$n[2]/ N_op_a5[4,2]),
                                   "p.value" = 0.5624  ),   
            .before = 14 )  

# M_GB_s = matrix(c(1508, 1584, 3957, 4202),
#                 nrow = 2)
# fisher.test(M_GB_s)  #  p-value = 0.7999
# chisq.test(M_GB_s)   #  p-value = 0.8127
# 
# M_GB_l = matrix(c(353, 522, 791, 1229),
#                 nrow = 2)
# fisher.test(M_GB_l)  # p-value = 0.5624
# chisq.test(M_GB_l)  #  p-value = 0.5774
