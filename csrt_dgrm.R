library(consort)
# run Z_up, t1 first
##

# tbl3_j = tbl_stack(list(cnt_d30, up_d30(d_elig_d30 |> filter(u6_fu)), cnt_a5, dw_a5(d_a5_j )))


  d_cs <-  d_elig_d30 |> 
    mutate(arm = ifelse(o_preop_vektskole, "EPEP", "SPEP"),
   fow1 =  ifelse (!u6_fu & a5_nt, "Did attend to a5 Follow-up", 
                   ifelse( !u6_fu,"lost of FU (u6)", NA)),
   fow2 = ifelse(is.na(a5_nt), "Not yet", ifelse(a5_nt, NA, "Lost of FU (a5)")))
  

  ordrs = c( # p_pasientid = "Finished Followup",
              arm     = "Pre-operative program",
             fow1    = "Lost of Follow-up",
             p_pasientid = "30d Follow-up",
             fow2    = "Not evaluable",
             p_pasientid = "a5 Follow-up")
  
  
consort_plot( data = d_cs,
              orders =  ordrs, 
              allocation = "arm",
              side_box = c( "fow1", "fow2") 
              )


##
# d <- Dt |>
#   mutate(k_u6 = !is.na(u6_TWL),
#          k_a1 = !is.na(a1_TWL),
#          k_a2 = !is.na(a2_TWL),
#          k_a5 =  case_when( is.na(a5_TWL) ~ "a5 missing"))
# 
# ordr   = c(p_pasientid = "Total Records", 
#            st_12 = "1, 2- year follow-up status",
#            p_pasientid = "Final")
# 
# sb = c("st_12")
# 
# consort_plot(data = d_c,
#              orders = ordr,
#              side_box = sb)
# 
# # ------
# 
# ordr   = c(p_pasientid = "Total Records", 
#            exl1 = "EXCLUDED: ",
#            p_pasientid = "Total Records", 
#            fu1 = "Follow-up 1 yr",
#            p_pasientid = "Total Records", 
#            fu2 = "Follow-up 2 yr",
#            p_pasientid = "Total Records", 
#            fu12 = "Follow-up, both 1 yr AND 2 yr",
#            p_pasientid = "Final")
# 
# sb = c("exl1", "fu1", "fu2", "fu12")
# 
# consort_plot(data = d_c,
#              orders = ordr,
#              side_box = sb)
