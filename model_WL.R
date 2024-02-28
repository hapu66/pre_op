#     Report results for each weight measure -------------------------------------------
rs_a5 = function(tb) {tb |> 
    select(trt, vtap,  subst) |>   
    tbl_summary( 
      by = trt,
      type = list(vtap ~ "continuous", 
                  subst ~ "dichotomous"),    
      statistic = list( 
        vtap ~ "{mean} ({sd})",
        subst~ "{n} / {N} ({p}%)"),
      digits = all_continuous() ~ 1,
      label = list( vtap ~ "weight measure", 
                    subst ~ "Substitution"),
      missing_text = "Missing data")  |> add_difference(  pvalue_fun = ~style_pvalue(.x, digits = 2))
}

# -------------------------------------------------------  TWL  2024-02-26 -----------------------------------------------------
# 
m_TWL_0_GS <-  lm(formula = a5_TWL ~ bmi_0 +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke  ,  #  
                  data = d_elig_GS |> filter(a5_nt))  #   
summary(m_TWL_0_GS) 

m_TWL_0_GB <-  lm(formula = a5_TWL ~ bmi_0 +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke  ,  #  
                  data = d_elig_GB |> filter(a5_nt))  #   
summary(m_TWL_0_GB) 
  
# -----------------------------------  random effects  

m_TWL_GS   <- lmer(formula = a5_TWL ~ bmi_0 +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  #  
                   data = d_elig_GS |> filter(a5_nt))  #  
summary(m_TWL_GS)
REff_TWL_GS  = ranef(m_TWL_GS)
RE_tbbl_TWL_GS = as_tibble(REff_TWL_GS)

dotplot( ranef(m_TWL_GS))

m_TWL_GB   <- lmer(formula = a5_TWL ~ bmi_0 +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  #  
                   data = d_elig_GB |> filter(a5_nt))  #  
summary(m_TWL_GB)
REff_TWL_GB  = ranef(m_TWL_GB)
RE_tbbl_TWL_GB = as_tibble(REff_TWL_GB)

dotplot( ranef(m_TWL_GB))

#        ----------------------------- weight adjusting RE hospitals at a5 
TWL_just_GS = function(o_sykehus){
  RE_tbbl_TWL_GS |> filter(grp == o_sykehus) |> pull(condval)
}

TWL_just_GB = function(o_sykehus){
  RE_tbbl_TWL_GB |> filter(grp == o_sykehus) |> pull(condval)
}

d_a5_TWL_GS_j = d_elig_GS |> filter(a5_nt) |> 
  mutate(a5_TWL_j =  a5_TWL - o_sykehus |> map(TWL_just_GS) |> unlist(),
         vtap = a5_TWL_j)

rs_a5(d_a5_TWL_GS_j)   # ---------------------- TWL_GS_j 

# -- GB
d_a5_TWL_GB_j = d_elig_GB |> filter(a5_nt) |> 
  mutate(a5_TWL_j =  a5_TWL - o_sykehus |> map(TWL_just_GB) |> unlist(),
         vtap = a5_TWL_j)

rs_a5(d_a5_TWL_GB_j)   # ---------------------- TWL_GB_j



# -------------------------------------------------------  bmi  2024-02-26 ----------------------------------------------------------
# 
m_bmi_0_GS <-  lm(formula = bmi_5a ~ bmi_0 +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke  ,  #  
                  data = d_elig_GS |> filter(a5_nt))  #   
summary(m_bmi_0_GS) 

m_bmi_0_GB <-  lm(formula = bmi_5a ~ bmi_0 +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke  ,  #  
                  data = d_elig_GB |> filter(a5_nt))  #   
summary(m_bmi_0_GB) 

# -----------------------------------  random effects  

m_bmi_GS   <- lmer(formula = bmi_5a ~ bmi_0 +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  #  
                   data = d_elig_GS |> filter(a5_nt))  #  
summary(m_bmi_GS)
REff_bmi_GS  = ranef(m_bmi_GS)
RE_tbbl_bmi_GS = as_tibble(REff_bmi_GS)

dotplot( ranef(m_bmi_GS))

m_bmi_GB   <- lmer(formula = bmi_5a ~ bmi_0 +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  #  
                   data = d_elig_GB |> filter(a5_nt))  #  
summary(m_bmi_GB)
REff_bmi_GB  = ranef(m_bmi_GB)
RE_tbbl_bmi_GB = as_tibble(REff_bmi_GB)

dotplot( ranef(m_bmi_GB))

#        ----------------------------- weight adjusting RE hospitals at a5 
bmi_just_GS = function(o_sykehus){
  RE_tbbl_bmi_GS |> filter(grp == o_sykehus) |> pull(condval)
}

bmi_just_GB = function(o_sykehus){
  RE_tbbl_bmi_GB |> filter(grp == o_sykehus) |> pull(condval)
}

d_a5_bmi_GS_j = d_elig_GS |> filter(a5_nt) |> 
  mutate(a5_bmi_j =  bmi_5a - o_sykehus |> map(bmi_just_GS) |> unlist(),
         vtap = a5_bmi_j)

rs_a5(d_a5_bmi_GS_j)   # ---------------------- bmi_GS_j 

# -- GB
d_a5_bmi_GB_j = d_elig_GB |> filter(a5_nt) |> 
  mutate(a5_bmi_j =  bmi_5a - o_sykehus |> map(bmi_just_GB) |> unlist(),
         vtap = a5_bmi_j)

rs_a5(d_a5_bmi_GB_j)   # ---------------------- bmi_GB_j



# -------------------------------------------------------  vekt  2024-02-26 ----------------------------------------------------------
# 
m_vkt_0_GS <-  lm(formula = a5_ant_vekt ~ b_ant_vekt +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke  ,  #  
                  data = d_elig_GS |> filter(a5_nt))  #   
summary(m_vkt_0_GS) 

m_vkt_0_GB <-  lm(formula = a5_ant_vekt ~ b_ant_vekt +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke  ,  #  
                  data = d_elig_GB |> filter(a5_nt))  #   
summary(m_vkt_0_GB) 

# -----------------------------------  random effects  

m_vkt_GS   <- lmer(formula = a5_ant_vekt ~ b_ant_vekt +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  #  
                   data = d_elig_GS |> filter(a5_nt))  #  
summary(m_vkt_GS)
REff_vkt_GS  = ranef(m_vkt_GS)
RE_tbbl_vkt_GS = as_tibble(REff_vkt_GS)

dotplot( ranef(m_vkt_GS))

m_vkt_GB   <- lmer(formula = a5_ant_vekt ~ b_ant_vekt +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  #  
                   data = d_elig_GB |> filter(a5_nt))  #  
summary(m_vkt_GB)
REff_vkt_GB  = ranef(m_vkt_GB)
RE_tbbl_vkt_GB = as_tibble(REff_vkt_GB)

dotplot( ranef(m_vkt_GB))

#        ----------------------------- weight adjusting RE hospitals at a5 
vkt_just_GS = function(o_sykehus){
  RE_tbbl_vkt_GS |> filter(grp == o_sykehus) |> pull(condval)
}

vkt_just_GB = function(o_sykehus){
  RE_tbbl_vkt_GB |> filter(grp == o_sykehus) |> pull(condval)
}

d_a5_vkt_GS_j = d_elig_GS |> filter(a5_nt) |> 
  mutate(a5_vkt_j =  a5_ant_vekt - o_sykehus |> map(vkt_just_GS) |> unlist(),
         vtap = a5_vkt_j)

rs_a5(d_a5_vkt_GS_j)   # ---------------------- vkt_GS_j 

# -- GB
d_a5_vkt_GB_j = d_elig_GB |> filter(a5_nt) |> 
  mutate(a5_vkt_j =  a5_ant_vekt - o_sykehus |> map(vkt_just_GB) |> unlist(),
         vtap = a5_vkt_j)

rs_a5(d_a5_vkt_GB_j)   # ---------------------- bmi_GB_j


  d_elig_GS |> filter(a5_nt) |> pull(b_ant_vekt) |>  mean()  #[1] 122.664
  d_elig_GB |> filter(a5_nt) |> pull(b_ant_vekt) |> mean()  #[1] 125.0076
 
  d_elig_GS |> filter(a5_nt, o_preop_vektskole==0) |> pull(b_ant_vekt) |>  mean()  #[1] 124.1124
  d_elig_GS |> filter(a5_nt, o_preop_vektskole==1) |> pull(b_ant_vekt) |>  mean()  #[1] 119.8012
  
  d_elig_GB |> filter(a5_nt, o_preop_vektskole==0) |> pull(b_ant_vekt) |>  mean()  #[1] 124.9199
  d_elig_GB |> filter(a5_nt, o_preop_vektskole==1) |> pull(b_ant_vekt) |>  mean()  #[1] 125.0687
  
  d_elig_GS |>filter(a5_nt) |> group_by(o_preop_vektskole) |> summarise(n(),  mean(b_ant_vekt))
  d_elig_GB |>filter(a5_nt) |> group_by(o_preop_vektskole) |> summarise(n(),  mean(b_ant_vekt))
  
###  --------------------------------------------------  initial weight?  
d_a5_vkt_GS_d = d_elig_GS |> filter(a5_nt) |> 
    mutate(a5_vkt_j =  a5_ant_vekt - o_sykehus |> map(vkt_just_GS) |> unlist(),
           vtap = -a5_vkt_j + b_ant_vekt)
  
  rs_a5(d_a5_vkt_GS_d)   # ---------------------- vkt_GS_d 
  
  # -- GB
d_a5_vkt_GB_d = d_elig_GB |> filter(a5_nt) |> 
    mutate(a5_vkt_j =  a5_ant_vekt - o_sykehus |> map(vkt_just_GB) |> unlist(),
           vtap = -a5_vkt_j + b_ant_vekt)
  
  rs_a5(d_a5_vkt_GB_d)   # ---------------------- bmi_GB_d
  
  

#
# calculate TWL in two ways ---------------------------------------------------------------------

  da_GS = d_elig_GS |> filter(a5_nt)
  da_GB = d_elig_GB |> filter(a5_nt)
  
  mean(da_GS$a5_TWL) 
  (mean(da_GS$b_ant_vekt) - mean(da_GS$a5_ant_vekt))/mean(da_GS$b_ant_vekt)*100
  
  mean(da_GB$a5_TWL) 
  (mean(da_GB$b_ant_vekt) - mean(da_GB$a5_ant_vekt))/mean(da_GB$b_ant_vekt)*100
  

#  plot weight measures  -----------------------------------------------------
#
  
  d_elig_GS |> ggplot(aes(x=bmi_0, y=a5_TWL))  + geom_point() +
    geom_abline(intercept =  m_TWL_0_GS$coefficients[1], 
                slope = m_TWL_0_GS$coefficients[2],
                colour ="red") + ggtitle("GS, TWL vs bmi_0+..") +
    theme_minimal()
  
  d_elig_GS |> ggplot(aes(x= bmi_0, y =bmi_5a))  + geom_point() +  
    geom_abline(intercept =  m_bmi_0_GS$coefficients[1], 
               slope = m_bmi_0_GS$coefficients[2],
               colour ="red") + ggtitle("GS, bmi_a5 vs bmi_0+..") +
    theme_minimal()
    
  d_elig_GS |> ggplot(aes(x= b_ant_vekt, y = a5_ant_vekt))  + geom_point() +
    geom_abline(intercept =  m_vkt_0_GS$coefficients[1], 
                slope = m_vkt_0_GS$coefficients[2],
                colour ="red") + ggtitle("GS, a5_ant_vekt vs b_ant_vekt +..") +
    theme_minimal()
  
    
  d_elig_GB |> ggplot(aes(x=bmi_0, y=a5_TWL))  + geom_point() +
    geom_abline(intercept =  m_TWL_0_GB$coefficients[1], 
                slope = m_TWL_0_GB$coefficients[2],
                colour ="red") +ggtitle("GB, TWL vs bmi_0 +..") +
    theme_minimal()
  
  d_elig_GB |> ggplot(aes(x= bmi_0, y =bmi_5a))  + geom_point() +  
    geom_abline(intercept =  m_bmi_0_GB$coefficients[1], 
                slope = m_bmi_0_GB$coefficients[2],
                colour ="red") +ggtitle("GB, bmi_a5 vs bmi_0 +..") +
    theme_minimal()
  
  d_elig_GB |> ggplot(aes(x= b_ant_vekt, y = a5_ant_vekt))  + geom_point() +
    geom_abline(intercept =  m_vkt_0_GB$coefficients[1], 
                slope = m_vkt_0_GB$coefficients[2],
                colour ="red") +ggtitle("GB, a5_ant_vekt vs b_ant_vekt +..") +
    theme_minimal()
  
  
  #
  #
  ggplot(data =  d_elig |> filter(a5_nt), aes(x= p_alder_v_op, y= a5_TWL, group=o_opmetode, colour=o_opmetode)) + geom_point() + theme_minimal()
  ggplot(data =  d_a5_j |> filter(a5_nt), aes(x= p_alder_v_op, y= a5_TWL_j, group=o_opmetode, colour=o_opmetode)) + geom_point() + theme_minimal()
  
  ggplot(data =  d_a5_vkt_GS_j |> filter(a5_nt), aes(x= p_alder_v_op, y= a5_vkt_j, group=o_opmetode, colour=o_opmetode)) + geom_point() + theme_minimal()
  ggplot(data =  d_a5_vkt_GB_j |> filter(a5_nt), aes(x= p_alder_v_op, y= a5_vkt_j, group=o_opmetode, colour=o_opmetode)) + geom_point() + theme_minimal()
  
  ggplot(data = d_a5_bmi_GS_j, aes(x= p_alder_v_op, y= bmi_5a, group=o_opmetode, colour=o_opmetode)) + geom_point() + theme_minimal()
  ggplot(data = d_a5_bmi_GS_j, aes(x= p_alder_v_op, y= a5_bmi_j, group=o_opmetode, colour=o_opmetode)) + geom_point() + theme_minimal()
  
  
  