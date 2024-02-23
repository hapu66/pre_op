#     Construct the lower part of T3 -------------------------------------------
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
      missing_text = "Missing data")  
}


# ---------------------- 5 yr linear model ------------------------------------  LME Models -----------------------------

m_TWL   <- lm(formula = a5_TWL ~ p_alder_v_op + Female + bmi_0 + o_opmetode + o_preop_vektskole + b_beh_diab + smoke,  data = d_elig |> filter(a5_nt))
summary(m_TWL)
rs_a5(d_elig |> filter(a5_nt))


d_a5_j = d_elig |> filter(a5_nt) |> 
  mutate(a5_TWL_j =  a5_TWL - o_sykehus |> map( just) |> unlist(),
         vtap = a5_TWL_j)
rs_a5(d_a5_j)

# ------
m_TWL_GS   <- lm(formula = a5_TWL ~ p_alder_v_op + Female + bmi_0         + o_preop_vektskole + b_beh_diab + smoke,  data = d_elig_GS |> filter(a5_nt))
summary(m_TWL_GS)

rs_a5(d_elig_GS |> filter(a5_nt))


d_a5_j_GS = d_elig_GS |> filter(a5_nt) |> 
  mutate(a5_TWL_j =  a5_TWL - o_sykehus |> map( just) |> unlist(),
         vtap = a5_TWL_j)
rs_a5(d_a5_j_GS)

# ------
m_TWL_GB   <- lm(formula = a5_TWL ~ p_alder_v_op + Female + bmi_0        + o_preop_vektskole + b_beh_diab + smoke,  data = d_elig_GB |> filter(a5_nt))
summary(m_TWL_GB)

rs_a5(d_elig_GB |> filter(a5_nt))


d_a5_j_GB = d_elig_GB |> filter(a5_nt) |> 
  mutate(a5_TWL_j =  a5_TWL - o_sykehus |> map( just) |> unlist(),
         vtap = a5_TWL_j)
rs_a5(d_a5_j_GB)


# ----------------------------------------------------------------------------   BMI 
# 
m_bmi0   <- lmer(formula = bmi_5a ~ p_alder_v_op + Female + bmi_0 + o_opmetode + o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  
                    data = d_elig |> filter(a5_nt))  #  
summary(m_bmi0)
bmi_REff  = ranef(m_bmi0)
bmi_RE_tbbl = as_tibble(bmi_REff)
library(lattice)
dotplot( ranef(m_bmi0))

#        ----------------------------- adjusting RE hospitals at a5
bmi_just = function(o_sykehus){
  bmi_RE_tbbl |> filter(grp == o_sykehus) |> pull(condval)
}


m_BMI  <- lm(formula = bmi_5a ~  p_alder_v_op + Female + bmi_0 + o_opmetode + o_preop_vektskole + b_beh_diab + smoke,  data = d_elig |> filter(a5_nt))
summary(m_BMI)

m_BMI_GS  <- lm(formula = bmi_5a ~  p_alder_v_op + Female + bmi_0  + o_preop_vektskole + b_beh_diab + smoke,  
                data = d_elig_GS |> filter(a5_nt))
summary(m_BMI_GS)

m_BMI_GB  <- lm(formula = bmi_5a ~  p_alder_v_op + Female + bmi_0  + o_preop_vektskole + b_beh_diab + smoke,  
                data = d_elig_GB |> filter(a5_nt))
summary(m_BMI_GB)

d_a5_bmi_j = d_elig |> filter(a5_nt) |> 
   mutate(bmi_5a_j =  bmi_5a - o_sykehus |> map( bmi_just) |> unlist(),
   vtap = bmi_5a_j)

m_BMI_j  <- lm(formula = bmi_5a_j ~  p_alder_v_op + Female + bmi_0 + o_opmetode + o_preop_vektskole + b_beh_diab + smoke,  data = d_a5_bmi_j)
summary(m_BMI)

m_BMI_GS_J  <- lm(formula = bmi_5a_j ~  p_alder_v_op + Female + bmi_0  + o_preop_vektskole + b_beh_diab + smoke,  
                data =  d_a5_bmi_j |> filter(o_opmetode == 6))
summary(m_BMI_GS)

m_BMI_GB_J  <- lm(formula = bmi_5a_j ~  p_alder_v_op + Female + bmi_0  + o_preop_vektskole + b_beh_diab + smoke,  
                data =  d_a5_bmi_j |> filter(o_opmetode == 1))
summary(m_BMI_GB)

#     Construct the lower part of T3 -------------------------------------------
bmi_a5 = function(tb) {tb |> 
    select(trt, vtap,  subst) |>   
    tbl_summary( 
      by = trt,
      type = list(vtap ~ "continuous", 
                  subst ~ "dichotomous"),    
      statistic = list( 
        vtap ~ "{mean} ({sd})",
        subst~ "{n} / {N} ({p}%)"),
      digits = all_continuous() ~ 1,
      label = list( vtap ~ "bmi a5", 
                    subst ~ "Substitution"),
      missing_text = "Missing data")  
}




bmi_a5(d_a5_bmi_j)
bmi_a5( d_a5_bmi_j |> filter(o_opmetode == 6))
bmi_a5( d_a5_bmi_j |> filter(o_opmetode == 1))


m_0   <- lmer(formula = a5_TWL ~ p_alder_v_op + Female + bmi_0 + o_opmetode + o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  
              data = d_elig |> filter(a5_nt))  #  
summary(m_0)
REff  = ranef(m_0)
RE_tbbl = as_tibble(REff)
library(lattice)
dotplot( ranef(m_0))


#        ----------------------------- adjusting RE hospitals at a5
just = function(o_sykehus){
  RE_tbbl |> filter(grp == o_sykehus) |> pull(condval)
}

d_a5_j = d_elig |> filter(a5_nt) |> 
  mutate(a5_TWL_j =  a5_TWL - o_sykehus |> map( just) |> unlist(),
         vtap = a5_TWL_j)
rs_a5(d_a5_j)


d_a5_GS_j = d_elig_GS |> filter(a5_nt) |> 
  mutate(a5_TWL_j =  a5_TWL - o_sykehus |> map( just) |> unlist(),
         vtap = a5_TWL_j)
rs_a5(d_a5_GS_j)


d_a5_GB_j = d_elig_GB |> filter(a5_nt) |> 
  mutate(a5_TWL_j =  a5_TWL - o_sykehus |> map( just) |> unlist(),
         vtap = a5_TWL_j)
rs_a5(d_a5_GB_j)

  ggplot(data =  d_elig |> filter(a5_nt), aes(x= p_alder_v_op, y= a5_TWL, group=o_opmetode, colour=o_opmetode)) + geom_point() + theme_minimal()
  ggplot(data =  d_a5_j |> filter(a5_nt), aes(x= p_alder_v_op, y= a5_TWL_j, group=o_opmetode, colour=o_opmetode)) + geom_point() + theme_minimal()
  
  ggplot(data =  d_a5_GS_j |> filter(a5_nt), aes(x= p_alder_v_op, y= a5_TWL_j, group=o_opmetode, colour=o_opmetode)) + geom_point() + theme_minimal()
  ggplot(data =  d_a5_GB_j |> filter(a5_nt), aes(x= p_alder_v_op, y= a5_TWL_j, group=o_opmetode, colour=o_opmetode)) + geom_point() + theme_minimal()
  
  
  ggplot(data = d_a5_bmi_j, aes(x= p_alder_v_op, y= bmi_5a, group=o_opmetode, colour=o_opmetode)) + geom_point() + theme_minimal()
  ggplot(data = d_a5_bmi_j, aes(x= p_alder_v_op, y= bmi_5a_j, group=o_opmetode, colour=o_opmetode)) + geom_point() + theme_minimal()


# -------------------------------------------------------  vekt  2024-02-23
 # 
m_vkt_0_GS <-  lm(formula = a5_ant_vekt ~ b_ant_vekt +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke  ,  # remove bmi_0
    data = d_elig_GS |> filter(a5_nt))  #   
 summary(m_vkt_0_GS) 
 
m_vkt_0_GB <-  lm(formula = a5_ant_vekt ~ b_ant_vekt +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke  ,  # remove bmi_0
   data = d_elig_GB |> filter(a5_nt))  #   
 summary(m_vkt_0_GB) 
# --- 
 
# -----------------------------------  random effects  
m_vkt_GS   <- lmer(formula = a5_ant_vekt ~ b_ant_vekt +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  # remove bmi_0
   data = d_elig_GS |> filter(a5_nt))  #  
  summary(m_vkt_GS)
  REff_vkt_GS  = ranef(m_vkt_GS)
  RE_tbbl_vkt_GS = as_tibble(REff_vkt_GS)
  
m_vkt_GB   <- lmer(formula = a5_ant_vekt ~ b_ant_vekt +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  # remove bmi_0
   data = d_elig_GB |> filter(a5_nt))  #  
  summary(m_vkt_GB)
  REff_vkt_GB  = ranef(m_vkt_GB)
  RE_tbbl_vkt_GB = as_tibble(REff_vkt_GB)
  
dotplot(ranef(m_vkt_GS))  
dotplot(ranef(m_vkt_GB))  

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

rs_a5(d_a5_vkt_GS_j)
  
#-- GB
d_a5_vkt_GB_j = d_elig_GB |> filter(a5_nt) |> 
  mutate(a5_vkt_j =  a5_ant_vekt - o_sykehus |> map(vkt_just_GB) |> unlist(),
         vtap = a5_vkt_j)

rs_a5(d_a5_vkt_GB_j)



#
# calculate TWL in two ways ---------------------------------------------------------------------
  da_GS = d_elig_GS |> filter(a5_nt)
  da_GB = d_elig_GB |> filter(a5_nt)
  
  mean(da_GS$a5_TWL) 
  (mean(da_GS$b_ant_vekt) - mean(da_GS$a5_ant_vekt))/mean(da_GS$b_ant_vekt)*100
  
  mean(da_GB$a5_TWL) 
  (mean(da_GB$b_ant_vekt) - mean(da_GB$a5_ant_vekt))/mean(da_GB$b_ant_vekt)*100
  

  # -------------------------------------------------------  bmi  2024-02-23 ----------------------------------------------------------------------
  # 
  m_bmi_0_GS <-  lm(formula = bmi_5a ~ bmi_0 +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke  ,  #  
                    data = d_elig_GS |> filter(a5_nt))  #   
  summary(m_bmi_0_GS) 
  
  m_bmi_0_GB <-  lm(formula = bmi_5a ~ bmi_0 +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke  ,  #  
                    data = d_elig_GB |> filter(a5_nt))  #   
  summary(m_bmi_0_GB) 
  # --- 
  
  # -----------------------------------  random effects  
  
  m_bmi_GS   <- lmer(formula = bmi_5a ~ bmi_0 +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  #  
                     data = d_elig_GS |> filter(a5_nt))  #  
  summary(m_bmi_GS)
  REff_bmi_GS  = ranef(m_bmi_GS)
  RE_tbbl_bmi_GS = as_tibble(REff_bmi_GS)
  
  m_bmi_GB   <- lmer(formula = bmi_5a ~ bmi_0 +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  #  
                     data = d_elig_GB |> filter(a5_nt))  #  
  summary(m_bmi_GB)
  REff_bmi_GB  = ranef(m_bmi_GB)
  RE_tbbl_bmi_GB = as_tibble(REff_bmi_GB)
  
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
  
  rs_a5(d_a5_bmi_GS_j)
  
  #-- GB
  d_a5_bmi_GB_j = d_elig_GB |> filter(a5_nt) |> 
    mutate(a5_bmi_j =  bmi_5a - o_sykehus |> map(bmi_just_GB) |> unlist(),
           vtap = a5_bmi_j)
  
  rs_a5(d_a5_bmi_GB_j)
  
  # -------------------------------------------------------  TWL  2024-02-23
  # 
  m_TWL_0_GS <-  lm(formula = a5_TWL ~ bmi_0 +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke  ,  #  
    data = d_elig_GS |> filter(a5_nt))  #   
  summary(m_TWL_0_GS) 
  
  m_TWL_0_GB <-  lm(formula = a5_TWL ~ bmi_0 +  p_alder_v_op + Female  + o_preop_vektskole + b_beh_diab + smoke  ,  #  
    data = d_elig_GB |> filter(a5_nt))  #   
  summary(m_TWL_0_GB) 
  # --- 
  
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
  
  rs_a5(d_a5_TWL_GS_j)
  
  #-- GB
  d_a5_TWL_GB_j = d_elig_GB |> filter(a5_nt) |> 
    mutate(a5_TWL_j =  a5_TWL - o_sykehus |> map(TWL_just_GB) |> unlist(),
   vtap = a5_TWL_j)
  
  rs_a5(d_a5_TWL_GB_j)
  
  