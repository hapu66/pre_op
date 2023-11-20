
library(lme4)

 
m1 =  lmer(a5_TWL ~ p_alder_v_op + Female + o_preop_vektskole + (1|o_sykehus), data = d_act_a5)
summary(m1)
ranef(m1)