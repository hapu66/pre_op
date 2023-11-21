
library(lme4)

 
m_WL5 =  lmer(a5_TWL ~  p_alder_v_op + Female + o_preop_vektskole + (1|o_sykehus), data = d_act_a5)
summary(m_WL5)
ranef(m_WL5)

m_WL_S =  lmer(a5_TWL ~ vt_pr + p_alder_v_op + Female + o_preop_vektskole + (1|o_sykehus), data = d_act_a5_GS)
summary(m_WL_S)
ranef(m_WL_S)

m_WL_B =  lmer(a5_TWL ~ vt_pr + p_alder_v_op + Female + o_preop_vektskole + (1|o_sykehus), data = d_act_a5_GB)
summary(m_WL_B)
ranef(m_WL_B)



m_WL5 =  lmer(a5_TWL ~  p_alder_v_op + Female +  o_preop_vektskole + (1|o_sykehus), data = d_act_a5_GS)
summary(m_WL5)
ranef(m_WL5)



m_fu =  lmer(a5_fu  ~  p_alder_v_op + Female + o_preop_vektskole + (1|o_sykehus), data = d_act_a5_GS)
summary(m_fu)
ranef(m_fu)