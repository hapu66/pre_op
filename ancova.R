# m_0   <- lmer(formula = a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0   + 
#                 o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  
#               data = d_elig |> filter(a5_nt))  # d_act_a5 has NOT -5.5år -filter
# summary(m_0)
# REff  = ranef(m_0)
# RE_tbbl = as_tibble(REff)
# 

dt = d_elig |> filter(a5_nt)

# generalized model
md_gen = lm(a5_TWL ~ vt_pr * o_sykehus + p_alder_v_op  * o_sykehus +
    Female * o_sykehus+ bmi_0 * o_sykehus + o_preop_vektskole * o_sykehus +
   b_beh_diab * o_sykehus + smoke * o_sykehus, data = dt) 

md_gen2 = lm(a5_TWL ~ vt_pr* o_sykehus + p_alder_v_op  +
    Female + bmi_0* o_sykehus + o_preop_vektskole  +
    b_beh_diab + smoke , data = dt) 

#ancova
md_ancova  = lm(a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0 + 
    o_preop_vektskole + b_beh_diab + smoke + o_sykehus, data = dt)

# reduced model
md_red = lm(a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0 + 
    o_preop_vektskole + b_beh_diab + smoke  , data = dt)

summary(md_gen)
summary(md_gen2)
summary(md_ancova)
summary(md_red)

anova(md_gen, md_ancova)
anova(md_gen2, md_ancova)

anova(md_red, md_ancova)

#generalized model
# model.1 = lm (Pulse ~ Temp + Species + Temp:Species, data = Data)

#ancova
# model.2 = lm (Pulse ~ Temp + Species, data = Data)

#reduced regression model
# model.3 = lm (Pulse ~ Temp, data = Data)



