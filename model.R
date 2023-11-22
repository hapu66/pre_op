
library(lme4)
library(lmerTest)

simp =  lmer(a5_TWL ~  p_alder_v_op+ o_preop_vektskole + (1|o_sykehus), data = d_act_a5)
d_act_a5$fixed <- predict(simp, re.form=NA)
d_act_a5$rand <- predict(simp)
 
gr0 = ggplot(data = d_act_a5, aes(x=p_alder_v_op, y=a5_TWL))
gr1 = gr0 + geom_point()

gr1
gr1<-gr1+geom_line(aes(x=p_alder_v_op,y=fixed),colour=2, lwd=1) 
gr1<-gr1+geom_line(aes(x=p_alder_v_op,y=rand),colour=4, lwd=1) 
gr1<-gr1+labs(y = ylabel,x=xlabel,title="Fixed and random effects")
gr1+facet_wrap("o_sykehus") 


m_WL5 =  lmer(a5_TWL ~  p_alder_v_op + Female + o_preop_vektskole + (1|o_sykehus), data = d_act_a5)
summary(m_WL5)
ranef(m_WL5)

m_WL_S =  lmer(a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0 + o_preop_vektskole + 
                 (1|o_sykehus), data = d_act_a5_GS)
m_WL_S

summary(m_WL_S)
ranef(m_WL_S)

m_WL_S$fixed <- predict(m_WL_S, re.form=NA)
m_WL_S$rand <-  predict(m_WL_S)
                      
Whales$fixed<-predict(intercept.mod,re.form=NA)
Whales$rand<-predict(intercept.mod)

#  -------
ANCOVA <- lm(a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0 + o_preop_vektskole + 
                o_sykehus , data = d_act_a5_GS)

par(mfrow = c(2,2))
plot(ANCOVA)
par(mfrow  =c(1,1))


ANCOVA2 <- aov(a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0 + o_preop_vektskole + 
               o_sykehus , data = d_act_a5_GS)
summary(ANCOVA2)

#

intercept.mod<-lmer(X15N~Age+(1|Whale),data=Whales)
intercept.mod

anova(intercept.mod)


Whales$fixed<-predict(intercept.mod,re.form=NA)
Whales$rand<-predict(intercept.mod)
g0<-ggplot(Whales,aes(x=Age,y=X15N))
g1<-g0+geom_point()
g1<-g1+geom_line(aes(x=Age,y=fixed),colour=2,lwd=1) 
g1<-g1+geom_line(aes(x=Age,y=rand),colour=4,lwd=1) 
g1<-g1+labs(y = ylabel,x=xlabel,title="Fixed and random effects")
g1+facet_wrap("Whale") 
# ----------------------------------------------

d_act_a5_GS$Female =            as.factor(d_act_a5_GS$Female)
d_act_a5_GS$o_preop_vektskole = as.factor(d_act_a5_GS$o_preop_vektskole)

m_WL_S =  lmer(a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0 + o_preop_vektskole + 
                 (1|o_sykehus), data = d_act_a5_GS)

d_act_a5_GS |> mutate( fixed = predict(m_WL_S, re.form=NA))



m_WL_B =  lmer(a5_TWL ~ vt_pr + p_alder_v_op + Female + o_preop_vektskole + (1|o_sykehus), data = d_act_a5_GB)
summary(m_WL_B)
ranef(m_WL_B)



m_WL5 =  lmer(a5_TWL ~  p_alder_v_op + Female +  o_preop_vektskole + (1|o_sykehus), data = d_act_a5_GS)
summary(m_WL5)
ranef(m_WL5)



m_fu =  lmer(a5_fu  ~  p_alder_v_op + Female + o_preop_vektskole + (1|o_sykehus), data = d_act_a5_GS)
summary(m_fu)
ranef(m_fu)