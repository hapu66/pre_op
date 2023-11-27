
library(lme4)
library(lmerTest)

simp =  lmer(a5_TWL ~  p_alder_v_op + o_preop_vektskole + (1|o_sykehus), data = d_act_a5)
d_act_a5$fixed <- predict(simp, re.form=NA)
d_act_a5$rand <- predict(simp)
 
gr0 = ggplot(data = d_act_a5, aes(x=p_alder_v_op, y=a5_TWL))
gr1 = gr0 + geom_point()

gr1
gr1<-gr1+geom_line(aes(x=p_alder_v_op,y=fixed),colour=2, lwd=1) 
gr1<-gr1+geom_line(aes(x=p_alder_v_op,y=rand),colour=4, lwd=1) 
gr1<-gr1+labs(y = "%TWL", x="Opr.age", title="Fixed and random effects")
gr1+facet_wrap("o_sykehus") 

# -----------

dee = d_act_a5_GS |> filter(!is.na(vt_pr))  # 8 st vt_pr  missing

cplx =  lmer(a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0 + o_preop_vektskole + (1|o_sykehus), data = dee)
dee$fixed <- predict(cplx, re.form=NA)
dee$rand <- predict(cplx)

gr0 = ggplot(data = dee, aes(x=p_alder_v_op, y=a5_TWL))
gr1 = gr0 + geom_point()

gr1
gr1<-gr1+geom_line(aes(x=p_alder_v_op, y=fixed),colour=2, lwd=1) 
gr1<-gr1+geom_line(aes(x=p_alder_v_op, y=rand),colour=4, lwd=1) 
gr1<-gr1+labs(y = "%TWL", x="Opr.age", title="Fixed and random effects")
gr1+facet_wrap("o_sykehus") 
# ----------------------------------------------

pp_sh = function(df){
  dee = df  |> filter( !is.na(vt_pr), o_opmetode %in% c(1,6))
  ch_nm = deparse(substitute(df))
  sts  = ifelse(str_detect(ch_nm, "GS"),"Sleeve", 
                ifelse(str_detect(ch_nm, "GB"),"Bypass", "not GS nor GB!!"))
  mg = paste0(sts,  ", 5 yr WL(%) vs. opr.age")
  
  model1 <- lmer(formula = a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0 + # o_opmetode + o_preop_vektskole +
                   o_opmetode*o_preop_vektskole+ 
                   bmi_0*o_preop_vektskole+
                  (1|o_sykehus), 
                 data    = dee)
  
  model2 <- lmer(formula = a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0   + 
                   o_preop_vektskole + (1|o_sykehus), 
                 data    = dee)
  
  model3 <- lmer(formula = a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0   + 
                   o_preop_vektskole + b_beh_depr + b_beh_diab + smoke +  (1|o_sykehus), 
                 data    = dee)
  
M =   summary(model3)  
  dee$fixed <- predict(model3, re.form=NA)
  dee$rand <- predict(model3)

print(M)     
print(ranef(model3)) 
  
G = ggplot(data = dee,
       aes(x = p_alder_v_op,
           y = a5_TWL,
           col = as.factor( o_sykehus)))+
  viridis::scale_color_viridis(discrete = TRUE)+
  geom_point(size = 1.6, alpha = 0.8) +
##  geom_smooth(method = lm,              col = "black",              size = .5,              alpha = 0.8)+
  theme_minimal()
#  scale_color_gradientn(colours = rainbow(100))+
G<-G+geom_smooth(method = lm, aes(x=p_alder_v_op, y=fixed),colour=4, lwd=1) 
G<-G+geom_smooth(method = lm, aes(x=p_alder_v_op, y=rand),colour= 2, lwd=1) 

  G = G+labs(title = mg,
       subtitle = "blue line: fixed effects, red line: random effect",
       xlab = "Age at operation", ylab = " 5 years %TWL", col ="Hospital" ) #+    scale_fill_discrete(name = )
  
G + facet_wrap("o_sykehus") 
}

pp_sh(d_act_a5)
pp_sh(d_act_a5_GS)
pp_sh(d_act_a5_GB)


mdl  <- lmer(formula = a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0   + 
                       o_preop_vektskole + b_beh_diab + b_beh_depr + smoke +(1|o_sykehus),   
                       data    = d_act_a5)
summary(mdl)
RE = ranef(mdl)

mdl_2  <- lmer(formula = a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0   + 
                        o_preop_vektskole + b_beh_diab + smoke +(1|o_sykehus),  #    UTEN + b_beh_depr
             data    = d_act_a5)
summary(mdl_2)
RE2 = ranef(mdl_2)



RE_tbbl = as_tibble(RE)
RE2_tbbl = as_tibble(RE2)


# d_act_a5 %>% left_join(RE_tbbl, by= c("o_sykkehus"=  "grp"))
  
# plot(mdl, sqrt(abs(resid(.))) ~ fitted(.),   type = c("p", "smooth"))


just = function(o_sykehus){
  RE2_tbbl |> filter(grp == o_sykehus) |> pull(condval)
}

# > just("Vestre Viken HF")
# numeric(0)
# > just("Helse Bergen HF")
# [1] 2.065264 
d_act_a5_j =  d_act_a5 |> filter(o_sykehus != "Vestre Viken HF") |> mutate(a5_TWL_j =  a5_TWL - o_sykehus |> map( just) |> unlist(),
                                                                           vtap = a5_TWL_j)

epep_j =   tbl_stack(list(cnt_d30, up_d30(d_act_d30), cnt_a5, dw_a5(d_act_a5_j)))

EP = epep_j |>  as_gt() |>  
  rows_add( .list = rlang::list2("label" =  "Opr.  >5.5 yr earlier",
                                 "stat_1" = as.character(N_op_a5$N_opr[1]),
                                 "stat_2" = as.character(N_op_a5$N_opr[2])),
            .before = 9 ) |> 
  rows_add( .n_empty = 1, .before = 9)



EP  %>% opt_footnote_marks(marks = "letters") %>% gtsave("Table3c.docx")


model1 <- lmer(formula = a5_TWL ~ vt_pr + p_alder_v_op + Female + bmi_0 + o_preop_vektskole + (1 |o_sykehus), 
               data    = d_act_a5_GS)
summary(model1)

dee = d_act_a5_GS  |> filter( !is.na(vt_pr))
dee$fixed <- predict(model1, re.form=NA)
dee$rand <- predict(model1)


plot(fitted(model1), resid(model1, type = "pearson"))  # this will create the plot
abline(0,0, col="red")

qqnorm(resid(model1)) 
qqline(resid(model1), col = "red") # add a perfect fit line

 qqnorm(ranef(model1)$o_sykehus[,1] )
 qqline(ranef(model1)$o_sykehus[,1], col = "red")

 qqnorm(ranef(model1)$o_sykehus[,2] )
 qqline(ranef(model1)$o_sykehus[,2], col = "red")
 

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