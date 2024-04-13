library(MatchIt)

d_act_a5_GS |> 
  group_by(o_preop_vektskole) |>
  summarise(TWL = mean(a5_TWL), sd = sd(a5_TWL), se = sd/n() )

with(d_act_a5_GS, t.test(a5_TWL ~ o_preop_vektskole))
with(d_act_a5_GS, t.test(a5_ant_vekt ~ o_preop_vektskole))

with(d_act_a5_GS, t.test(b_ant_vekt ~ o_preop_vektskole))
# mean in group 0 mean in group 1  ##  p-value = 0.0002509
# 123.9106        120.2642 


# covariates --------------------------------------------------------------
vekt_cov <- c('o_preop_vektskole', 'b_ant_vekt', 'p_alder_v_op', 
              'Female', 'b_beh_diab', 'smoke')

d_act_a5_GS |>
  group_by(o_preop_vektskole) |>
  select(one_of(vekt_cov)) |>
  summarise_all(list(~ mean(., na.rm = T)))

lapply(vekt_cov[-1], function(v) {
  t.test(d_act_a5_GS[[v]] ~ d_act_a5_GS[['o_preop_vektskole']])
})

# -------------------------------------------------------------- PSM model
d_act_a5_GS =   d_act_a5_GS |> mutate(a5_WL =   b_ant_vekt - a5_ant_vekt)

m_ps <- glm(formula = o_preop_vektskole ~ a5_WL + p_alder_v_op + Female + 
                      + b_beh_diab + smoke,
            family = binomial(), 
            data = d_act_a5_GS)
summary(m_ps) 

#  Call:
#  lm(formula = a5_ant_vekt ~ b_ant_vekt + p_alder_v_op + Female + 
#       o_preop_vektskole + b_beh_diab + smoke, data = filter(d_elig_GS, a5_nt)                                                             a5_nt))


#  Using this model, we can now calculate the propensity score for each patient. 
# It is simply the patient’s predicted probability of 
#                                                being Treated   (o_preop_vektskole), 
# given the estimates from the logit model. Below, I calculate this propensity score using 
# predict() and create a tibble that has the propensity score as well as the 
# patient’s actual treatment status.


prs_df <- tibble(pr_score = predict(m_ps, type = "response"),
                     o_preop_vektskole = m_ps$model$o_preop_vektskole)
head(prs_df)
plot()


labs <- paste("Actual school type attended:", c("SPEP", "EPEP"))
prs_df %>%
  mutate(o_preop_vektskole = ifelse(o_preop_vektskole == 1, labs[2], labs[1])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(fill ="lightgreen", color = "black") +
  facet_wrap(~o_preop_vektskole) +
  xlab("Probability of going to EPEP school") +
  theme_minimal()  # 

# Matching algorithm

vekt_nomiss <- d_act_a5_GS %>%  # MatchIt does not allow missing values
  select(a5_WL, one_of(vekt_cov)) %>%
  na.omit()

mod_match <- matchit(formula =  o_preop_vektskole ~ a5_WL + p_alder_v_op +
                                        Female + b_beh_diab + smoke,
                     method = "nearest", 
                     data = vekt_nomiss)

summary(mod_match)
plot(mod_match)


dta_m <- match.data(mod_match)
dim(dta_m)  #[1] 1340   10                     1340 = 670*2

with(dta_m, t.test(a5_WL ~ o_preop_vektskole))


# use TWL instead -----------------------------------------------------------
TWL_nomiss <- d_act_a5_GS %>%  # MatchIt does not allow missing values
  select(a5_TWL, one_of(vekt_cov)) %>%
  na.omit()

TWL_match <- matchit(formula =  o_preop_vektskole ~ a5_TWL + p_alder_v_op +
                       Female + b_beh_diab + smoke,
                     method = "nearest", 
                     data = TWL_nomiss)

summary(TWL_match)
plot(TWL_match)


dta_m <- match.data(TWL_match)
dim(dta_m)  #[1] 1340   10                     1340 = 670*2

with(dta_m, t.test(a5_TWL ~ o_preop_vektskole))
# ----



fn_test <- function(dta, variable){
  dta$o_preop_vektskole <- as.factor(dta$o_preop_vektskole)
   support <- c(min(dta[[variable]]), max(dta[[variable]]))
  # dta[[variable]] 
  #  support
   ggplot(dta, aes(x = distance, y = variable, color = o_preop_vektskole)) +
     geom_point(alpha = 0.2, size = 1.3) +
     geom_smooth(method = "loess", se = F) +
     xlab("Propensity score") +
     ylab(variable) +
     theme_bw() +
     ylim(support)
}


fn_bal <- function(dta, variable) {
#  dta$variable <- dta[, variable]
   dta$o_preop_vektskole <- as.factor(dta$o_preop_vektskole)
  support <- c(min(dta[[variable]]), max(dta[[variable]]))
                   
  ggplot(dta, aes(x = distance, y = variable, color = o_preop_vektskole)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}
#  ----------------------- 1 case
dta = dta_m
variable = "p_alder_v_op"

support <- c(min(dta[[variable]]), max(dta[[variable]]))

ggplot(dta, aes(x = distance, y = p_alder_v_op, color = o_preop_vektskole)) +
  geom_point(alpha = 0.2, size = 1.3) +
  geom_smooth(method = "loess", se = F) +
  xlab("Propensity score") +
  ylab(variable) +
  theme_bw() + theme(legend.position = "none") +
  ylim(support)

ggplot(dta, aes(x = distance, y = a5_WL, color = o_preop_vektskole)) +
  geom_point(alpha = 0.2, size = 1.3) +
  geom_smooth(method = "loess", se = F) +
  xlab("Propensity score") +
  ylab("a5_WL") +
  theme_bw() + theme(legend.position = "none") +
  ylim(support)

dta$Female = as.factor(dta$Female)
dta$o_preop_vektskole <- as.factor(dta$o_preop_vektskole)
ggplot(dta, aes(x = distance, y = Female, color = o_preop_vektskole)) +
  geom_point(alpha = 0.2, size = 1.3) +
  geom_smooth(method = "loess", se = F) +
  xlab("Propensity score") +
  ylab("Female") +
  theme_bw() + theme(legend.position = "none") 
   


library(gridExtra)
grid.arrange(
  fn_bal(dta_m, "w3income"),
  fn_bal(dta_m, "p5numpla") + theme(legend.position = "none"),
  fn_bal(dta_m, "p5hmage"),
  fn_bal(dta_m, "w3momed_hsb") + theme(legend.position = "none"),
  fn_bal(dta_m, "race_white"),
  nrow = 3, widths = c(1, 0.8)
)







