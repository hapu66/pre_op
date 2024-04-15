# https://simonejdemyr.com/r-tutorials/statistics/tutorial8.html

library(MatchIt)
library(dplyr)
library(ggplot2)
setwd(r'(C:\Users\R\PSM\ecls-master)')
 ecls <- read.csv("data-processed\\ecls.csv")
 setwd(r'(C:\Users\R\PSM\ecls-master\R)')
 
ecls %>%
  group_by(catholic) %>%
  summarise(n_students = n(),
            mean_math = mean(c5r2mtsc_std),
            std_error = sd(c5r2mtsc_std) / sqrt(n_students))

with(ecls, t.test(c5r2mtsc_std ~ catholic))

# 1.2 Difference-in-means: pre-treatment covariates
#
# We’ll work with the following covariates for now:
#   
#   race_white: Is the student white (1) or not (0)?
#   p5hmage: Mother’s age
# w3income: Family income
# p5numpla: Number of places the student has lived for at least 4 months
# w3momed_hsb: Is the mother’s education level high-school or below (1) or some college or more (0)?
#   Let’s calculate the mean for each covariate by the treatment status:
  
ecls_cov <- c('race_white', 'p5hmage', 'w3income', 'p5numpla', 'w3momed_hsb')
ecls %>%
  group_by(catholic) %>%
  select(one_of(ecls_cov)) %>%
  summarise_all(list(~mean(., na.rm = T)))

# We can carry out t-tests to evaluate whether these means are statistically distinguishable:
  
  lapply(ecls_cov, function(v) {
    t.test(ecls[, v] ~ ecls[, 'catholic'])
  })
  
  # 2 Propensity score estimation
  # We estimate the propensity score by running a logit model (probit also works) where
  # the outcome variable is a binary variable indicating treatment status. 
  # What covariates should you include? 
  
# For the matching to give you a causal 
# estimate in the end, you need to include any covariate that is related to both 
# the treatment assignment and potential outcomes. 
  
  # I choose just a few covariates 
  # below—they are unlikely to capture all covariates that should be included. 
  # You’ll be asked to come up with a potentially better model on your own later.
  
  ecls <- ecls %>% mutate(w3income_1k = w3income / 1000)
  m_ps <- glm(catholic ~ race_white + w3income_1k + p5hmage + p5numpla + w3momed_hsb,
              family = binomial(), data = ecls)
  summary(m_ps)  
  
#  Using this model, we can now calculate the propensity score for each student. 
# It is simply the student’s predicted probability of being Treated, given the estimates
# from the logit model. Below, I calculate this propensity score using predict() and 
# create a dataframe that has the propensity score as well as the student’s actual treatment status.
  
  prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                       catholic = m_ps$model$catholic)
  head(prs_df)
  
#  After estimating the propensity score, it is useful to plot histograms of the estimated 
# propensity scores by treatment status:
    
    labs <- paste("Actual school type attended:", c("Catholic", "Public"))
  prs_df %>%
    mutate(catholic = ifelse(catholic == 1, labs[1], labs[2])) %>%
    ggplot(aes(x = pr_score)) +
    geom_histogram(color = "white") +
    facet_wrap(~catholic) +
    xlab("Probability of going to Catholic school") +
    theme_bw() 
  
  ecls_nomiss <- ecls %>%  # MatchIt does not allow missing values
    select(c5r2mtsc_std, catholic, one_of(ecls_cov)) %>%
    na.omit()
  
  mod_match <- matchit(catholic ~ race_white + w3income + p5hmage + p5numpla + w3momed_hsb,
                       method = "nearest", data = ecls_nomiss)
  
  
  summary(mod_match) 
  plot(mod_match)
  
#  To create a dataframe containing only the matched observations, use the match.data() function:
    
    dta_m <- match.data(mod_match)
  dim(dta_m)

    
#  4.1 Visual inspection
  fn_bal <- function(dta, variable) {
    dta$variable <- dta[, variable]
    if (variable == 'w3income') dta$variable <- dta$variable / 10^3
    dta$catholic <- as.factor(dta$catholic)
    support <- c(min(dta$variable), max(dta$variable))
    ggplot(dta, aes(x = distance, y = variable, color = catholic)) +
      geom_point(alpha = 0.2, size = 1.3) +
      geom_smooth(method = "loess", se = F) +
      xlab("Propensity score") +
      ylab(variable) +
      theme_bw() +
      ylim(support)
  }
  
library(gridExtra)
  grid.arrange(
    fn_bal(dta_m, "w3income"),
    fn_bal(dta_m, "p5numpla") + theme(legend.position = "none"),
    fn_bal(dta_m, "p5hmage"),
    fn_bal(dta_m, "w3momed_hsb") + theme(legend.position = "none"),
    fn_bal(dta_m, "race_white"),
    nrow = 3, widths = c(1, 0.8)
  )  
  
#  4.2 Difference-in-means
#  The means below indicate that we have attained a high degree of balance 
#  on the five covariates included in the model.  
  
  dta_m %>%
    group_by(catholic) %>%
    select(one_of(ecls_cov)) %>%
    summarise_all(funs(mean))  
  
  
#  You can test this more formally using t-tests. Ideally, we should not be 
# able to reject the null hypothesis of no mean difference for each covariate:
    
    lapply(ecls_cov, function(v) {
      t.test(dta_m[, v] ~ dta_m$catholic)
    })
    
#     4.3 Average absolute standardized difference
    
#    5 Estimating treatment effects
#    Estimating the treatment effect is simple once we have 
# a matched sample that we are happy with. We can use a t-test:
      
      with(dta_m, t.test(c5r2mtsc_std ~ catholic))  
      
#  Or we can use OLS with or without covariates:
      
      lm_treat1 <- lm(c5r2mtsc_std ~ catholic, data = dta_m)
      summary(lm_treat1)
      
      
      lm_treat2 <- lm(c5r2mtsc_std ~ catholic + race_white + p5hmage +
                        I(w3income / 10^3) + p5numpla + w3momed_hsb, data = dta_m)
      summary(lm_treat2)      
      
    