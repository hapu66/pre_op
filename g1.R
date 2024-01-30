# source("set_up.R")
library(ggdist)

# dl =    d_act_a5 %>%  
#   mutate( baseline = bmi_0, operation = bmi_0o, `1 m` = bmi_0u6, `1 yr` = bmi_1a, `2 yr` = bmi_2a, `5 yr` = bmi_5a) %>% 
#   pivot_longer(cols = c( "baseline", "operation", `1 m`, `1 yr`, `2 yr`, `5 yr`),   
#                names_to = "time", values_to = "value")  

dl =    d_act_a5 %>% pivot_longer(cols = contains("bmi_"), names_to = "time", values_to = "value")   

# dl_SG = d %>% filter(o_opmetode == 6) %>% pivot_longer(cols = contains("bmi"), names_to = "time", values_to = "value")  
# dl_GB = d %>% filter(o_opmetode == 1) %>% pivot_longer(cols = contains("bmi"), names_to = "time", values_to = "value")  

dl_0 = dl %>% filter(o_preop_vektskole == 0)
dl_1 = dl %>% filter(o_preop_vektskole == 1)


plt_1 =  dl %>% ggplot(aes(x=time, y=value, 
                           fill=factor(o_preop_vektskole)))+
 # scale_fill_discrete(name = "Program", labels = c("SPEP", "EPEP"))+ # labs(fill ="Program") +  
  stat_dotsinterval(data=dl_0, side = "bottom", scale=0.5, quantiles = 600, point_interval = mode_hdci  ) +
  stat_dotsinterval(data=dl_1,  side = "top", scale=0.5, quantiles = 600, point_interval = mode_hdci  ) +
  geom_boxplot(notch = TRUE, width=0.3, outlier.colour = "red", outlier.shape = 1) +
  #  geom_point(position = position_jitterdodge(), size= 0.2, alpha=0.3)+
  scale_fill_manual(name = "Program", values = c("lightyellow",  "lightgreen"), labels = c("SPEP", "EPEP")) +
  scale_y_continuous(limits= c(15,60))+
  theme_bw(base_size = 16)

plt_1 + labs(y= "BMI") +  # ujusterte tall
scale_x_discrete(labels=c("bmi_0" = "baseline", "bmi_0o" = "operation",
                          "bmi_0u6" = "1 month","bmi_1a"="1 year","bmi_2a"="2 years","bmi_5a"="5 years"))
# +scale_fill_discrete(name = "Program", labels = c("SPEP", "EPEP"))  # labs(fill ="Program") 