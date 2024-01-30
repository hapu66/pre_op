# source("set_up.R")
library(ggdist)

dl =    d_act_a5 %>% pivot_longer(cols = contains("bmi_"), names_to = "time", values_to = "value")   

dl_SG = d %>% filter(o_opmetode == 6) %>% pivot_longer(cols = contains("bmi"), names_to = "time", values_to = "value")  
dl_GB = d %>% filter(o_opmetode == 1) %>% pivot_longer(cols = contains("bmi"), names_to = "time", values_to = "value")  

dl_0 = dl %>% filter(o_preop_vektskole == 0)
dl_1 = dl %>% filter(o_preop_vektskole == 1)


plt_1 =  dl %>% ggplot(aes(x=time, y=value, 
                           fill=factor(o_preop_vektskole)))+
  labs(fill ="skole") +  
  stat_dotsinterval(data=dl_0, side = "bottom", scale=0.5, quantiles = 600, point_interval = mode_hdci  ) +
  stat_dotsinterval(data=dl_1,  side = "top", scale=0.5, quantiles = 600, point_interval = mode_hdci  ) +
  geom_boxplot(notch = TRUE, width=0.3, outlier.colour = "red", outlier.shape = 1) +
  #  geom_point(position = position_jitterdodge(), size= 0.2, alpha=0.3)+
  scale_fill_manual(values = c("lightyellow",  "lightgreen")) +
  scale_y_continuous(limits= c(15,60))+
  theme_bw(base_size = 16)

plt_1   # ujusterte tall
