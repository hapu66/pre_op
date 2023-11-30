#   
# Follow up 5 yrs	1,183 (64%)	1,511 (61%)	0.047
# %TWL	               27.3 (10.2)	27.0 (10.1)	0.41              justert bort RE-sykehus
# 
# Delta BMI (kg/m^2)	-11.5 (4.9)	   -12.0 (5.0)	0.030
library(glue)
#  d_act_a5 # 2985
#  d_a5_j  #  2963
  
dt =  d_act_a5 |> 
    select(p_pasientid, ForlopsID, o_sykehus,   a5_TWL, bmi_0, bmi_5a) |>
    mutate(a5_dBMI = bmi_0 - bmi_5a)

dt |> ggplot(aes(x= a5_TWL, y=a5_dBMI, colour=o_sykehus)) + 
  geom_point() +  ggtitle("Weight measures are highly correlated.")+
 xlab("Five years %TWL")+
  ylab(expression(Delta-BMI)) + theme_minimal() 
  

# title = "TWL and d-BMI are highly correlated.",
# ylab = 

cm <- function(p1, p2){
  u =  dt |> filter(p_pasientid == p1)
  v =  dt |> filter(p_pasientid == p2)
  dWL  = u$a5_TWL - v$a5_TWL
  dBM  = u$a5_dBMI - v$a5_dBMI
  ifelse( sign( dWL*dBM ) == -1, "N","P") 
}  

 



  cmpr <- function( p1, p2){
     tb =   dt |> filter(p_pasientid %in% c(p1, p2)) |>  select(a5_TWL, a5_dBMI)
    res= ifelse( sign( tb[["a5_TWL"]][1] - tb[["a5_TWL"]][2]) != sign( tb[["a5_dBMI"]][1] - tb[["a5_dBMI"]][2] ), "W" , "C")
    print(res)
    } 

    map2_chr( dt$p_pasientid, dt$p_pasientid, ~cmpr( .x,.y))
    
    
    x <- list(1, 10, 100)
    y <- list(1, 2, 3)
    map2(x, y, ~ .x + .y)
    
    
G =  expand_grid(dt$p_pasientid, dt$p_pasientid)
names(G) <- c("pId1", "pId2")

H = G[1:100000,]

map2_chr(H$pId1, H$pId2, cm) |> table()
map2_chr(G$pId1, G$pId2, cm) |> table()

#  H = G[1:100000,]
# 
# > map2_chr(H$pId1, H$pId2, cm) |> table()
# 
# N     P 
# 8164 91836 


map2_chr(.x =  G[,1],.y =  G[,2], .f = ~cmpr(.x,.y))