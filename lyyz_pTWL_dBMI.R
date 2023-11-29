#   
# Follow up 5 yrs	1,183 (64%)	1,511 (61%)	0.047
# %TWL	               27.3 (10.2)	27.0 (10.1)	0.41              justert bort RE-sykehus
# 
# Delta BMI (kg/m^2)	-11.5 (4.9)	   -12.0 (5.0)	0.030

#  d_act_a5 # 2985

#  d_a5_j  #  2963
  
  
  
  d_act_a5 |> 
    select(o_sykehus,   a5_TWL, bmi_0, bmi_5a) |>
    mutate(a5_dBMI = bmi_0 - bmi_5a)
  
  