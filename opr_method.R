#  2024-02-16    effect of GS/GB 

d_act_a5 |> group_by(o_preop_vektskole, o_opmetode) |> summarize(WL = mean(a5_TWL), WL_sd=sd(a5_TWL))

d_act_a5 |> group_by(  o_opmetode) |> summarize(WL = mean(a5_TWL), WL_sd=sd(a5_TWL))

# o_opmetode    WL WL_sd
# -------------------------
#   1          1  29.4  9.96
#   2          6  25.7 10.1 
table(d_act_a5$o_opmetode, d_act_a5$o_preop_vektskole)

#      0    1
# 1  603  933
# 6 1098  635

SPEP_GS_p = 1090/(1090+603) # 0.6438275
EPEP_GS_p = 635/(933+635)   #  0.4049745


# ---------------------------------------------------------------------------
#  SPEP
#
SPEP_GS_p * 25.7 +  (1- SPEP_GS_p) * 29.4
# 27.01784

# EPEP
#
EPEP_GS_p * 25.7 +  (1- EPEP_GS_p) * 29.4
# 27.90159

d_a5 = as.data.frame(d_act_a5) 
d_a5$o_preop_vektskole = factor(d_a5$o_preop_vektskole)
gghistogram( d_a5, x="a5_TWL", add = "mean", rug = TRUE,
            color = "o_preop_vektskole", fill = "o_preop_vektskole",
            palette = c("#00AFBB", "#E7B800"), bins = 90)