##############
# T_GS = tbl_merge(tbls = list(T_GS_d30, T_GS_a5), tab_spanner = c("**30 d**","**a 5**"))
# T_GB = tbl_merge(tbls = list(T_GB_d30, T_GB_a5), tab_spanner = c("**30 d**","**a 5**"))
###############

# TA =  T_GS %>% as_gt %>% opt_footnote_marks(marks = "letters") 
# TB =  T_GB %>% as_gt %>% opt_footnote_marks(marks = "letters")
# TC =  T_GS_GB %>% as_gt %>% opt_footnote_marks(marks = "letters") 

library(officer)
library(gto)
library(gt)

#  gt_tbl <- gt(head(exibble))

doc <- read_docx()
doc <- body_add_gt(doc, value = TA)  # T_GB  T_GS_GB  TA TB TC
fileout <-   "TA.docx"
print(doc, target = fileout)
