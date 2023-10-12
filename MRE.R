library(tidyverse)
library(gtsummary)
library(gt)
 
mtcars |>
  tbl_summary( by = gear) |>
  add_p()  |>
  as_gt() |> 
  opt_footnote_marks(marks = "letters") |>
  gt::gtsave("mre.docx")

 