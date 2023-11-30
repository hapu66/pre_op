

my_theme <-
  list(
    # round large p-values to two places
    "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 2),
    "pkgwide-fn:prependpvalue_fun" = function(x) style_pvalue(x, digits = 2, prepend_p = TRUE),
    # report median (IQR) and n (percent) as default stats in `tbl_summary()`
    "tbl_summary-str:continuous_stat" = "{median} ({p25} - {p75})",
    "tbl_summary-str:categorical_stat" = "{n} ({p})"
  )


check_gtsummary_theme(my_theme)

set_gtsummary_theme(my_theme)