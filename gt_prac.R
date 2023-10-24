a =231164
library(gtsummary)
library(gt)
exibble |>
      gt(rowname_col = "row") |>
       rows_add(
             row = "row_h",
             num = a,
             char = "ilama",
             fctr = "nine",
             group = "grp_b",
             .after = "row_4"
         )


exibble |>
  gt(rowname_col = "row", groupname_col = "group") |>
  rows_add(
    row = c("row_a_end", "row_b_end"),
    group = c("grp_a", "grp_b")
  ) |>
  tab_style(
    style = cell_borders(sides = "top", style = "hidden"),
    locations = list(
      cells_body(rows = ends_with("end")),
      cells_stub(rows = ends_with("end"))
    )
  ) |>
  sub_missing(missing_text = "") |>
  text_case_when(
    grepl("end", x) ~ "",
    .locations = cells_stub()
  ) |>
  opt_vertical_padding(scale = 0.5)


dplyr::tibble(
  msrp = numeric(0),
  item = character(0),
  type = character(0)
) |>
  gt() |>
  rows_add(
    msrp = c(29.95, 49.95, 79.95, 14.95),
    item = c("Klax", "Rez", "Ys", "D"),
    type = c("A", "B", "X", "Z")
  ) |>
  cols_add(
    genre = c("puzzle", "action", "RPG", "adventure")
  ) |>
  fmt_currency() |>
  cols_move_to_end(columns = msrp)


trial |> tbl_summary() |> as_gt() |>   
  rows_add(
     
  )

sp500 |>
  dplyr::filter(date >= "2015-01-05" & date <= "2015-01-16") |>
  dplyr::arrange(date) |>
  dplyr::mutate(week = paste0("W", strftime(date, format = "%V"))) |>
  dplyr::select(-adj_close, -volume) |>
  gt(
    rowname_col = "date",
    groupname_col = "week"
  ) |>
  summary_rows(
    fns = list(
      "min",
      "max",
      list(label = "avg", fn = "mean")
    ),
    fmt = ~ fmt_number(., use_seps = FALSE)
  )


trial |> tbl_summary() |> as_gt()  |> add_row(  .before = "Age")

exibble |>
  gt(rowname_col = "row", groupname_col = "group") |>
  rows_add(
    row = c("row_a_end", "row_b_end"),
    group = c("grp_a", "grp_b")
  ) |>
  tab_style(
    style = cell_borders(sides = "top", style = "hidden"),
    locations = list(
      cells_body(rows = ends_with("end")),
      cells_stub(rows = ends_with("end"))
    )
  ) |>
  sub_missing(missing_text = "") |>
  text_case_when(
    grepl("end", x) ~ "",
    .locations = cells_stub()
  ) |>
  opt_vertical_padding(scale = 0.5)


dplyr::tibble(
  time = lubridate::POSIXct(),
  event = character(0)
) |>
  gt() |>
  rows_add(
    time = lubridate::ymd_hms("2022-01-23 12:36:10"),
    event = "start"
  ) |>
  rows_add(
    time = lubridate::ymd_hms("2022-01-23 13:41:26"),
    event = "completed"
  )


# Take the `islands` dataset and use some
# dplyr functionality to obtain the ten
# biggest islands in the world
islands_tbl <- 
  tibble(
    name = names(islands),
    size = islands
  ) |>
  arrange(desc(size)) |>
  slice(1:10)

# Display the table
islands_tbl

# Create a display table showing ten of
# the largest islands in the world
gt_tbl <- gt(islands_tbl)

# Show the gt Table
gt_tbl


# Use markdown for the heading's `title` and `subtitle` to
# add bold and italicized characters
gt(islands_tbl[1:2,]) |>
  tab_header(
    title = md("**Large Landmasses of the World**"),
    subtitle = md("The *top two* largest are presented")
  ) |> rows_add( name="EU", size=  4325)

## ------------------

kasi = islands_tbl[8,]
ysi = islands_tbl[9,]

G = gt(islands_tbl[1:2,]) |>
  tab_header(
    title = md("**Large Landmasses of the World**"),
    subtitle = md("The *top two* largest are presented")
  ) |> rows_add( name= kasi$name, size = kasi$size * ysi$size) 
