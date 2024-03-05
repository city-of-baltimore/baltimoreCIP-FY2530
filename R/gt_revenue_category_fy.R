## ---- gt_revenue_category_fy
#' Create gt table with revenue category by year
#'
#' This takes a list column of data frames
gt_revenue_category_fy <- function(data,
                                   title = "Total agency requests by source") {
  data <- data |>
    purrr::list_rbind()

  if (!has_name(data, "request_id")) {
    return("Table not working!!")
  }

  data |>
    filter(!is.na(request_id)) |>
    summarise(
      budget_category_sort = min(budget_category_sort),
      across(
        starts_with("fy_"),
        \(x) {
          sum(x, na.rm = TRUE)
        }
      ),
      .by = c(revenue_category_name_short)
    ) |>
    arrange(budget_category_sort) |>
    select(!budget_category_sort) |>
    ungroup() |>
    gt(
      groupname_col = "revenue_category_name_short",
      row_group_as_column = TRUE
    ) |>
    grand_summary_rows(
      columns = starts_with("fy_"),
      fns = list(
        "Total ($K)" ~ sum(., na.rm = TRUE)
      ),
      fmt = list(~ fmt(
        data = .,
        columns = starts_with("fy_"),
        fns = \(x){
          vec_fmt_currency_plain(x)
        }
      ))
    ) |>
    cols_label_ext(
      columns = c(
        "budget_category",
        paste0("fy_", c(2025:2030)),
        "fy_total"
      ),
      labels = c(
        "Source",
        paste0("FY", c(25:30)),
        "Total ($K)"
      )
    ) |>
    fmt_currency_plain(
      columns = starts_with("fy_")
    ) |>
    tab_header(
      title = title
    )
}
