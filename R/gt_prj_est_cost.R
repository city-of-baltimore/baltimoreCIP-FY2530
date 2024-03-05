## ---- gt_prj_est_cost
# Used by _project_report.qmd
gt_prj_est_cost <- function(data,
                            title = "Estimated costs",
                            name_col = "Cost type",
                            value_col = "Estimate") {
  # sum_if_any <- function(x, na.rm = TRUE) {
  #   if (all(is.na(x))) {
  #     return(NA)
  #   }
  #
  #   sum(x, na.rm = na.rm)
  # }

  data |>
    select(
      starts_with("est_cost")
    ) |>
    tidyr::pivot_longer(
      cols = starts_with("est_cost"),
      names_to = name_col,
      values_to = value_col,
      names_pattern = "est_cost_(.+)",
      names_transform = str_to_title
    ) |>
    filter(
      .data[[name_col]] != "Total"
    ) |>
    mutate(
      "{value_col}" := as.numeric(.data[[value_col]])
    ) |>
    gt(
      rowname_col = name_col
    ) |>
    fmt_currency_plain(
      columns = value_col
    ) |>
    sub_missing() |>
    grand_summary_rows(
      columns = value_col,
      fns = list("Total ($K)" ~ sum(., na.rm = TRUE)),
      fmt = list(~ fmt(
        data = .,
        columns = value_col,
        fns = \(x){
          vec_fmt_currency_plain(x)
        }
      ))
    ) |>
    sub_missing() |>
    tab_header(title = title)
}
