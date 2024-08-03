#' Create a summary gt table with CIP budget data
#'
#' By default the summary table uses the `agency_short_name` as the table row name.
gt_full_summary <- function(
    data,
    rowname_col = "agency_short_name",
    title = NULL,
    columns = NULL,
    labels = NULL) {
  data |>
    gt(
      rowname_col = rowname_col
    ) |>
    grand_summary_rows(
      columns = starts_with("fy_"),
      fns = list(
        "Total ($M)" ~ sum(., na.rm = TRUE)
      ),
      fmt = list(~ fmt(
        data = .,
        columns = starts_with("fy_"),
        fns = \(x){
          vec_fmt_currency_plain(x, scale_by = 0.000001, decimals = 1)
        }
      ))
    ) |>
    cols_label_ext(
      columns = columns,
      labels = labels
    ) |>
    fmt_currency_plain(
      columns = starts_with("fy_"),
      scale_by = 0.000001,
      decimals = 1
    ) |>
    tab_header(title)
}

pull_from_gt_grand_summary <- function(gt_object, ...) {
  extract_summary(gt_object) |>
    pluck("summary_df_data_list", "::GRAND_SUMMARY") |>
    pull(...)
}

fy_from_gt <- function(gt_object, fy_col = "fy_total", suffixing = TRUE, ...) {
  gt::vec_fmt_currency(
    pull_from_gt_grand_summary(gt_object, .data[[fy_col]]),
    suffixing = suffixing,
    ...
  )
}
