#' Create a project fiscal year capital request table
#'
#' [gt_prj_fy_requests()] is used to insert a standalone fiscal year
#' request table in a project profile report.
#'
#' @seealso [gt_project_details()]
gt_prj_fy_requests <- function(data,
                               labels = NULL,
                               prefix = "FY",
                               decimals = 0,
                               scale_by = NULL,
                               suffixing = NULL,
                               show_summary = TRUE,
                               ...,
                               textwidth = 10,
                               title = "Recommendations by source",
                               request_source_col = "revenue_category_name_short",
                               tbl_fy_cols = getOption(
                                 "tbl_fy_cols",
                                 c("fy_total", paste0("fy_", c(2025:2030)))
                               ),
                               tbl_fy_labels = getOption(
                                 "tbl_fy_labels",
                                 c("Total ($K)", paste0(prefix, c(25:30)))
                               ),
                               rowname_col = request_source_col) {
  if (vctrs::is_list_of(data) && has_length(data, 1)) {
    data <- data[[1]]
  }

  check_data_frame(data)

  fy_cols <- tbl_fy_cols

  labels <- labels %||%
    set_names(
      c("Source", tbl_fy_labels),
      c(request_source_col, fy_cols)
    )

  data <- data |>
    select(
      all_of(request_source_col),
      starts_with("fy")
    ) |>
    arrange(
      .data[[request_source_col]]
    ) |>
    gt_simple(
      ...,
      rowname_col = rowname_col,
      labels = labels,
      textwidth = textwidth
    ) |>
    fmt_currency_plain(
      columns = all_of(c("fy_total", fy_cols))
    ) |>
    sub_missing(
      columns = all_of(request_source_col)
    )

  if (!show_summary) {
    return(data)
  }

  data <- data |>
    grand_summary_rows(
      columns = starts_with("fy_"),
      fns = list("Total by Year ($K)" ~ sum(., na.rm = TRUE)),
      fmt = list(~ fmt(
        data = .,
        columns = starts_with("fy_"),
        fns = \(x){
          vec_fmt_currency_plain(x)
        }
      ))
    )

  data |>
    tab_header(
      title = title
    )
}
