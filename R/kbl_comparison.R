#' Create a comparison table for the appendix based on `cip_comparison_summary`
#'
#' Used by `_appendix_recommendations.qmd`
#'
#' @seealso [kbl_tbl()]
kbl_comparison <- function(data,
                           col_names = c(
                             "", "Project", "Request",
                             "Recommendation",
                             "Change ($K)", "Change (%)"
                           ),
                           groupname_col = "agency_label",
                           digits = 0,
                           widths = c("3in", "1.125in", "1.125in", "0.875in", "0.875in"),
                           width_cols = c(2, 3, 4, 5, 6),
                           align = "llrrrr") {
  data |>
    mutate(
      # apply positive and negative
      fy_total_pct_diff = case_when(
        (fy_total_pct_diff == 0 | is.na(fy_total_pct_diff)) ~ "---",
        fy_total_pct_diff < 0 ~ paste0(
          round(fy_total_pct_diff * 100, digits = digits), "%"
        ),
        fy_total_pct_diff > 0 ~ paste0(
          "+", round(fy_total_pct_diff * 100, digits = digits), "%"
        )
      ),
      across(
        c(request, recommendation, fy_total_diff),
        \(x) {
          x <- vec_fmt_currency_plain(x, escape = FALSE)
          stringr::str_replace(x, "NA", "---")
        }
      ),
      fy_total_diff = if_else(
        str_detect(fy_total_diff, "\\)"),
        fy_total_diff,
        paste0(fy_total_diff, "  ")
      )
    ) |>
    kbl_tbl(
      col_names = col_names,
      widths = widths,
      align = align,
      width_cols = width_cols,
      latex_options = "repeat_header",
      groupname_col = groupname_col,
      rowname_col = "project_code",
      rowname_col_label = ""
    )
}
