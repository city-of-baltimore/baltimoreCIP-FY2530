#' Create a list of two gt tables with `gt_summary_table_agency()` and
#' `gt_summary_table_source()`
gt_group_summary_tables <- function(
    report_data,
    cip_verb = "recommended",
    cip_data_col = "recommendation_data") {
  list(
    "table_agency" = gt_summary_table_agency(
      report_data = report_data,
      cip_verb = cip_verb,
      cip_data_col = cip_data_col
    ),
    "table_source" = gt_summary_table_source(
      report_data = report_data,
      cip_verb = cip_verb,
      cip_data_col = cip_data_col
    )
  )
}
