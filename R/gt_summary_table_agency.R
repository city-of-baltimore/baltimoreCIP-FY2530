#' Create a gt table summarizing capital funds by year and agency
gt_summary_table_agency <- function(
    report_data,
    cip_verb = "recommended",
    # FIXME: Make the table title conditional based on the report stage
    title = "Total approved and planned capital funds by year and agency",
    tbl_cols = c(
      "agency_short_name",
      paste0("fy_", c(2025:2030)),
      "fy_total"
    ),
    tbl_labels = c(
      "Agency",
      paste0("FY", c(25:30)),
      "Total ($M)"
    ),
    cip_data_col = "recommendation_data") {
  cip_tbl_data <- report_data[[cip_data_col]] |>
    purrr::list_rbind() |>
    # FIXME: Rename request_id column with more general column name
    filter(!is.na(request_id)) |>
    summarise(
      across(
        starts_with("fy_"),
        \(x) {
          sum(x, na.rm = TRUE)
        }
      ),
      .by = c(agency_short_name)
    ) |>
    arrange(agency_short_name) |>
    ungroup()

  cip_tbl_data |>
    gt_full_summary(
      rowname_col = "agency_short_name",
      title = title,
      columns = tbl_cols,
      labels = tbl_labels
    )
}
