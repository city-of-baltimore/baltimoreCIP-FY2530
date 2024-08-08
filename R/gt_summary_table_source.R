#' Create a summary table by source
gt_summary_table_source <- function(
    report_data,
    cip_verb = "recommended",
    cip_data_col = "recommendation_data",
    tbl_title = NULL,
    tbl_cols = c(
      "budget_category",
      paste0("fy_", c(2025:2030)),
      "fy_total"
    ),
    tbl_labels = c(
      "Source",
      paste0("FY", c(25:30)),
      "Total ($M)"
    ),
    perc = FALSE) {
  cip_tbl_data <- report_data[[cip_data_col]] |>
    purrr::list_rbind() |>
    filter(!is.na(request_id)) |>
    mutate(
      # FIXME: Double-check if this work around is still needed
      budget_category_sort = if_else(
        budget_category == "Utility Revenue" &
          fund_grant_special_purpose_name == "9965 FND Parking Facilities Capital Project Fund",
        22,
        budget_category_sort
      ),
      budget_category = if_else(
        budget_category == "Utility Revenue" &
          fund_grant_special_purpose_name == "9965 FND Parking Facilities Capital Project Fund",
        "Other",
        budget_category
      )
    ) |>
    summarise(
      budget_category_sort = min(budget_category_sort),
      across(
        starts_with("fy_"),
        \(x) {
          sum(x, na.rm = TRUE)
        }
      ),
      .by = c(budget_category)
    ) |>
    arrange(budget_category_sort) |>
    select(!budget_category_sort) |>
    ungroup()

  cip_tbl_data |>
    gt_full_summary(
      rowname_col = "budget_category",
      title = tbl_title,
      columns = tbl_cols,
      labels = tbl_labels
    )
}
