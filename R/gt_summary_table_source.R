gt_group_summary_tables <- function(report_data,
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

gt_summary_table_source <- function(
    report_data,
    cip_verb = "recommended",
    cip_data_col = "recommendation_data",
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

  tbl_title <- paste0(
    "Total ", cip_verb, " capital funds by year and source category"
  )

  tbl_cols <- c(
    "budget_category",
    paste0("fy_", c(2025:2030)),
    "fy_total"
  )

  tbl_labels <- c(
    "Source",
    paste0("FY", c(25:30)),
    "Total ($M)"
  )

  cip_tbl_data |>
    gt_full_summary(
      rowname_col = "budget_category",
      title = tbl_title,
      columns = tbl_cols,
      labels = tbl_labels
    )

  # cip_tbl_data |>
  #   gt(
  #     rowname_col = "budget_category"
  #   ) |>
  #   grand_summary_rows(
  #     columns = starts_with("fy_"),
  #     fns = list(
  #       "Total ($M)" ~ sum(., na.rm = TRUE)
  #     ),
  #     fmt = list(~ fmt(
  #       data = .,
  #       columns = starts_with("fy_"),
  #       fns = \(x){
  #         vec_fmt_currency_plain(x, scale_by = 0.000001, decimals = 1)
  #       }
  #     ))
  #   ) |>
  #   cols_label_ext(
  #     columns = tbl_cols,
  #     labels = tbl_labels
  #   ) |>
  #   fmt_currency_plain(
  #     columns = starts_with("fy_"),
  #     scale_by = 0.000001,
  #     decimals = 1
  #   ) |>
  #   tab_header(tbl_title)
}

gt_summary_table_agency <- function(
    report_data,
    cip_verb = "recommended",
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


  tbl_cols <- c(
    "agency_short_name",
    paste0("fy_", c(2025:2030)),
    "fy_total"
  )

  tbl_labels <- c(
    "Agency",
    paste0("FY", c(25:30)),
    "Total ($M)"
  )

  tbl_title <- paste0(
    "Total ", cip_verb, " capital funds by year and agency"
  )

  #   cip_tbl_data |>
  #     gt(
  #       rowname_col = "agency_short_name"
  #     ) |>
  #     grand_summary_rows(
  #       columns = starts_with("fy_"),
  #       fns = list(
  #         "Total ($M)" ~ sum(., na.rm = TRUE)
  #       ),
  #       fmt = list(~ fmt(
  #         data = .,
  #         columns = starts_with("fy_"),
  #         fns = \(x){
  #           vec_fmt_currency_plain(x, scale_by = 0.000001, decimals = 1)
  #         }
  #       ))
  #     ) |>
  #     cols_label_ext(
  #       columns = tbl_cols,
  #       labels = tbl_labels
  #     ) |>
  #     fmt_currency_plain(
  #       columns = starts_with("fy_"),
  #       scale_by = 0.000001,
  #       decimals = 1
  #     ) |>
  #     tab_header(tbl_title)

  cip_tbl_data |>
    gt_full_summary(
      rowname_col = "agency_short_name",
      title = tbl_title,
      columns = tbl_cols,
      labels = tbl_labels
    )
}

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
