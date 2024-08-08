# NOTE: Requires cols_width_ext function
gt_revenue_category_reference <- function(revenue_category_name_xwalk) {
  sources_abb <- revenue_category_name_xwalk |>
    select(
      all_of(
        c(
          "budget_category",
          "revenue_category_label",
          "revenue_category_name_short",
          "exclude_flag"
        )
      )
    ) |>
    filter(
      is.na(exclude_flag),
      !is.na(budget_category)
    ) |>
    select(!exclude_flag)

  sources_abb |>
    gt(
      groupname_col = "budget_category",
      row_group_as_column = TRUE
    ) |>
    cols_label(
      revenue_category_label ~ "Funding source",
      revenue_category_name_short ~ "Abbreviation"
    ) |>
    cols_width_ext(
      columns = c("budget_category", "revenue_category_label", "revenue_category_name_short"),
      widths = c(0.25, 0.4, 0.35),
      textwidth = 5
    ) |>
    tab_header(
      title = "Capital funding source names with abbreviations"
    )
}
