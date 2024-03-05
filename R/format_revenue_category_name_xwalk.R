format_revenue_category_name_xwalk <- function(xwalk) {
  xwalk |>
    filter(
      form_field == "Revenue category"
    ) |>
    select(
      revenue_category_sort,
      revenue_category_code,
      revenue_category_name,
      revenue_category_label,
      revenue_category_name_short,
      r_object_name,
      budget_category_sort,
      budget_category,
      exclude_flag
    )
}
