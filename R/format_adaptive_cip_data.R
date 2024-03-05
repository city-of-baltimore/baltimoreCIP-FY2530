#' Format an exported CIP data report
#'
format_adaptive_cip_data <- function(data,
                                     dictionary,
                                     revenue_category_name_xwalk) {
  stopifnot(
    all(has_name(dictionary, c("adaptive_export_name", "adaptive_import_col_type")))
  )

  # Subset dictionary to fields in file
  cip_dictionary <- filter(
    dictionary,
    adaptive_export_name %in% names(data)
  )

  cip_dictionary_numeric <- filter(
    cip_dictionary,
    adaptive_import_col_type == "numeric"
  )

  data |>
    mutate(
      request_id = row_number(),
      across(
        all_of(cip_dictionary_numeric[["adaptive_export_name"]]),
        \(x) {
          if (is.character(x)) {
            readr::parse_number(x)
          } else {
            x
          }
        }
      )
    ) |>
    janitor::clean_names("snake") |>
    filter(
      # Filter rows appearing after "Total" summary row
      !cumany(cost_center_code == "Total"),
      !is.na(project_code)
    ) |>
    format_fgs_fund_cols() |>
    format_cip_cols(
      revenue_category_name_xwalk = revenue_category_name_xwalk
    ) |>
    select(
      "project_code",
      "request_id",
      # FIXME: Document that this data doesn't include the project name
      # "project_name",
      starts_with(
        c(
          "fund_", "fgs_grant_", "fgss_purpose_",
          "r", # r object + revenue category
          "g", # grant
          "fy"
        )
      ),
      any_of(names(revenue_category_name_xwalk))
    )
}
