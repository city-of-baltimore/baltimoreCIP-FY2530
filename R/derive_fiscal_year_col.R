#' Derive a new fiscal year column from the project_name column
#'
derive_fiscal_year_col <- function(data) {
  check_names(data, must.include = "project_name")

  cli_alert_new_col("fiscal_year", "project_name")

  mutate(
    data,
    fiscal_year = if_else(
      str_detect(
        project_name,
        "FY[:digit:]{2,4}|FY [:digit:]{2,4}"
      ),
      str_extract(
        project_name,
        "((?<=FY)[:digit:]{2,4})|((?<=FY )[:digit:]{2,4})"
      ),
      NA_character_
    ),
    fiscal_year = if_else(
      !str_length(fiscal_year) == 4,
      str_c("20", fiscal_year),
      fiscal_year
    )
  )
}
