#' Derive an account type column value based on the project name
#'
#' This function also strips the "active" suffix from the project name.
derive_account_type_col <- function(data) {
  check_names(data, must.include = "project_name")
  cli_alert_new_col("account_type", "project_name")

  mutate(
    data,
    account_type = case_when(
      str_detect(
        project_name,
        "Unallocated Reserve"
      ) ~ "Unallocated Reserve",
      str_detect(
        project_name,
        "Construction Reserve"
      ) ~ "Construction Reserve",
      str_detect(
        project_name,
        "Reserve"
      ) ~ "Reserve",
      str_detect(
        project_name,
        "Active$|ACTIVE|Act$|ACT$|Active [:digit:]"
      ) ~ "Active",
      .default = NA_character_
    ),
    project_name = str_remove_all(
      project_name,
      "Active$|ACTIVE|Act$|ACT$|Active [:digit:]"
    )
  )
}
