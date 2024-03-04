## ---- format_workday_names
format_workday_names <- function(data) {
  stopifnot(
    all(has_name(data, c("p_manager_code", "p_manager_name", "p_project_owner_code", "p_project_owner_name")))
  )

  ## Names
  data |>
    format_p_manager_cols() |>
    format_p_owner_cols()
}
