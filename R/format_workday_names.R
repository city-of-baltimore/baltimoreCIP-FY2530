#' Format p_manager_name and p_project_owner_name columns
#' @seealso [format_p_manager_cols()]; [format_p_owner_cols()]
format_workday_names <- function(data) {
  check_names(
    data,
    must.include = c(
      "p_manager_code", "p_manager_name",
      "p_project_owner_code", "p_project_owner_name"
    )
  )

  data |>
    format_p_manager_cols() |>
    format_p_owner_cols()
}
