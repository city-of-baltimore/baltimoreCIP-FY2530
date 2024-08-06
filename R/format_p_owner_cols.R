#' Format p_project_owner_name column
format_p_owner_cols <- function(data) {
  data |>
    format_code_name_cols(
      code_col = "p_project_owner_code",
      name_col = "p_project_owner_name",
      pattern = "\\([:alnum:]+\\)",
      .f = str_extract
    ) |>
    mutate(
      p_project_owner_name = str_remove_trim(p_project_owner_name, "\\(\\)")
    )
}
