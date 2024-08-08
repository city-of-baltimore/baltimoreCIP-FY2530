#' Format p_manager_name column
format_p_manager_cols <- function(data) {
  data |>
    format_code_name_cols(
      code_col = "p_manager_code",
      name_col = "p_manager_name",
      pattern = "\\([:alnum:]+\\)",
      .f = str_extract
    ) |>
    mutate(
      p_manager_name = str_remove_trim(p_manager_name, "\\(\\)")
    )
}
