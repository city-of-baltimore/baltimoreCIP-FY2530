#' Format request program type column
#'
#' @seealso [join_request_program_type()]
format_request_program <- function(
    data,
    project_name_col = "Project Name",
    program_col = "request_program_name",
    request_type_col = "request_type",
    ...) {
  data |>
    mutate(
      "{program_col}" := match_request_program(.data[[project_name_col]]),
      .after = all_of(project_name_col)
    ) |>
    join_request_program_type(
      program_col = program_col,
      request_type_col = request_type_col
    )
}
