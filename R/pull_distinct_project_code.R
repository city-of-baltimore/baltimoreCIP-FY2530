#' Filter a data frame and then pull a distinct vector of project codes
pull_distinct_project_code <- function(x, ...) {
  x |>
    dplyr::filter(...) |>
    dplyr::distinct(project_code) |>
    dplyr::pull(project_code)
}
