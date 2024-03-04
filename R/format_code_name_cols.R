#' Tidy values in a matched pair of name and code columns
#'
#' Use [str_remove_all()] (by default) and [str_remove_trim()] to extract a code
#' column value from a combined name-code column and then remove the code value
#' from the name column.
#'
#' @param data Input data frame
#' @param update_data Update data with columns `by_col` and `update_col`
#' @param col Existing values from this column are retained if the corresponding
#'   update_col value is `NULL`.
#' @param update_col Name of column in `update_data` to coalesce with `col`.
#' @param by_col String to pass to by in [left_join()], Default: 'project_code'
#' @param relationship Passed to [left_join()], Default: 'one-to-one'
#' @param na_matches Passed to [left_join()], Default: 'never'
#' @family adaptive
#' @returns A data frame
format_code_name_cols <- function(
    data,
    code_col,
    name_col,
    pattern,
    .f = str_remove_all) {
  check_function(.f)

  mutate(
    data,
    "{code_col}" := .f(.data[[code_col]], pattern),
    "{name_col}" := str_remove_trim(.data[[name_col]], .data[[code_col]])
  )
}
