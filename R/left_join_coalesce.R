#' Left join two data frames and then coalesce values from specified columns
#'
#' Use [dplyr::left_join()] to join a dataframe to a data frame with a shared
#' join column and a column of replacement values then [coalesce()] values
#' across two columns.
#'
#' @param data Input data frame
#' @param update_data Update data with columns `by_col` and `update_col`
#' @param col Existing values from this column are retained if the corresponding
#'   update_col value is `NULL`.
#' @param update_col Name of column in `update_data` to coalesce with `col`.
#' @param by_col String to pass to by in [left_join()], Default: 'project_code'
#' @param relationship Passed to [left_join()], Default: 'one-to-one'
#' @param na_matches Passed to [left_join()], Default: 'never'
#' @returns A data frame
left_join_coalesce <- function(
    data,
    update_data,
    col,
    update_col,
    by = "project_code",
    relationship = "one-to-one",
    na_matches = "never") {
  check_names(data, must.include = c(by, col))
  check_names(update_data, must.include = c(by, update_col))

  data |>
    left_join(
      update_data,
      by = by, # join_by(by),
      relationship = relationship,
      na_matches = na_matches,
    ) |>
    mutate(
      "{col}" := coalesce(
        .data[[update_col]],
        .data[[col]]
      )
    ) |>
    select(!any_of(update_col))
}
