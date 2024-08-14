#' Summarise CIP data by fiscal year
#'
#' [summarise_cip_data_fy()] wraps [dplyr::summarise()] to combine fiscal year
#' amount columns grouped by some other variables.
#'
#' @param data Input data frame with numeric fiscal year columns.
#' @param fy_col_prefix String that the fiscal year columns start with. Passed
#'   to [tidyselect::starts_with()] and is not case sensitive.
#' @returns A data frame with FY columns summed by the column names passed to
#'   .by
summarise_cip_data_fy <- function(data,
                                  .by = NULL,
                                  fy_col_prefix = "FY",
                                  na.rm = TRUE) {
  data |>
    summarise(
      across(
        starts_with(fy_col_prefix),
        \(x) {
          sum(x, na.rm = na.rm)
        }
      ),
      .by = all_of(.by)
    )
}
