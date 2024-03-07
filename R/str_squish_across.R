#' Modify character columns using `stringr::str_squish` and `stringr::str_trim`
#'
#' [str_squish_across()] is a convenience function for squishing and removing
#' whitespace from all character columns in a data frame.
#'
#' @inheritParams stringr::str_trim
str_squish_across <- function(data) {
  mutate(
    data,
    across(
      where(is.character),
      \(x) {
        str_squish(x)
      }
    )
  )
}
