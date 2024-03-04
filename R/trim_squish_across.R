#' Modify character columns using `str_squish` and `str_trim`
#'
#' [trim_squish_across()] is a convenience function for squishing and removing
#' whitespace from all character columns in a data frame.
#' @inheritParams stringr::str_trim
trim_squish_across <- function(data, side = c("both", "left", "right")) {
  mutate(
    data,
    across(
      where(is.character),
      \(x) {
        str_trim(str_squish(x), side = side)
      }
    )
  )
}
