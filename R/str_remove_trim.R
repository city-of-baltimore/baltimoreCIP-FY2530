#' Remove part of a string matching a pattern and then trim whitespace
#'
#' [str_remove_trim()] is a wrapper for `str_remove` and
#' `str_trim`. The function is used by [format_prj_data()] to
#' remove codes from name columns.
#'
#' @inheritParams stringr::str_remove
#' @inheritParams stringr::str_trim
str_remove_trim <- function(string, pattern, side = c("both", "left", "right")) {
  str_trim(str_remove(string, pattern), side = side)
}
