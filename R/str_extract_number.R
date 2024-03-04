#' Extract and parse digits from a string
#'
#' [str_extract_number()] is a wrapper for `stringr::str_extract` and
#' `readr::parse_number`.
#'
#' @inheritParams stringr::str_extract
str_extract_number <- function(string, pattern = "[:digit:]+") {
  readr::parse_number(str_extract(string, pattern))
}
