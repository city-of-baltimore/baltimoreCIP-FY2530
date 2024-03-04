#' Extract a CIP number
#' @rdname wd_str_extract
#' @name str_extract_cip_number
str_extract_cip_number <- function(string) {
  str_extract(
    string,
    "[:digit:]{3}\\-[:digit:]{3}"
  )
}
