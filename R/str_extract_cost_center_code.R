#' Extract a leading Cost Center Code
#' @rdname wd_str_extract
str_extract_cost_center_code <- function(string) {
  str_extract(
    string,
    "^CCA[:digit:]+"
  )
}
