#' Extract Project Codes (a.k.a. PRJ number)
#' @rdname wd_str_extract
#' @name str_extract_project_code
str_extract_project_code <- function(string) {
  str_extract(
    string,
    "PRJ[:digit:]+"
  )
}

#' Extract all Project Codes (a.k.a. PRJ numbers)
#' @rdname wd_str_extract
#' @name str_extract_all_project_codes
str_extract_all_project_codes <- function(string, ...) {
  str_extract_all(
    string,
    "PRJ[:digit:]+", ...
  )
}
