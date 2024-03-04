#' Extract Project Codes (a.k.a. PRJ number)
#' @rdname wd_str_extract
#' @name str_extract_project_code
str_extract_project_code <- function(string) {
  str_extract(
    string,
    "PRJ[:digit:]+"
  )
}
