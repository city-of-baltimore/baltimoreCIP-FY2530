#' Extract a leading Spend Category Code
#' @rdname wd_str_extract
str_extract_spend_category_code <- function(string) {
  str_extract(
    string,
    "^SC[:digit:]+"
  )
}
