#' Extract legacy capital account number or CIP number
#'
#' [str_extract_legacy_proj_num()] is a helper for extracting the legacy project
#' number from a project name. It is called by [format_prj_data()]
#' @rdname wd_str_extract
str_extract_legacy_proj_num <- function(string) {
  str_trim(str_extract(
    string,
    # FIXME: Is this a project number or an account number?
    "(^[:digit:]{6})|([:digit:]{3}-[:digit:]{3})"
  ))
}
