where_in_sql <- function(needle, haystack,..., .envir = current_env(), .con = DBI::ANSI()) {
  glue::glue_sql(haystack, " ", "IN", " ({needle*})", ..., .envir = .envir, .con = .con)
}
