#' Create a where query for use with `arcgislayers::arc_read()`
where_in_sql <- function(needle, haystack, ..., .envir = current_env(), .con = DBI::ANSI()) {
  glue::glue_sql(haystack, " ", "IN", " ({needle*})", ..., .envir = .envir, .con = .con)
}
