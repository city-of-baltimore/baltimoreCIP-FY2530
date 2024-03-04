#' Extract an Agency Contract Identifier
#'
#' @param pattern Pattern used to extract
#' @param extract_all If `TRUE`, use [stringr::str_extract_all()] and return a
#'   list of extracted identifiers. This supports multiple identifiers per
#'   string.
str_extract_agency_contract_id <- function(
    string,
    pattern = "(TR |Tr |TR|TR-|SWC|SWC |SWC-|WC|WC-|WC |SDC|SDC |SDC-|SC |SC|SC-|ER |ER-|ER)[:digit:]+",
    extract_all = FALSE,
    ...) {
  fn <- str_extract

  if (extract_all) {
    fn <- str_extract_all
  }

  string <- fn(
    string,
    pattern,
    ...
  )

  remove_pattern <- "[:space:]|[:punct:]"

  if (!is.list(string)) {
    return(str_remove_trim(toupper(string), pattern = remove_pattern))
  }

  lapply(
    string,
    \(x) {
      str_remove_trim(toupper(x), pattern = remove_pattern)
    }
  )
}
