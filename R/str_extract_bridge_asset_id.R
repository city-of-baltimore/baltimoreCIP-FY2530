#' Extract bridge ID value from character vector
#'
#' @inheritParams stringr::str_extract
#' @param pattern A pattern to match the varied appearance of bridge ID values
#'   in capital project names.
str_extract_bridge_asset_id <- function(string,
                                        pattern = "(BC |BC|\\(BC |\\(BC)[:digit:]+") {
  str_remove_all(
    str_extract(
      string,
      pattern
    ),
    "\\(|[:blank:]"
  )
}
