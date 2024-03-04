#' Read the Adaptive exports data dictionary sheet
#'
#' @keywords adaptive
read_adaptive_dictionary <- function(
    sheet = "adaptive_dictionary",
    ...) {
  read_sheet_ext(
    sheet = sheet,
    .f = \(x) {
      x |>
        mutate(
          rename_flag = (requests_clean_names != import_clean_names)
        )
    },
    ...
  )
}
