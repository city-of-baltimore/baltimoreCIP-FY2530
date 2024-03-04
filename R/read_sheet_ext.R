#' Read sheet from specified path and optionally apply function
#'
#' @keywords targets
read_sheet_ext <- function(
    url = "https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing",
    sheet = NULL,
    .f = NULL,
    save = TRUE,
    path = c("_targets", "user", "data", "reference"),
    ...) {
  data <- googlesheets4::read_sheet(
    url,
    sheet = sheet
  )

  if (!is.null(.f)) {
    data <- .f(data)
  }

  if (!save) {
    return(data)
  }

  write_user_csv(
    data,
    file = sheet,
    path = path
  )
}
