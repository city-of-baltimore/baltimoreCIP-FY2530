#' Load a subset of sheets from a reference Google Sheet based on a supplied pattern
#'
#' @keywords targets
load_sheets_list <- function(
    url = "https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing",
    pattern = NULL,
    sheets = NULL) {
  stopifnot(
    is_string(url)
  )

  sheets <- sheets %||% googlesheets4::sheet_names(url)

  if (!is.null(pattern)) {
    sheets <- str_subset(sheets, pattern)
  }

  sheets_data <- map(
    sheets,
    \(sheet) {
      googlesheets4::read_sheet(
        url,
        sheet = sheet
      )
    }
  )

  set_names(sheets_data, sheets)
}
