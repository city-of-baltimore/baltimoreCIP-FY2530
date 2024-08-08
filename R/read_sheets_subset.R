#' Read a subset of sheets from a reference Google Sheet based on a supplied pattern
#'
#' @keywords targets
read_sheets_subset <- function(
    url = "https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing",
    pattern = "_xwalk$",
    sheets = NULL,
    save = TRUE,
    local = FALSE,
    path = fs::path("_targets", "user", "data", "reference")) {
  stopifnot(
    is_string(url),
    is_string(pattern)
  )

  if (!local) {
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
  } else {
    files <- fs::dir_ls(
      path,
      regexp = pattern
    )

    sheets <- fs::path_ext_remove(
      fs::path_file(files)
    )

    sheets_data <- map(
      files,
      \(file) {
        readr::read_csv(
          file,
          show_col_types = FALSE
        )
      }
    )
  }

  if (!save || local) {
    return(set_names(sheets_data, sheets))
  }

  walk2(
    sheets_data,
    sheets,
    \(x, y) {
      write_user_csv(
        data = x,
        file = y,
        path = path
      )
    }
  )

  set_names(sheets_data, sheets)
}
