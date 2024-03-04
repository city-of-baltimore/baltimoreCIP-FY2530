## ---- cols_width_ext
# https://github.com/rstudio/gt/issues/119
cols_width_ext <- function(data,
                           columns = NULL,
                           widths = NULL,
                           .list = NULL,
                           textwidth = 7.5,
                           dpi = 96,
                           unit = "in",
                           scale = TRUE) {
  if (is.null(.list)) {
    if (is.null(widths)) {
      return(data)
    }

    if (is_named(widths)) {
      columns <- columns %||% names(widths)
    }

    stopifnot(all(purrr::map_lgl(columns, is.character)))

    .list <- lapply(
      seq_along(widths),
      \(i) {
        new_formula(
          columns[[i]],
          perc(
            widths[[i]],
            textwidth = textwidth,
            dpi = dpi,
            unit = unit,
            scale = scale
          )
        )
      }
    )
  }

  cols_width(
    data,
    .list = .list
  )
}
