## ---- kbl_summary ----
#'
#' Create a table with a grand summary row
#'
kbl_summary <- function(data,
                        cols = NULL,
                        .fns = \(x) {
                          sum(x, na.rm = TRUE)
                        },
                        replace_na = "---",
                        ...,
                        format = "latex",
                        hline_before = TRUE,
                        hline_after = TRUE) {
  if (!is.null(cols)) {
    sum_data <- data |>
      summarise(
        across(
          all_of(cols),
          .fns = .fns
        )
      )

    data <- bind_rows(data, sum_data)

    data <- data |>
      mutate(
        across(
          everything(),
          \(x) {
            str_replace_na(as.character(x), replacement = replace_na)
          }
        )
      )
  }

  tbl <- data |>
    kbl_tbl(format = format, ...)


  if (is.null(cols)) {
    return(tbl)
  }

  tbl |>
    kableExtra::row_spec(
      row = nrow(data) - 1,
      hline_after = hline_before
    ) |>
    kableExtra::row_spec(
      row = nrow(data),
      hline_after = hline_after
    )
}
