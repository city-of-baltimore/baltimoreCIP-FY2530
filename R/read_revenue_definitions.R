#' Read the revenue definitions sheet
#'
read_revenue_definitions <- function(
    sheet = "revenue_definitions",
    ...) {
  read_sheet_ext(
    sheet = sheet,
    .f = \(x) {
      x |>
        filter(
          !is.na(pos)
        ) |>
        select(
          type, description
        )
    },
    ...
  )
}
