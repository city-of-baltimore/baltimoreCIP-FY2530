filter_st_is_not_empty <- function(x, ...) {
  x |>
    dplyr::filter(
      !sf::st_is_empty(.data[[attr(x, "sf_column")]])
    )
}
