#' Bind coordinate columns to a sf data frame
#'
#' @param data A sf object.
#' @inheritParams convert_to_coords
#' @inheritDotParams convert_to_coords
bind_coords <- function(data, coords = c("lon", "lat"), ...) {
  testthat::expect_s3_class(data, "sf")

  sf_column <- attr(data, "sf_column")

  data <- purrr::list_cbind(
    list(
      data,
      convert_to_coords(
        data,
        coords = coords,
        ...
      )
    )
  )

  data |>
    sf::st_as_sf() |>
    dplyr::relocate(
      all_of(coords),
      .before = any_of(sf_column)
    )
}
