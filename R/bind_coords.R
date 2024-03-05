#' Bind coordinate columns
#'
#' @param data A sf object.
#' @inheritParams convert_to_coords
#' @inheritDotParams convert_to_coords
bind_coords <- function(data, coords = c("lon", "lat"), ...) {
  testthat::expect_s3_class(data, "sf")

  sf_column <- attr(data, "sf_column")

  data |>
    bind_cols(
      convert_to_coords(
        data,
        coords = coords,
        ...
      )
    ) |>
    relocate(
      all_of(coords),
      .before = any_of(sf_column)
    )
}
