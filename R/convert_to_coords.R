#' Convert a sf object to a data frame of coordinates
#'
#' @inheritParams base::round
convert_to_coords <- function(
    data,
    coords = c("lon", "lat"),
    .f = sf::st_centroid,
    ...,
    digits = 8) {
  data |>
    .f(...) |>
    suppressWarnings() |>
    sf::st_transform(4326) |>
    sf::st_coordinates() |>
    as.data.frame() |>
    set_names(coords) |>
    mutate(
      across(
        all_of(coords),
        \(x) {
          round(x, digits = digits)
        }
      )
    )
}
