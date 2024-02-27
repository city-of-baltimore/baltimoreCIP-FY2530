#' Convert a sf object to a data frame of coordinates
#'
#' @inheritParams base::round
convert_to_coords <- function(data, coords = c("lon", "lat"), digits = 8) {
  data |>
    sf::st_centroid() |>
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
