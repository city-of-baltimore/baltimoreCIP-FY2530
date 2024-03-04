#' Load and simplify `baltimore_city` data from the `{mapbaltimore}` package
#'
#' @inheritParams sf::st_transform
#' @importFrom sf st_transform
#' @importFrom rmapshaper ms_simplify
load_baltimore_city <- function(crs = 3857) {
  mapbaltimore::baltimore_city |>
    sf::st_transform(crs = crs) |>
    rmapshaper::ms_simplify()
}
