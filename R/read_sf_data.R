read_sf_data <- function(dsn, ..., sf_col = "geometry", crs = 3857) {
  stopifnot(file.exists(dsn))

  obj <- sf::read_sf(dsn, ...)

  if (attr(obj, "sf_column") != sf_col) {
    obj <- obj |>
      sf::st_set_geometry(sf_col)
  }

  sf::st_transform(obj, crs = crs)
}
