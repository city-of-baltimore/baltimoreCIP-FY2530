#' Combine a list of sf objects
#'
sf_list_rbind <- function(.list,
                          crs = 3857,
                          sf_column = "geometry",
                          names_to = purrr::zap()) {
  .list <- map(
    .list,
    \(x) {
      stopifnot(inherits(x, "sf"))

      x |>
        sf::st_transform(crs) |>
        sf::st_set_geometry(sf_column) |>
        sf::st_make_valid()
    }
  )

  .df <- purrr::list_rbind(.list, names_to = names_to)

  sf::st_as_sf(.df)
}

#' Convert a sf list to a data frame list
#'
sf_list_to_df_list <- function(
    .list,
    coords = c("lon", "lat")) {
  map(
    .list,
    \(x) {
      if (inherits_any(x, "sf")) {
        if (!all(has_name(x, coords))) {
          x <- bind_coords(x, coords = coords)
        }

        x <- sf::st_drop_geometry(x)
      }

      x
    }
  )
}
