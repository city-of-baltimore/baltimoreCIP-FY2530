load_dot_locations <- function(project_asset_xwalk = NULL) {
  dot_intersections <- load_dot_intersections(project_asset_xwalk)

  fy25_dot_locations <- read_fy25_dot_locations()

  dot_locations <- purrr::list_rbind(
    list(
      fy25_dot_locations,
      dot_intersections
    )
  )

  sf::st_as_sf(dot_locations)
}
