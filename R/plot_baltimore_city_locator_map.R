#' Plot a Baltimore City locator map
plot_baltimore_city_locator_map <- function(
    data = NULL,
    size = 3,
    shape = 21,
    fill = "black",
    alpha = 1,
    color = "white",
    stroke = (size / 7) + 0.1,
    # stroke = (size / 4) + 0.3,
    border_color = "gray10", # "#00415F",
    border_linewidth = 0.4,
    project_layer = NULL,
    basemap = NULL,
    boundaries = NULL,
    location = "centroid",
    limit_bbox = TRUE,
    ...) {
  # basemap <- basemap %||% read_basemap_rds("baltimore_locator_basemap")
  # TODO: Updated 2024-04-11 to fill in
  basemap <- basemap %||% read_basemap_rds("24510_county_basemap")

  # FIXME: Making boundaries into a parameter only works if the basemap supports
  # a boundaries parameter
  boundaries <- boundaries %||% read_basemap_rds("community_areas")
  boundaries <- sf::st_filter(boundaries, data)

  if (location == "centroid") {
    project_locations <- suppressWarnings(sf::st_centroid(data))
  }

  if (is_function(project_layer)) {
    project_layer <- project_layer(data = project_locations)
  }

  project_layer <- project_layer %||% geom_sf(
    data = project_locations,
    size = size,
    shape = shape,
    alpha = alpha,
    fill = fill,
    color = color,
    stroke = stroke
  )

  map_limits <- list()

  if (limit_bbox) {
    baltimore_city_bbox <- read_basemap_rds("baltimore_city_bbox")

    map_limits <- list(
      coord_sf(
        xlim = c(baltimore_city_bbox$xmin, baltimore_city_bbox$xmax),
        ylim = c(baltimore_city_bbox$ymin, baltimore_city_bbox$ymax),
        expand = FALSE
      )
    )
  }

  baltimore_city <- read_basemap_rds("baltimore_city")

  basemap +
    geom_sf(
      data = boundaries,
      fill = "#FCB826",
      color = "#FDD06E",
      alpha = 0.9,
      linewidth = 0.3
    ) +
    geom_sf(
      data = baltimore_city,
      color = border_color,
      # color = "gray20",
      fill = NA,
      linewidth = border_linewidth
    ) +
    project_layer +
    map_limits
}
