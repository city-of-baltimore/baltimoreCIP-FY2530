intersects_baltimore_city <- function(x) {
  y <- sf::st_transform(
    mapbaltimore::baltimore_city,
    crs = sf::st_crs(x)
  )

  is_st_intersects(x, y)
}

is_st_intersects <- function(x, y, sparse = FALSE) {
  x <- filter(x, !sf::st_is_empty(x))

  intersects <- sf::st_intersects(x, y, sparse = sparse)

  # if (!sparse && dim(intersects)[[2]] == 1) {
  if (!sparse) {
    return(as.logical(intersects))
  }

  intersects
}

## ---- plot_prj_locator_map
plot_prj_locator_map <- function(data = NULL,
                                 id = NULL,
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
  data <- data %||% get_asset_by_id(id)

  if (!is.data.frame(data)) {
    data <- data |>
      purrr::list_rbind() |>
      sf::st_as_sf() |>
      dplyr::filter(!is.na(asset_id))

    return(data)

    if (nrow(data) == 0) {
      return(invisible(NULL))
    }
  }

  county_col <- "county"

  if (has_name(data, "asset_county")) {
    county_col <- "asset_county"
  }

  # Remove empty geometries
  data <- data |>
    filter(!sf::st_is_empty(geometry))

  needs_msa_map <- !all(data[[county_col]] %in% "Baltimore city") # ||
  # !all(intersects_baltimore_city(data))

  if (needs_regional_map) {
    msa_map <- plot_baltimore_msa_locator_map(
      data,
      size = size,
      shape = shape,
      stroke = stroke,
      border_color = border_color,
      border_linewidth = border_linewidth,
      ...
    )

    return(msa_map)
  }

  plot_baltimore_city_locator_map(
    data,
    size = size,
    shape = shape,
    stroke = stroke,
    border_color = border_color,
    border_linewidth = border_linewidth,
    project_layer = project_layer,
    basemap = basemap,
    boundaries = boundaries,
    location = location,
    limit_bbox = limit_bbox,
    ...
  )
}
