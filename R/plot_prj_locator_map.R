#' Plot project locator map (Baltimore City or MSA)
plot_prj_locator_map <- function(data = NULL,
                                 size = 3,
                                 shape = 21,
                                 fill = "black",
                                 alpha = 1,
                                 color = "white",
                                 stroke = (size / 7) + 0.1,
                                 border_color = "gray10",
                                 border_linewidth = 0.4,
                                 project_layer = NULL,
                                 basemap = NULL,
                                 boundaries = NULL,
                                 location = "centroid",
                                 limit_bbox = TRUE,
                                 ...) {
  if (!is.data.frame(data)) {
    data <- data |>
      purrr::list_rbind() |>
      sf::st_as_sf()
  }

  # Remove empty geometries
  data <- data |>
    filter_st_is_not_empty()

  if (nrow(data) == 0) {
    return(invisible(NULL))
  }

  county_col <- "county"

  if (has_name(data, "asset_county")) {
    county_col <- "asset_county"
  }

  needs_msa_map <- !all(
    has_county_value(data, county_col = county_col)
  )

  if (needs_msa_map) {
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

#' Does a data frame county column have a specified county value?
has_county_value <- function(data,
                             value = "Baltimore City",
                             county_col = "county") {
  tolower(data[[county_col]]) %in% tolower(value)
}
