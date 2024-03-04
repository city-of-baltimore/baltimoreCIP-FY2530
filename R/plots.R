read_basemap_rds <- function(file) {
  path <- path_user_data("basemap", paste0(file, ".rds"))

  readr::read_rds(path)
}

# base_size <- 12
# family_mono <- "Source Code Pro"
# family_sans <- "Raleway"
# family_mono <- "Raleway"

## ---- locator_basemap_theme
#' Create a locator basemap theme
locator_basemap_theme <- function() {
  check_installed("maplayer")

  list(
    theme_void(),
    theme(
      panel.border = element_blank(),
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_blank()
    ),
    maplayer::theme_sf_axis()
  )
}

#' Plot project locations using `plot_prj_locator_map`
#'
#' This function handles the nested data frame column with project location data
#' before calling the map-making function [plot_prj_locator_map()]
#'
#' @inheritParams plot_prj_locator_map
#' @inheritDotParams plot_prj_locator_map
plot_project_locations <- function(data,
                                   size = 2,
                                   ...,
                                   asset_data = NULL) {
  if (has_name(data, "location_data")) {
    data <- purrr::list_rbind(data$location_data)

    if (is_empty(data)) {
      return(invisible(NULL))
    }

    data <- distinct(data, location_asset_id, .keep_all = TRUE)
  }

  if (has_name(data, "location_asset_id")) {
    data <- data |>
      # FIXME: Why do some input data sources not have a project_code value?
      select(
        asset_id = location_asset_id,
        any_of("project_code"),
        any_of("asset_county")
      )
  }

  if (!inherits_all(data, "sf")) {
    data <- asset_data |>
      filter(
        .data[["asset_id"]] %in% data[["asset_id"]]
      )
    # select(
    #   !asset_county
    # ) |>
    # right_join(
    #   asset_list,
    #   by = join_by(asset_id),
    #   unmatched = "drop"
    # ) |>
    # sf::st_as_sf()
  }


  if (has_name(data, "project_code")) {
    data <- data |>
      filter(!is.na(project_code))
  }

  plot_prj_locator_map(
    data = data,
    ...,
    size = size
  )
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

  county_col <- "county"

  if (has_name(data, "asset_county")) {
    county_col <- "asset_county"
  }

  # Remove empty geometries
  data <- data |>
    filter(!sf::st_is_empty(geometry))

  if (!all(data[[county_col]] %in% "Baltimore city")) {
    regional_map <- make_prj_regional_locator_map(
      data,
      size = size,
      shape = shape,
      stroke = stroke,
      border_color = border_color,
      border_linewidth = border_linewidth,
      ...
    )

    return(regional_map)
  }

  basemap <- basemap %||% read_basemap_rds("baltimore_locator_basemap")
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

## make_prj_regional_locator_map ----
make_prj_regional_locator_map <- function(data = NULL,
                                          id = NULL,
                                          size = 3,
                                          shape = 21,
                                          stroke = (size / 6),
                                          # color = "#FCB826",
                                          color = "white",
                                          fill = "black",
                                          alpha = 0.8,
                                          # fill = "#FCB826",
                                          # color = "black",
                                          border_color = "#00415F",
                                          border_linewidth = 0.5,
                                          mask_dist = 0.125,
                                          mask_unit = "mi",
                                          basemap = NULL) {
  data <- data %||% get_asset_by_id(id)

  basemap <- basemap %||%
    read_basemap_rds("baltimore_msa_locator_basemap")

  baltimore_city_buffers_poly <- read_basemap_rds("baltimore_city_buffers_poly")
  baltimore_msa_counties <- read_basemap_rds("baltimore_msa_counties")

  select_msa_counties <- baltimore_msa_counties |>
    sf::st_filter(data)

  map_area_buffer <- baltimore_city_buffers_poly |>
    sf::st_filter(data) |>
    slice_min(order_by = dist, n = 1)

  map_area_bbox <- map_area_buffer |>
    sfext::st_buffer_ext(dist = mask_dist, unit = mask_unit) |>
    sf::st_bbox()

  map_area <- map_area_bbox |>
    sf::st_as_sfc() # |>
  # sfext::st_buffer_ext(dist = mask_dist, unit = mask_unit)

  map_area_mask <- list(
    geom_sf(
      data = map_area |>
        sfext::st_buffer_ext(dist = mask_dist * 5, unit = mask_unit) |>
        rmapshaper::ms_erase(map_area_buffer),
      fill = "white",
      color = NA,
      linewidth = 0
    ),
    geom_sf(
      data = map_area_buffer,
      fill = NA,
      color = border_color,
      # color = "black",
      linewidth = border_linewidth
    ),
    coord_sf(
      xlim = c(map_area_bbox$xmin, map_area_bbox$xmax),
      ylim = c(map_area_bbox$ymin, map_area_bbox$ymax),
      expand = FALSE
    )
  )

  project_data <- suppressWarnings(sf::st_centroid(data))

  basemap +
    geom_sf(
      data = baltimore_msa_counties |>
        rmapshaper::ms_dissolve() |>
        rmapshaper::ms_clip(sf::st_as_sfc(map_area_bbox)),
      fill = NA,
      color = border_color,
      alpha = 0.8,
      linewidth = border_linewidth * 0.8
    ) +
    # geom_sf(
    #   data = select_msa_counties,
    #   # fill = "#FCB826",
    #   fill = NA,
    #   color = "#FCB826",
    #   alpha = 0.9,
    #   linewidth = 0.3
    # ) +
    # geom_sf(
    #   data = project_data,
    #   size = size * 1.25,
    #   fill = "#FCB826",
    #   color = NA,
    #   alpha = 1
    # ) +
    geom_sf(
      data = project_data,
      size = size * 1.4,
      shape = 19,
      color = "#FCB826",
      alpha = 1
    ) +
    geom_sf(
      data = project_data,
      size = size,
      shape = shape,
      fill = fill,
      stroke = stroke,
      alpha = alpha,
      color = color
    ) +
    map_area_mask +
    locator_basemap_theme()
}

## ---- plot_agency_project_map
# plot_agency_project_map <- function(data,
#                                     basemap = NULL,
#                                     project_layer = list(),
#                                     border_color = "black",
#                                     border_linewidth = 0.2,
#                                     limit_bbox = TRUE) {
#   basemap <- basemap %||% read_basemap_rds("baltimore_locator_basemap")
#
#   map_limits <- list()
#
#   if (limit_bbox) {
#     baltimore_city_bbox <- read_basemap_rds("baltimore_city_bbox")
#
#     map_limits <- list(
#       coord_sf(
#         xlim = c(baltimore_city_bbox$xmin, baltimore_city_bbox$xmax),
#         ylim = c(baltimore_city_bbox$ymin, baltimore_city_bbox$ymax),
#         expand = FALSE
#       )
#     )
#   }
#
#   baltimore_city <- read_basemap_rds("baltimore_city")
#
#   basemap +
#     geom_sf(
#       data = baltimore_city,
#       color = border_color,
#       # color = "gray20",
#       fill = NA,
#       linewidth = border_linewidth
#     ) +
#     project_layer +
#     map_limits
# }
