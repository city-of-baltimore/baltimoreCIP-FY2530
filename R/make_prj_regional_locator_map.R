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
