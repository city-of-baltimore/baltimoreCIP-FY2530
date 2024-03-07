#' Load data for DHCD Impact Investment areas
#'
load_dhcd_iia <- function(
    ...,
    url = "https://services5.arcgis.com/U5lRs16ODaohcqOy/ArcGIS/rest/services/INSPIRE_IIA_Dashboard/FeatureServer/1",
    # url = "https://geodata.baltimorecity.gov/egis/rest/services/Housing/dmxFocusAreas/MapServer/0",
    crs = 3857) {
  impact_investment_areas_src <- arcgislayers::arc_read(
    url = url,
    crs = crs
  )

  impact_investment_areas_src <- sf::st_make_valid(impact_investment_areas_src)

  impact_investment_areas <- impact_investment_areas_src |>
    mutate(
      asset_name = paste0(stringr::str_to_title(Name), " Impact Investment Area"),
      asset_name_alt = stringr::str_to_title(Name),
      asset_name_short = paste0(asset_name_alt, " IIA"),
      asset_id = paste0("DHCD:IIA", stringr::str_pad(OBJECTID, pad = "0", width = 2)),
      asset_id_type = "DHCD",
      asset_type_label = "Area",
      agency_abb = "DHCD",
      agency_name = "Department of Housing and Community Development",
      city = "Baltimore",
      right_of_way = FALSE,
      demolished = FALSE,
      geometry = geometry,
      .keep = "none"
    )

  impact_investment_areas |>
    bind_cols(
      suppressWarnings(convert_to_coords(impact_investment_areas))
    ) |>
    left_join(
      st_join_mapbaltimore(impact_investment_areas) |>
        sf::st_drop_geometry() |>
        select(!park),
      by = join_by(asset_id)
    ) |>
    relocate(
      lat,
      lon,
      .after = asset_type_label
    )
}
