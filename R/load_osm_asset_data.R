load_osm_asset_data <- function(
    url = "https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing",
    crs = 3857) {
  asset_osm_additions <- googlesheets4::read_sheet(
    url,
    sheet = "asset_osm_additions"
  ) |>
    filter(!is.na(id))

  asset_osm_additions_list <- purrr::pmap(
    asset_osm_additions,
    \(osm_id, osm_geometry, osm_id_type, ...) {
      getdata::get_osm_id(
        id = osm_id,
        type = osm_id_type,
        geometry = osm_geometry
      )
    }
  )

  asset_osm_additions_sf <- asset_osm_additions_list |>
    purrr::list_rbind() |>
    sf::st_as_sf() |>
    sf::st_transform(crs = crs) |>
    left_join(
      asset_osm_additions |>
        mutate(
          osm_id = as.character(osm_id)
        ),
      by = join_by(osm_id)
    ) |>
    mutate(
      geometry = if_else(
        asset_name == "Cab Calloway Legends Park",
        sf::st_point_on_surface(geometry),
        geometry
      ),
      street_address = if_else(
        !is.na(.data[["addr:housenumber"]]),
        glue(
          "{`addr:housenumber`} {`addr:street`}"
        ),
        NA_character_
      ),
      right_of_way = asset_name == "Potomac Street Cycletrack",
      demolished = FALSE
    ) |>
    select(
      starts_with("asset_"),
      street_address,
      osm_id,
      right_of_way,
      demolished
    )

  asset_osm_additions_sf |>
    bind_coords() |>
    relocate(
      lat,
      lon,
      .before = geometry
    )
}
