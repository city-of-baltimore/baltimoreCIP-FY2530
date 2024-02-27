#' Complete spatial joins to planning distict, neighborhood, county, and park data from mapbaltimore
st_join_mapbaltimore <- function(data) {
  stopifnot(
    has_name(data, "asset_id"),
    inherits(data, "sf"),
    is_installed("mapbaltimore"),
    is_installed("sfext")
  )

  # Join to neighborhoods, planning districts, counties, and parks
  data |>
    sfext::st_join_ext(
      mapbaltimore::planning_districts |>
        select(planning_district = name),
      largest = TRUE,
      .id = "planning_district"
    ) |>
    sfext::st_join_ext(
      mapbaltimore::neighborhoods_2020 |>
        select(neighborhood = name),
      largest = TRUE,
      .id = "neighborhood"
    ) |>
    sfext::st_join_ext(
      mapbaltimore::baltimore_msa_counties |>
        select(county = namelsad),
      largest = TRUE,
      .id = "county"
    ) |>
    sfext::st_join_ext(
      mapbaltimore::parks |>
        select(park = name),
      largest = TRUE,
      .id = "park"
    ) |>
    select(
      asset_id, neighborhood, park, planning_district, county
    )
}
