## ---- join_project_locations ----
join_project_locations <- function(
    project_data,
    # location_data is assumed to be nested
    location_data,
    location_cols = c(
      "project_code",
      "asset_name",
      "asset_address",
      "location_asset_id",
      "asset_agency_label"
    )) {
  project_locations <- pull_location_data(location_data)

  stopifnot(
    !has_name(project_data, "location_data")
  )

  location_data <- location_data |>
    mutate(
      has_location = TRUE
    )

  project_data |>
    # Join formatted project details to nested list column with locations
    left_join(
      location_data,
      by = join_by(project_code),
      relationship = "one-to-one",
      na_matches = "never"
    ) |>
    tidyr::replace_na(
      list(has_location = FALSE)
    ) |>
    # Join formatted project details to first location asset ID (to support
    # legacy code)
    left_join(
      project_locations |>
        sf::st_drop_geometry() |>
        select(any_of(location_cols)),
      suffix = c("", "_location"),
      # TODO: Document that the location_asset_id for that column is just the
      # first and the nested location data should be used for maps
      multiple = "first",
      by = join_by(project_code),
      na_matches = "never"
    ) |>
    mutate(
      # FIXME: This should be documented
      location = if_else(
        is.na(location) & !is.na(location_asset_id),
        asset_name,
        location
      )
    )
}
