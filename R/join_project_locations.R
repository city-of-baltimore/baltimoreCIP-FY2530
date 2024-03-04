## ---- join_project_locations ----
join_project_locations <- function(project_details_data,
                                   project_asset_xwalk,
                                   asset_parcels) {
  project_locations <- prep_project_locations(
    project_details_data = project_details_data,
    project_asset_xwalk = project_asset_xwalk,
    asset_parcels = asset_parcels
  )

  project_locations_nested <- nest_project_locations(project_locations)

  project_details_data |>
    # Join formatted project details to nested list column with locations
    left_join(
      project_locations_nested,
      by = join_by(project_code),
      na_matches = "never"
    ) |>
    select(!location_asset_id) |>
    # Join formatted project details to first location asset ID (to support legacy code)
    left_join(
      project_locations |>
        select(
          project_code,
          asset_name,
          asset_address,
          location_asset_id,
          asset_agency_label
        ),
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
