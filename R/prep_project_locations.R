# ----- prep_project_locations
prep_project_locations <- function(project_details_data,
                                   project_asset_xwalk,
                                   asset_parcels = NULL) {
  project_asset_locations <- asset_parcels |>
    select(
      location_asset_id = asset_id,
      asset_name,
      asset_address = street_address,
      asset_agency_label = agency_name,
      asset_county = county
    ) |>
    filter(
      # Drop empty geometry
      !sf::st_is_empty(geometry)
    )

  project_details_data |>
    # Filter to projects with parsed or matched location asset ID values
    filter(!is.na(location_asset_id) | project_code %in% project_asset_xwalk$project_code) |>
    # TODO: Should this be select instead of distinct? Shouldn't be any different
    distinct(
      agency_label, project_code, project_name, location_asset_id
    ) |>
    # Combine with the project_asset_xwalk
    # Full join instead of left_join to handle projects with multiple matched assets
    full_join(
      project_asset_xwalk, # |>
      # FIXME: This is only using a subset of the crosswalk
      # filter(project_code_n == 1) |>
      # select(!ends_with("_n")),
      by = join_by(project_code),
      relationship = "many-to-many",
      na_matches = "never"
    ) |>
    # Fill missing location_asset_id values from crosswalk
    mutate(
      location_asset_id = coalesce(
        asset_id_updated,
        location_asset_id
      )
    ) |>
    select(!any_of(c("asset_id_updated", "match"))) |>
    # Join asset asset_agency_label to project locations
    left_join(
      project_asset_locations,
      # FIXME: This is returning a warning due to an unexpected many-to-many
      # relationship
      relationship = "many-to-one",
      by = join_by(location_asset_id),
      na_matches = "never"
    ) |>
    # FIXME: are the next two function calls actually needed?
    # mutate(
    #   location_asset_id = coalesce(
    #     asset_id_updated,
    #     location_asset_id
    #   )
    # ) |>
    # select(!c(asset_id_updated, update_asset_match)) |>
    # Blank asset_agency_label if it is the same as agency_label
    mutate(
      asset_agency_label = if_else(
        asset_agency_label == agency_label,
        NA_character_,
        asset_agency_label
      )
    ) |>
    relocate(
      agency_label,
      asset_agency_label,
      project_code,
      project_name,
      .before = everything()
    )
}
