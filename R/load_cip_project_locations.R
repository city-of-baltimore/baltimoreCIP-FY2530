#' Load CIP Project locations
#'
load_cip_project_locations <- function(
    project_data,
    asset_data,
    project_asset_xwalk,
    agency_reference,
    additional_data = NULL,
    nested = TRUE) {
  project_locations <- project_data |>
    select(
      project_code,
      agency_label,
      location_asset_id
    ) |>
    left_join(
      select(project_asset_xwalk, asset_id, project_code),
      by = join_by(project_code),
      na_matches = "never"
    ) |>
    mutate(
      asset_id = coalesce(
        asset_id,
        location_asset_id
      )
    ) |>
    left_join(
      select(asset_data, all_of(c("asset_id", "asset_name", "asset_agency_label", "county"))),
      by = join_by(asset_id),
      na_matches = "never"
    ) |>
    left_join(
      agency_reference |>
        select(agency_label, agency_short_name),
      by = join_by(agency_label),
      na_matches = "never"
    ) |>
    bind_rows(
      additional_data
    )

  if (inherits(project_locations, "sf")) {
    # print("project_locations is an sf object!")

    project_locations <- project_locations |>
      filter(
        !sf::st_is_empty(geometry)
      )
  }

  if (!nested) {
    return(project_locations)
  }

  project_locations |>
    nest_by(project_code, .key = "location_data", .keep = TRUE)
}
