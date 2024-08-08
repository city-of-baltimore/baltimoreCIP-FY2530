#' Derive a location asset ID column, bridge asset ID, and contract number
#' column value from the project name
derive_location_asset_id_col <- function(data) {
  check_names(data, must.include = c("location", "project_name"))

  cli_alert_new_col(
    c("location_asset_id", "contract_num"),
    c("location", "project_name")
  )

  data |>
    mutate(
      # FIXME: This only works if the asset ID is enclosed in parentheses
      location_asset_id = str_extract(
        location,
        "(?<=\\()[:alnum:]+(?=\\))"
      ),
      bridge_asset_id = str_extract_bridge_asset_id(project_name),
      location_asset_id = coalesce(location_asset_id, bridge_asset_id),
      contract_num = str_extract(
        project_name,
        "(Alley Co|Footway Co|Contract) ([:digit:]+|I{1,3})"
      ),
      .before = everything()
    ) |>
    naniar::replace_with_na(
      replace = list(
        location_asset_id = c("TBD", "CHM", "HUBS", "CCG", "BHIP", "Choice")
      )
    )
}
