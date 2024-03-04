format_project_locations <- function(data,
                                     location_updates) {
  cli::cli_alert_info(
    "Formatting {.arg location} column"
  )

  data |>
    naniar::replace_with_na(
      list(
        location = c("N/A", "NA", "TBD")
      )
    ) |>
    # FIXME: Location updates should be made in Adaptive - not here
    left_join_coalesce(
      location_updates,
      "location",
      "location_updated",
      by = "project_code"
    ) |>
    mutate(
      location = str_remove(location, "\\(N/A\\)")
    ) |>
    # FIXME: Expand on this approach to separate address from location name
    mutate(
      location = case_match(
        location,
        "Ashburton WFP" ~ "Ashburton Water Filtration Plant",
        "Montebello WFP" ~ "Montebello Water Filtration Plant",
        "3901 Hillen Road" ~ "Montebello Water Filtration Plant",
        .default = location
      ),
      location_asset_id = case_match(
        location,
        "Ashburton Water Filtration Plant" ~ "S07900",
        "Montebello Water Filtration Plant" ~ "S07999",
        .default = location_asset_id
      ),
      location_asset_id = case_when(
        # FIXME: Check these
        is.na(location_asset_id) & location == "6100 Quarantine Road" ~ "B06031",
        is.na(location_asset_id) & location == "111 Kane Street" ~ "B01050",
        is.na(location_asset_id) & location == "2030 Cromwell Bridge Road" ~ "C06525",
        .default = location_asset_id
      )
    ) |>
    select(!c(bridge_asset_id))
}
