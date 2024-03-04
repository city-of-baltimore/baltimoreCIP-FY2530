derive_location_asset_id_col <- function(data) {
  check_names(data, must.include = c("location", "project_name"))

  cli_alert_new_col(
    c("location_asset_id", "contract_num"),
    c("location", "project_name")
  )

  data |>
    mutate(
      # TODO: This only works if the asset ID is enclosed in parentheses. DGS does
      # this but other agencies may not
      location_asset_id = str_extract(
        location,
        "(?<=\\()[:alnum:]+(?=\\))"
      ),
      bridge_asset_id = str_extract_bridge_asset_id(project_name),
      # FIXME: This isn't working as expected
      # project_name = str_remove(
      #   project_name,
      #   bridge_asset_id
      # ),
      location_asset_id = coalesce(
        location_asset_id,
        bridge_asset_id
      ),
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
