#' Create a short asset name column by abbreviated specified terms
#'
#' Must include column names "asset_name_short" and "asset_name"
#'
format_asset_name_short <- function(asset_data) {
  check_names(asset_data, must.include = "asset_name")
  cli_alert_new_col("asset_name_short", "asset_name")

  mutate(
    asset_data,
    asset_name_short = asset_name,
    asset_name_short = str_replace(
      asset_name_short,
      "Building",
      "Bldg"
    ),
    asset_name_short = str_replace(
      asset_name_short,
      "Baltimore City Police",
      "BCPD"
    ),
    asset_name_short = str_replace(
      asset_name_short,
      "Department of Transportation",
      "DOT"
    ),
    asset_name_short = str_replace(
      asset_name_short,
      "Recreation Center",
      "Rec Center"
    ),
    asset_name_short = str_replace(
      asset_name_short,
      "Community Action Center",
      "CAC"
    )
  )
}
