#' Flag the asset start and end year based on common patterns in the asset name and asset type columns
flag_asset_start_end_year <- function(asset_data) {
  stopifnot(
    all(has_name(asset_data, c("asset_name"))) # , "asset_type")))
  )

  if (!has_name(asset_data, "start_year")) {
    asset_data <- mutate(
      asset_data,
      start_year = NA_integer_
    )
  }

  if (!has_name(asset_data, "end_year")) {
    asset_data <- mutate(
      asset_data,
      end_year = NA_integer_
    )
  }

  mutate(
    asset_data,

    # Extract start and end years from asset name
    start_year = case_when(
      str_detect(asset_name, "NEW 2021") ~ 2021,
      str_detect(asset_name, "NEW 2022") ~ 2022,
      str_detect(asset_name, "NEW 2013") ~ 2013,
      str_detect(asset_name, "- New 2013") ~ 2013,
      .default = start_year
    ),
    end_year = case_when(
      str_detect(asset_name, "DEMOLISHED-2021") ~ 2021,
      str_detect(asset_name, "DEMOLISHED 2021") ~ 2021,
      # str_detect(asset_type, "DEMO SUMMER 2021") ~ 2021,
      str_detect(asset_name, "DEMOLISHED 2020") ~ 2020,
      str_detect(asset_name, "DEMOLISHED-2020") ~ 2020,
      .default = end_year
    )
  )
}
