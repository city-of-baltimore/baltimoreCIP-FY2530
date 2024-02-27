#' Add asset_id_type and asset_type_label columns
#'
#' Must include columns named "asset_id", "asset_name", "asset_type"
format_asset_type_label <- function(asset_data) {
  check_names(asset_data, must.include = c("asset_id", "asset_name", "asset_type"))

  if (!has_name(asset_data, "asset_id_type")) {
    #' Create asset ID type column based on the leading prefix from the asset id column
    asset_data <- mutate(
      asset_data,
      asset_id_type = str_extract(asset_id, "[:alpha:]"),
      .after = asset_id
    )
  }

  mutate(
    asset_data,
    asset_type_label = case_when(
      asset_id_type == "L" ~ "Land",
      str_detect(asset_name, "Pool") & !str_detect(asset_name, "Building") ~ "Pool",
      str_detect(asset_name, "Seating") ~ "Seating",
      str_detect(asset_name, "Playground") ~ "Playground",
      str_detect(asset_name, "Modular") ~ "Modular building",
      str_detect(asset_type, "EQUIP") ~ "Equipment",
      str_detect(asset_type, "OPEN SPACE") ~ "Open space",
      str_detect(asset_type, "BUILDING") ~ "Building",
      str_detect(asset_type, "STRUCT") ~ "Structure",
      asset_type == "BCPSS ASSET" ~ "Building",
      str_detect(asset_name, "Building") ~ "Building",
      .default = "Other"
    ),
    .after = asset_type
  )
}
