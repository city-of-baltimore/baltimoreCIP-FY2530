#' Flag demolished buildings and structures based on `asset_name` and `asset_type`
#'
#' @param data description
derive_demolished_col <- function(asset_data) {
  if (!has_name(asset_data, "demolished")) {
    asset_data <- mutate(
      asset_data,
      demolished = FALSE
    )
  }

  if (has_name(asset_data, "asset_name")) {
    asset_data <- mutate(
      asset_data,
      demolished = str_detect(asset_name, "DEMOLISHED|Demolished") | demolished
    )
  }


  if (has_name(asset_data, "asset_type")) {
    asset_data <- mutate(
      asset_data,
      # FIXME: This previously used asset_type == "DEMOLISHED" - make sure this
      # doesn't flag anything as demolished that isn't actually demolished
      demolished = str_detect(asset_type, "DEMOLISHED|DEMO") | demolished
    )
  }

  asset_data
}
