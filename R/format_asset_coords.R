#' Format coordinate columns from DGS asset data
#'
#' @param coords Coordinate column names. Not used wholly.
#' @inheritParams base::round
#' @inheritParams dgs_asset_data
format_asset_coords <- function(asset_data,
                                coords = c("lon", "lat"),
                                digits = 8) {
  stopifnot(
    all(has_name(asset_data, coords))
  )

  # FIXME: lat/lon are hard-coded values here despite being exposed as a parameter
  mutate(
    asset_data,
    # Fix issue with incorrectly formatted latitude field
    lat = if_else(
      str_detect(lat, ","),
      readr::parse_number(str_extract(lat, "([:blank:]|-|[:digit:]+)(?=,)")),
      readr::parse_number(lat)
    ),
    lon = readr::parse_number(lon),
    # Convert lat and lon into numeric values with 6 digits
    across(
      all_of(coords),
      \(x) {
        round(x, digits = digits)
      }
    )
  )
}
