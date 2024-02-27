#' Cache iMap parcel data for Baltimore City
#'
#' FIXME: This function is incomplete and should not be used
#'
#' @inheritParams mapbaltimore::cache_baltimore_data
cache_baltimore_imap_parcels <- function(filename = "baltimore_imap_parcels",
                                         pkg = "mapbaltimore") {
  mapbaltimore::cache_baltimore_data(
    filename = filename,
    pkg = pkg
  )
}
