pull_location_data <- function(report_data) {
  stopifnot(has_name(report_data, "location_data"))

  location_data <- report_data[["location_data"]] |>
    list_rbind()

  if (nrow(location_data) == 0) {
    return(location_data)
  }

  if (!inherits(location_data, "sf")) {
    location_data <- location_data |>
      sf::st_as_sf()
  }

  location_data |>
    filter_st_is_not_empty()
}
