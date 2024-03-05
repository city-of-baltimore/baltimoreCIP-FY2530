pull_location_data <- function(report_data) {
  stopifnot(has_name(report_data, "location_data"))

  location_data <- list_rbind(report_data[["location_data"]]) |>
    sf::st_as_sf()

  location_data |>
    dplyr::filter(!sf::st_is_empty(.data[[attr(location_data, "sf_column")]]))
}
