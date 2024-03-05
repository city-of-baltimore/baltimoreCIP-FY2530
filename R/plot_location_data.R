plot_location_data <- function(data, ...) {
  if (has_name(data, "location_data")) {
    location_data <- pull_location_data(data)
  } else {
    stopifnot(inherits_any(data, "sf"))
    location_data <- data
  }

  stopifnot(has_name(location_data, "county"))
  location_counties <- location_data |>
    filter(!is.na(county)) |>
    pull(county)

  if (all(location_counties == "Baltimore city")) {
    return(plot_baltimore_city_locator_map(data = location_data, ...))
  }

  plot_baltimore_msa_locator_map(data = location_data, ...)
}
