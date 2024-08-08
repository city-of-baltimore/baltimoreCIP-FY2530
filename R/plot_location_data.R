#' Plot a CIP Project location map
plot_location_data <- function(
    data, ...) {
  if (has_name(data, "location_data")) {
    location_data <- pull_location_data(data)
  } else {
    stopifnot(inherits_any(data, "sf"))
    location_data <- data
  }

  location_data <- location_data |>
    filter_st_is_not_empty()

  if (nrow(location_data) == 0) {
    return(invisible(NULL))
  }

  check_names(location_data, must.include = "county")

  if (all(has_county_value(location_data))) {
    return(plot_baltimore_city_locator_map(data = location_data, ...))
  }

  plot_baltimore_msa_locator_map(data = location_data, ...)
}
