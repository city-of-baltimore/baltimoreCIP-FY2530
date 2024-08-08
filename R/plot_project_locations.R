#' Plot project locations using `plot_prj_locator_map`
#'
#' This function handles the nested data frame column with project location data
#' before calling the map-making function [plot_prj_locator_map()]
#'
#' @inheritParams plot_prj_locator_map
#' @inheritDotParams plot_prj_locator_map
plot_project_locations <- function(data,
                                   size = 2,
                                   ...,
                                   asset_data = NULL) {
  if (has_name(data, "location_data")) {
    data <- purrr::list_rbind(data$location_data)
    data <- sf::st_as_sf(data)

    if (is_empty(data)) {
      return(invisible(NULL))
    }

    data <- distinct(data, location_asset_id, .keep_all = TRUE)
  }

  if (has_name(data, "location_asset_id")) {
    data <- data |>
      # FIXME: Why do some input data sources not have a project_code value?
      select(
        asset_id = location_asset_id,
        any_of("project_code"),
        any_of("asset_county")
      )
  }

  if (!inherits_any(data, "sf") && !is.null(asset_data)) {
    data <- asset_data |>
      filter(
        .data[["asset_id"]] %in% data[["asset_id"]]
      )
    # select(
    #   !asset_county
    # ) |>
    # right_join(
    #   asset_list,
    #   by = join_by(asset_id),
    #   unmatched = "drop"
    # ) |>
    # sf::st_as_sf()
  }


  if (has_name(data, "project_code")) {
    data <- data |>
      filter(!is.na(project_code))
  }

  plot_prj_locator_map(
    data = data,
    ...,
    size = size
  )
}
