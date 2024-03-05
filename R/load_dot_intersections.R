#' Load selection intersections from `mapbaltimore::intersections`
#'
#' Use project_asset_xwalk as the input for intersections
#'
load_dot_intersections <- function(intersections) {
  if (is.data.frame(intersections) && has_name(intersections, "asset_id")) {
    intersections <- intersections |>
      filter(
        str_detect(
          asset_id,
          "^INT"
        )
      ) |>
      pull(asset_id)
  }

  expect_character(intersections)

  intersections <- str_extract(intersections, "[:digit:]+")

  intersection_data <- mapbaltimore::named_intersections |>
    sf::st_transform(3857) |>
    filter(
      id %in% intersections
    ) |>
    sf::st_set_geometry("geometry")

  intersection_data |>
    mutate(
      asset_id = paste0("INT:", id),
      asset_name = str_to_title(name),
      asset_agency = "Department of Transportation",
      agency_abb = "DOT",
      asset_id_type = "INT",
      right_of_way = TRUE,
      demolished = FALSE
    ) |>
    # FIXME: Why are the columns assigned if only three columns are selected
    select(
      asset_id, asset_name, agency_abb
    ) |>
    bind_coords()
}
