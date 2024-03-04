nest_project_locations <- function(project_locations) {
  # Created a nested list column and a location count project_locations
  project_locations |>
    nest_by(project_code, .key = "location_data", .keep = FALSE) |>
    left_join(
      project_locations |>
        count(project_code, name = "location_count"),
      # FIXME: This is returning a warning due to an unexpected many-to-many
      # relationship
      relationship = "one-to-one",
      by = join_by(project_code),
      na_matches = "never"
    )
}
