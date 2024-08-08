#' Read project detail updates (names, descriptions, and locations) from a reference Google Sheet
#'
format_project_detail_updates <- function(project_detail_updates) {

  list(
    "project_name_updates" = project_detail_updates |>
      filter(
        !is.na(project_name_updated)
      ) |>
      select(
        project_code,
        project_name_updated
      ),
    "project_desc_updates" = project_detail_updates |>
      filter(
        !is.na(project_desc_updated)
      ) |>
      select(
        project_code,
        project_desc_updated
      ),
    "location_updates" = project_detail_updates |>
      filter(
        !is.na(location_updated)
      ) |>
      select(
        project_code,
        location_updated
      )
  )
}
