#' Read Workday Project Hierarchy-Baltimore City Public Entity Crosswalk
#'
#' `p_hierarchy1_xwalk` and `p_hierarchy2_xwalk` are crosswalks between the
#' responsible agency identified in the p_hierarchy1 and p_hierarchy2 columns
#' from Workday and a standard public entity name. This crosswalk is used by
#' `dop_cip_projects.R` to prep `project_details` and other objects
load_p_hierarchy_xwalks <- function(
    url = "https://airtable.com/app1lcJCwi0mpQGqZ/tbl81zsVzjBxVZePB/viwlrhbxPrDasYqzp?blocks=hide",
    cell_format = "string",
    ...) {
  # pak::pkg_install("matthewjrogers/rairtable@dev")
  # FIXME: This doesn't need to be a live link — replace with a CSV export

  entity_xwalk <- rairtable::list_records(url = url, cell_format = cell_format, ...)

  # Read crosswalk for Project Hierarchy 1
  p_hierarchy1_xwalk <- entity_xwalk |>
    filter(
      source == "Adaptive Planning CIP Reports - Project Hierarchy 1"
    ) |>
    select(p_hierarchy1_code = id, agency_label = entity)

  # Read crosswalk for Project Hierarchy 2
  p_hierarchy2_xwalk <- entity_xwalk |>
    filter(
      source == "Adaptive Planning CIP Reports - Project Hierarchy 2"
    ) |>
    select(p_hierarchy2_code = id, division = entity)

  list(
    "p_hierarchy1_xwalk" = p_hierarchy1_xwalk,
    "p_hierarchy2_xwalk" = p_hierarchy2_xwalk
  )
}
