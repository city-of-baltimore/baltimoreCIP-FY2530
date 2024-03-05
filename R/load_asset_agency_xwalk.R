#' Load DGS Asset List-Baltimore City Public Entity Crosswalk
#'
#' `asset_agency_xwalk` is a crosswalk between the responsible agency
#' identified in `Asset List .xlsx` and a standard public entity name. This
#' crosswalk is used by `prep_asset_list.R` to prep `asset_list`
#'
#' last-modified: 2023-11-10
load_asset_agency_xwalk <- function() {
  # pak::pkg_install("matthewjrogers/rairtable@dev")

  agency_xwalk <- rairtable::list_records(
    airtable = rairtable::airtable(
      base = "app1lcJCwi0mpQGqZ",
      table = "tbl81zsVzjBxVZePB"
    ),
    # TODO: Add the short name and agency abbreviation to this data
    fields = c("name", "entity", "source"),
    cell_format = "string",
    metadata = NULL
  )

  filter(
    agency_xwalk,
    source == "DGS Asset List"
  ) |>
    select(
      agency_abb = name,
      agency_name = entity
    )
}
