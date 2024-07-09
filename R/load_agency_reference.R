#' Load Baltimore City Agency reference data
load_agency_reference <- function(
    agency_overviews,
    entity_reference = baltimoredata::entity_reference) {
  agency_reference <- entity_reference |>
    mutate(
      agency_label = case_match(
        name,
        "Health Department" ~ "Baltimore City Health Department",
        # "Baltimore City Public School System" ~ "Baltimore City Public Schools",
        .default = name
      ),
      name_abb = case_match(
        name,
        "Maryland Zoo in Baltimore" ~ "Zoo",
        "Baltimore City Mayor's Office" ~ "Mayor",
        .default = name_abb
      ),
      entity_url = str_remove(
        entity_url,
        "/$"
      )
    ) |>
    select(
      agency_label,
      agency_name = name,
      agency_short_name = name_short,
      agency_label_abb = name_abb,
      agency_abb = name_abb,
      agency_url = entity_url,
      agency_start_year = start_year
    )

  agency_overviews |>
    select(
      agency_label, overview
    ) |>
    mutate(
      agency_label = case_match(
        agency_label,
        "Baltimore City Public School System" ~ "Baltimore City Public Schools",
        .default = agency_label
      )
    ) |>
    left_join(
      agency_reference,
      by = join_by(agency_label),
      relationship = "one-to-one",
      na_matches = "never"
    )
}
