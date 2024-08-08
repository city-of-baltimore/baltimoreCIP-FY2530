#' Format project details report data from Adaptive Planning
#'
format_adaptive_project_data <- function(
    data,
    dictionary = NULL,
    dictionary_filename = "Capital_Projects_-_Project_Details.xlsx",
    project_detail_updates = NULL,
    project_exclusions = NULL,
    report_xwalks = NULL,
    agency_reference = NULL) {
  project_details_source <- data

  location_updates <- project_detail_updates[["location_updates"]]
  project_name_updates <- project_detail_updates[["project_name_updates"]]
  project_desc_updates <- project_detail_updates[["project_desc_updates"]]

  operating_budget_impact_xwalk <- report_xwalks[["operating_budget_impact_xwalk"]] |>
    mutate(
      impact_on_operating_budget = as.character(impact_on_operating_budget)
    )


  ## ---- format_project_details
  project_details_formatted <- project_details_source |>
    filter(
      # Filter rows appearing after "Total" summary row
      !cumany(.data[["Cost Center Code"]] == "Total"),
      !is.na(.data[["Project Code"]])
    ) |>
    format_adaptive_project_with_dictionary(
      dictionary = dictionary,
      dictionary_filename = dictionary_filename
    ) |>
    janitor::clean_names("snake") |>
    # Drop empty columns
    janitor::remove_empty("cols") |>
    select(
      # FIXME: Dropping columns because they are also in CIP requests but I need
      # to double-check that the values are identical in both sources
      !starts_with("fgs_grant_")
    ) |>
    filter_excluded_projects(
      exclusions = project_exclusions
    ) |>
    # Format adaptive data
    format_prj_data() |>
    # Format workday hierarchy
    format_p_hierarchy_cols(
      p_hierarchy_xwalks = report_xwalks[c("p_hierarchy1_xwalk", "p_hierarchy2_xwalk")]
    ) |>
    format_workday_names() |>
    mutate(
      date_beg = lubridate::parse_date_time(date_beg, "m/d/y"),
      date_end = lubridate::parse_date_time(date_end, "m/d/y")
    ) |>
    # rename(
    #   # FIXME: Is this renamed to avoid conflict with cip_requests when they are
    #   # joined to make project_requests? If so, should it just be dropped?
    #   cost_center_id = cost_center_code
    # ) |>
    derive_location_asset_id_col() |>
    derive_account_type_col() |>
    # Create reserve_type and project_type_short variables
    derive_project_type_short_col() |>
    str_squish_across() |>
    # FIXME: This should not be needed here
    distinct() |>
    # mutate(
    #   location_asset_id = case_when(
    #     # FIXME: These should probably be blanked out and the information added to
    #     # the asset-project where I can document any associated uncertainty
    #     # FIXME: Corrections to asset ID (incorrect input?)
    #     project_code == "PRJ000371" ~ "B00028",
    #     # Correction to parsed asset ID
    #     project_code == "PRJ002911" ~ "BC4105",
    #     # FIXME: Correction to bridge asset ID - that could potentially be
    #     # addressed in the asset_list prep by trimming the trailing C or X
    #     project_code == "PRJ002897" ~ "BC3556 C",
    #     project_code == "PRJ003231" ~ "BC6527 X",
    #     .default = location_asset_id
    #   )
    # ) |>
    mutate(
      flag_project_desc_invalid = case_when(
        p_description_name == project_code ~ TRUE,
        p_description_name == project_name ~ TRUE,
        str_length(p_description_name) < 50 ~ TRUE,
        str_detect(
          p_description_name,
          "^Provide additional supporting text detailing"
        ) ~ TRUE,
        .default = FALSE
      ) # ,
      # FIXME: Maintaining invalid project descriptions with expectation they will
      # be fixed.
      # p_description_name = case_when(
      #   p_description_name == project_code ~ NA_character_,
      #   p_description_name == project_name ~ NA_character_,
      #   str_length(p_description_name) < 50 ~ NA_character_,
      #   .default = p_description_name
      # )
    ) |>
    # Correct handling of NA values in year_of_impact column
    naniar::replace_with_na(
      list(
        year_of_impact = "NA",
        related_plan = c("N/A", "NA", "None")
      )
    ) |>
    tidyr::replace_na(
      replace = list(
        project_type_name = "Unspecified"
      )
    ) |>
    mutate(
      project_type_name_src = project_type_name,
      project_type_name = case_match(
        project_type_name,
        "State of Good Repair" ~ "Repair",
        "Community or Economic Development" ~ "Development",
        .default = project_type_name
      )
    ) |>
    mutate(
      project_type_name = forcats::fct_infreq(project_type_name),
      project_type_name = forcats::fct_rev(project_type_name)
    ) |>
    format_project_locations(
      location_updates = location_updates
    ) |>
    # FIXME: Project name updates should be made in Workday - not here
    left_join_coalesce(
      project_name_updates,
      "project_name",
      "project_name_updated",
      by = "project_code"
    ) |>
    left_join_coalesce(
      project_desc_updates,
      "p_description_name",
      "project_desc_updated",
      by = "project_code"
    ) |>
    relocate(
      agency_label,
      division,
      .before = everything()
    ) |>
    left_join(
      operating_budget_impact_xwalk,
      by = join_by(impact_on_operating_budget),
      relationship = "many-to-one",
      na_matches = "never"
    ) |>
    rename(
      # FIXME: This should be handled by the dictionary
      operating_impact = operating_budget_impact
    ) |>
    left_join(
      select(agency_reference, agency_label, agency_short_name, agency_label_abb),
      by = join_by(agency_label)
    )
}

filter_excluded_projects <- function(
    data,
    exclusions) {
  check_character(exclusions)

  data |>
    dplyr::filter(
      !(project_code %in% exclusions)
    )
}

format_adaptive_project_with_dictionary <- function(
    project_data,
    dictionary = NULL,
    dictionary_filename = "Capital_Projects_-_Project_Details.xlsx") {
  check_names(
    dictionary,
    must.include = c("adaptive_export_name", "adaptive_import_col_type")
  )

  # Filter dictionary to project details
  project_dictionary <- dplyr::filter(
    dictionary,
    !is.na(adaptive_export_name),
    adaptive_export_filename == dictionary_filename
  )

  # Subset columns with numeric values
  project_dictionary_numeric <- filter(
    project_dictionary,
    adaptive_import_col_type == "numeric"
  )

  project_data |>
    mutate(
      across(
        any_of(project_dictionary_numeric[["adaptive_export_name"]]),
        \(x) {
          if (is.character(x)) {
            readr::parse_number(x)
          } else {
            x
          }
        }
      )
    )
}
