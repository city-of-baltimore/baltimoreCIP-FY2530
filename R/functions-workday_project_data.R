## ---- format_prj_data
#' Format data exported from Adaptive Planning
#'
#' [format_prj_data()] handles the reformatting of the common column names
#' between the two data exports from Adaptive Planning.
#'
format_prj_data <- function(data,
                            .before = NULL,
                            drop_columns = c(
                              "p_code_code", "p_group_code", "p_status_code",
                              "p_description_code", "p_percent_complete_code",
                              "p_problem_statement_code", "p_objective_code",
                              "p_in_scope_code", "p_outof_scope_code",
                              "p_measuresof_success_code", "p_project_overview_code",
                              "r_object_code"
                            )) {
  if (all(has_name(data, c("project_name", "project_code")))) {
    data <- format_project_name(data)

    if (has_name(data, "p_description_name")) {
      data <- data |>
        mutate(
          cip_number = coalesce(
            cip_number,
            # Legacy project codes are also appended to end of some descriptions
            # (mainly DOT)
            str_extract_cip_number(p_description_name)
          ),
          p_description_name = case_when(
            is.na(cip_number) ~ p_description_name,
            !is.na(cip_number) ~ str_remove_trim(p_description_name, cip_number)
          )
        )
    }
  }

  ##  ---- Cost Center ----

  if (all(has_name(data, c("cost_center_code", "cost_center_name")))) {
    data <- mutate(
      data,
      cost_center_cat = str_sub(cost_center_code, 1, 3),
      cost_center_name = str_remove_trim(cost_center_name, cost_center_code),
      cost_center_name = str_remove_trim(cost_center_name, cost_center_cat),
      .before = .before %||% everything()
    )
  }

  ##  ---- Priority and Importance levels ----

  if (all(has_name(data, c(
    "p_importance_code", "p_importance_name",
    "p_priority_code", "p_priority_name"
  )))) {
    data <- mutate(
      data,
      p_importance_code = str_extract(p_priority_code, "[:digit:]+"),
      p_importance_name = str_extract(p_priority_name, "[:alpha:]+"),
      p_priority_code = str_extract(p_priority_code, "[:digit:]+"),
      p_priority_name = str_extract(p_priority_name, "[:alpha:]+"),
      .before = .before %||% everything()
    )
  }

  ##  ---- Fund and grant ----

  if (all(has_name(data, c(
    "fgs_fund_code", "fgs_fund_name",
    "fund_grant_s_purpose_code",
    "fund_grant_s_purpose_name"
  )))) {
    data <- data |>
      format_fgs_fund_cols() |>
      format_fund_grant_s_purpose_code_cols()
  }

  # drop_columns <- drop_columns[drop_columns %in% names(data)]

  select(
    data,
    !any_of(drop_columns)
  )
}


format_fund_grant_s_purpose_code_cols <- function(data) {
  format_code_name_cols(
    data,
    code_col = "fund_grant_s_purpose_code",
    name_col = "fund_grant_s_purpose_name",
    pattern = "_\\[blank\\]"
  )
}

format_fgs_fund_cols <- function(data) {
  format_code_name_cols(
    data,
    code_col = "fgs_fund_code",
    name_col = "fgs_fund_name",
    pattern = "^([\\S]+)",
    .f = str_extract
  )
}

## ---- format_workday_names
format_workday_names <- function(data) {
  stopifnot(
    all(has_name(data, c("p_manager_code", "p_manager_name", "p_project_owner_code", "p_project_owner_name")))
  )

  ## Names
  data |>
    format_p_manager_cols() |>
    format_p_owner_cols()
}

format_p_manager_cols <- function(data) {
  data |>
    format_code_name_cols(
      code_col = "p_manager_code",
      name_col = "p_manager_name",
      pattern = "\\([:alnum:]+\\)",
      .f = str_extract
    ) |>
    mutate(
      p_manager_name = str_remove_trim(p_manager_name, "\\(\\)")
    )
}

format_p_owner_cols <- function(data) {
  data |>
    format_code_name_cols(
      code_col = "p_manager_code",
      name_col = "p_manager_name",
      pattern = "\\([:alnum:]+\\)",
      .f = str_extract
    ) |>
    mutate(
      p_project_owner_name = str_remove_trim(p_project_owner_name, "\\(\\)")
    )
}

## ---- format_workday_hierarchy
format_p_hierarchy_cols <- function(
    data,
    p_hierarchy_xwalks = NULL,
    ...) {
  stopifnot(
    all(has_name(data, c(
      "p_hierarchy1_code", "p_hierarchy2_code",
      "p_hierarchy1_name", "p_hierarchy2_name"
    )))
  )

  # FIXME: This is a manual correction that should be
  # removed when the project is updated in Workday
  data <- data |>
    mutate(
      ## PRJ002543 (added on 2024-01-10)
      p_hierarchy1_name = if_else(
        project_code == "PRJ002543",
        "PJH6100 Public Works",
        p_hierarchy1_name
      ),
      p_hierarchy1_code = if_else(
        project_code == "PRJ002543",
        "PJH6100 Public Works",
        p_hierarchy1_code
      ),
      p_hierarchy2_name = if_else(
        project_code == "PRJ002543",
        "PJHCIP0525 Capital Projects - Pollution and Erosion Control",
        p_hierarchy2_name
      ),
      p_hierarchy2_code = if_else(
        project_code == "PRJ002543",
        "PJHCIP0525 Capital Projects - Pollution and Erosion Control",
        p_hierarchy2_code
      ),

      ## PRJ002321 (added on 2024-01-12)
      p_hierarchy1_name = if_else(
        project_code == "PRJ002321",
        "PJH3100 Housing and Community Development",
        p_hierarchy1_name
      ),
      p_hierarchy1_code = if_else(
        project_code == "PRJ002321",
        "PJH3100 Housing and Community Development",
        p_hierarchy1_code
      ),
      p_hierarchy2_name = if_else(
        project_code == "PRJ002321",
        "PJHCIP0588 Capital Projects - Housing and Community Development",
        p_hierarchy2_name
      ),
      p_hierarchy2_code = if_else(
        project_code == "PRJ002321",
        "PJHCIP0588 Capital Projects - Housing and Community Development",
        p_hierarchy2_code
      )
    )

  data |>
    mutate(
      # Remove project names from hierarchy code columns
      p_hierarchy1_code = str_extract(p_hierarchy1_code, "^([\\S]+)"),
      p_hierarchy2_code = str_extract(p_hierarchy2_code, "^([\\S]+)"),
      # Remove project code from hierarchy name columns
      p_hierarchy1_name = str_remove_trim(p_hierarchy1_name, p_hierarchy1_code),
      p_hierarchy2_name = str_remove_trim(p_hierarchy2_name, p_hierarchy2_code)
    ) |>
    # Join project details to project hierarchy crosswalk
    left_join(
      p_hierarchy_xwalks[["p_hierarchy1_xwalk"]],
      by = join_by(p_hierarchy1_code),
      na_matches = "never"
    ) |>
    left_join(
      p_hierarchy_xwalks[["p_hierarchy2_xwalk"]],
      by = join_by(p_hierarchy2_code),
      na_matches = "never"
    ) |>
    ##  ---- Division and agency labels ----
    mutate(
      agency_label = coalesce(agency_label, division),
      agency_label = case_when(
        division == "Baltimore Development Corporation" ~ division,
        division == "Mayor's Office of Recovery Programs" ~ division,
        .default = agency_label
      ),
      division = if_else(
        division == agency_label,
        NA_character_,
        division
      )
    )
}

cli_alert_new_col <- function(new_col, existing_col) {
  cli::cli_alert_success(
    "Adding new {.arg {new_col}} column{?s} based on {.arg {existing_col}} column{?s}"
  )
}

## ---- format_account_type
derive_account_type_col <- function(data) {
  check_names(data, must.include = "project_name")

  cli_alert_new_col("account_type", "project_name")

  mutate(
    data,
    account_type = case_when(
      str_detect(
        project_name,
        "Unallocated Reserve"
      ) ~ "Unallocated Reserve",
      str_detect(
        project_name,
        "Construction Reserve"
      ) ~ "Construction Reserve",
      str_detect(
        project_name,
        "Reserve"
      ) ~ "Reserve",
      str_detect(
        project_name,
        "Active$|ACTIVE|Act$|ACT$|Active [:digit:]"
      ) ~ "Active",
      .default = NA_character_
    ),
    project_name = str_remove_all(
      project_name,
      "Active$|ACTIVE|Act$|ACT$|Active [:digit:]"
    )
  )
}

## ---- format_project_type_short
derive_project_type_short_col <- function(data) {
  check_names(data, must.include = "project_name")

  cli_alert_new_col("project_type_short", "project_name")

  mutate(
    data,
    # FIXME: This should potentially be split into multiple categories and
    # aligned with some standard values
    # TODO: This should also be moved into a function
    project_type_short = case_when(
      str_detect(
        project_name,
        "HVAC Replacement"
      ) ~ "HVAC Replacement",
      str_detect(
        project_name,
        "HVAC|Heat Pump"
      ) ~ "HVAC Repair/Upgrades",
      str_detect(
        project_name,
        "Elevator Replacement"
      ) ~ "Elevator Replacement",
      str_detect(
        project_name,
        "Elev"
      ) ~ "Elevator Repair/Upgrade",
      str_detect(
        project_name,
        "Fire Suppression|Fire Protection"
      ) ~ "Fire Protection Upgrades",
      str_detect(
        project_name,
        "Fire Alarm"
      ) ~ "Fire Alarm Improvements",
      str_detect(
        project_name,
        "Pump|Pumping"
      ) ~ "Pump Facility Upgrades",
      str_detect(
        project_name,
        "Boiler Replacement"
      ) ~ "Boiler Replacement",
      str_detect(
        project_name,
        "Waterproof|Intrusion|Basement Flooding"
      ) ~ "Waterproofing Improvements",
      str_detect(
        project_name,
        "Roof Repla"
      ) ~ "Roof Replacement",
      str_detect(
        project_name,
        "Roof"
      ) ~ "Roof Repair",
      str_detect(
        project_name,
        "Window Replace"
      ) ~ "Window Replacement",
      str_detect(
        project_name,
        "Footways"
      ) ~ "Footway Repair",
      str_detect(
        project_name,
        "Alleys"
      ) ~ "Alley Repair",
      str_detect(
        project_name,
        "Water Main|Pump|Pumping|Water Service"
      ) ~ "Water Supply Upgrades",
      str_detect(
        project_name,
        "Sewer|Drain|Siphon|Interceptor"
      ) ~ "Drainage/Sewer Upgrades",
      str_detect(
        project_name,
        "Stormwater|Storm Water|Bio Retention"
      ) ~ "Stormwater Mitigation",
      str_detect(
        project_name,
        "Traffic Signal"
      ) ~ "Traffic Signal Upgrades",
      str_detect(
        project_name,
        "Resufacing|Resurface"
      ) ~ "Resurfacing",
      str_detect(
        project_name,
        "Renovation|Bathroom|Lobby|Renovate"
      ) ~ "Building Renovation",
      str_detect(
        project_name,
        "Expansion"
      ) ~ "Building Expansion",
      str_detect(
        project_name,
        "Demolition"
      ) ~ "Building Demolition",
      str_detect(
        project_name,
        "Relocation"
      ) ~ "Relocation",
      str_detect(
        project_name,
        "Acquisition"
      ) ~ "Building Acquisition",
      str_detect(
        project_name,
        "Park Improvement"
      ) ~ "Park Improvements",
      (cost_center_name == "Recreation and Parks") &
        str_detect(
          project_name,
          "Center"
        ) ~ "Rec Center Improvements",
      str_detect(
        project_name,
        "^ADA|[:space:]ADA"
      ) ~ "ADA Upgrades",
      str_detect(
        project_name,
        "Lighting|Lights"
      ) ~ "Lighting Upgrades",
      str_detect(
        project_name,
        "Electrical|Generator|Electric|Range Conversion|Elec"
      ) ~ "Electrical Upgrades",
      str_detect(
        project_name,
        "Lead Hazard|Lead Prevention"
      ) ~ "Lead Hazard Reduction",
      str_detect(
        project_name,
        "Network Card|Unmanaged Network|Network Management|Network and|Fiber Optic"
      ) ~ "Networking",
      str_detect(
        project_name,
        "Streetscaping|Streetscape"
      ) ~ "Streetscaping Improvements",
      str_detect(
        project_name,
        "Homeowner"
      ) ~ "Homeowner Support/Incentives",
      str_detect(
        project_name,
        "Incentive|Rehab Program|Booster"
      ) ~ "Development Incentives",
      str_detect(
        project_name,
        "Windows"
      ) ~ "Window Upgrades",
      str_detect(
        project_name,
        "Concrete Apron"
      ) ~ "Site Improvements",
      str_detect(
        project_name,
        "Bike|Bicycle"
      ) ~ "Bike Facilities",
      str_detect(
        project_name,
        "Action Center|Library|Station|Envelope|Facility Renovation|Facility Improvements|Facility Building|Masonry|Stairway|Cornice|Security|Garage"
      ) ~ "Other Facility Repairs/Improvements",
      str_detect(
        project_name,
        "Staff Cost|Operating|Operation"
      ) ~ "Operating Support",
      str_detect(
        project_name,
        "Conduit"
      ) ~ "Conduit Construction/Repair"
    ),
    .after = all_of("project_name")
  )
}


#' Format project details report data from Adaptive Planning
#'
format_adaptive_project_data <- function(data,
                                         dictionary,
                                         project_detail_updates,
                                         project_data_corrections,
                                         report_xwalks,
                                         p_hierarchy_xwalks) {
  project_details_source <- data
  adaptive_dictionary <- dictionary
  project_details_filename <- "Capital_Projects_-_Project_Details.xlsx"
  location_updates <- project_detail_updates[["location_updates"]]
  project_name_updates <- project_detail_updates[["project_name_updates"]]
  project_desc_updates <- project_detail_updates[["project_desc_updates"]]

  operating_budget_impact_xwalk <- report_xwalks[["operating_budget_impact_xwalk"]] |>
    mutate(
      impact_on_operating_budget = as.character(impact_on_operating_budget)
    )

  p_hierarchy1_xwalk <- p_hierarchy_xwalks[["p_hierarchy1_xwalk"]]
  p_hierarchy2_xwalk <- p_hierarchy_xwalks[["p_hierarchy2_xwalk"]]

  exclude_projects <- project_data_corrections |>
    filter(
      exclude_flag == "Y"
    ) |>
    select(
      project_code
    )

  # Filter dictionary to project details
  project_details_dictionary <- filter(
    adaptive_dictionary,
    !is.na(adaptive_export_name),
    adaptive_export_filename == project_details_filename
  )

  # Subset columns with numeric values
  project_details_numeric <- filter(
    project_details_dictionary,
    adaptive_import_col_type == "numeric"
  )

  ## ---- format_project_details
  project_details_formatted <- project_details_source |>
    filter(
      # Filter rows appearing after "Total" summary row
      !cumany(.data[["Cost Center Code"]] == "Total"),
      !is.na(.data[["Project Code"]])
    ) |>
    mutate(
      across(
        all_of(project_details_numeric[["adaptive_export_name"]]),
        \(x) {
          if (is.character(x)) {
            readr::parse_number(x)
          } else {
            x
          }
        }
      )
    ) |>
    janitor::clean_names("snake") |>
    # Drop empty columns
    janitor::remove_empty("cols") |>
    select(
      # FIXME: Dropping columns because they are also in CIP requests but I need
      # to double-check that the values are identical in both sources
      !starts_with("fgs_grant_")
    ) |>
    filter(
      !(project_code %in% exclude_projects[["project_code"]])
    ) |>
    # Format adaptive data
    format_prj_data() |>
    # Format workday hierarchy
    format_p_hierarchy_cols(
      p_hierarchy_xwalks = p_hierarchy_xwalks
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
    trim_squish_across() |>
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
      project_type_name = forcats::fct_infreq(
        project_type_name
      ),
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
    # FIXME: This should be handled by the dictionary
    rename(
      operating_impact = operating_budget_impact
    )
}


format_project_locations <- function(data,
                                     location_updates) {
  cli::cli_alert_info(
    "Formatting {.arg location} column"
  )

  data |>
    naniar::replace_with_na(
      list(
        location = c("N/A", "NA", "TBD")
      )
    ) |>
    # FIXME: Location updates should be made in Adaptive - not here
    left_join_coalesce(
      location_updates,
      "location",
      "location_updated",
      by = "project_code"
    ) |>
    mutate(
      location = str_remove(location, "\\(N/A\\)")
    ) |>
    # FIXME: Expand on this approach to separate address from location name
    mutate(
      location = case_match(
        location,
        "Ashburton WFP" ~ "Ashburton Water Filtration Plant",
        "Montebello WFP" ~ "Montebello Water Filtration Plant",
        "3901 Hillen Road" ~ "Montebello Water Filtration Plant",
        .default = location
      ),
      location_asset_id = case_match(
        location,
        "Ashburton Water Filtration Plant" ~ "S07900",
        "Montebello Water Filtration Plant" ~ "S07999",
        .default = location_asset_id
      ),
      location_asset_id = case_when(
        # FIXME: Check these
        is.na(location_asset_id) & location == "6100 Quarantine Road" ~ "B06031",
        is.na(location_asset_id) & location == "111 Kane Street" ~ "B01050",
        is.na(location_asset_id) & location == "2030 Cromwell Bridge Road" ~ "C06525",
        .default = location_asset_id
      )
    ) |>
    select(!c(bridge_asset_id))
}

#' Format project name
#'
#' Creates new columns named project_name_src, legacy_account_number,
#' cip_number, agency_contract_id, agency_contract_id_list, and fiscal_year
#' (using [format_fiscal_year()])
#'
format_project_name <- function(data) {
  check_names(data, must.include = c("project_name", "project_code"))

  cli::cli_alert_info(
    "Formatting {.arg project_name} column"
  )

  data |>
    mutate(
      project_name_src = project_name,

      ##  ---- Legacy Account Number ----

      project_name = str_remove_trim(project_name, project_code),
      # piece of the 26 digit accounting string to identify a specific project
      legacy_account_number = str_extract_legacy_proj_num(project_name),
      project_name = if_else(
        !is.na(legacy_account_number),
        str_remove_trim(project_name, legacy_account_number),
        str_trim(project_name)
      )
    ) |>
    derive_fiscal_year_col() |>
    mutate(

      ##  ---- Legacy CIP Project Number ----
      # Extract cip_number from project names
      cip_number = str_extract_cip_number(project_name),

      # Strip legacy project number from name and description
      project_name = case_when(
        is.na(cip_number) ~ project_name,
        !is.na(cip_number) ~ str_remove_trim(project_name, cip_number),
        .default = project_name
      ),

      # Strip project code from project names
      project_name = str_remove_trim(project_name, paste0(project_code, "$")),

      ##  ---- Agency Project ID ----

      # Extract agency project ID
      agency_contract_id = str_extract_agency_contract_id(
        project_name
      ),
      agency_contract_id_list = str_extract_agency_contract_id(
        project_name,
        extract_all = TRUE
      ),

      # FIXME: Added the code below to try to strip agency project ID values
      # project_name = str_remove(
      #   project_name,
      #   agency_contract_id
      # ),
      # project_name = str_remove(
      #   project_name,
      #   "\\(\\)"
      # ),
      .before = everything()
    )
}


derive_location_asset_id_col <- function(data) {
  check_names(data, must.include = c("location", "project_name"))

  cli_alert_new_col(
    c("location_asset_id", "contract_num"),
    c("location", "project_name")
  )

  data |>
    mutate(
      # TODO: This only works if the asset ID is enclosed in parentheses. DGS does
      # this but other agencies may not
      location_asset_id = str_extract(
        location,
        "(?<=\\()[:alnum:]+(?=\\))"
      ),
      bridge_asset_id = str_extract_bridge_asset_id(project_name),
      # FIXME: This isn't working as expected
      # project_name = str_remove(
      #   project_name,
      #   bridge_asset_id
      # ),
      location_asset_id = coalesce(
        location_asset_id,
        bridge_asset_id
      ),
      contract_num = str_extract(
        project_name,
        "(Alley Co|Footway Co|Contract) ([:digit:]+|I{1,3})"
      ),
      .before = everything()
    ) |>
    naniar::replace_with_na(
      replace = list(
        location_asset_id = c("TBD", "CHM", "HUBS", "CCG", "BHIP", "Choice")
      )
    )
}

#' Derive a new fiscal year column from the project_name column
#'
derive_fiscal_year_col <- function(data) {
  check_names(data, must.include = "project_name")

  cli_alert_new_col("fiscal_year", "project_name")

  mutate(
    data,
    fiscal_year = if_else(
      str_detect(
        project_name,
        "FY[:digit:]{2,4}|FY [:digit:]{2,4}"
      ),
      str_extract(
        project_name,
        "((?<=FY)[:digit:]{2,4})|((?<=FY )[:digit:]{2,4})"
      ),
      NA_character_
    ),
    fiscal_year = if_else(
      !str_length(fiscal_year) == 4,
      str_c("20", fiscal_year),
      fiscal_year
    )
  )
}

# ----- prep_project_locations

prep_project_locations <- function(project_details_data,
                                   project_asset_xwalk,
                                   asset_parcels) {
  project_asset_locations <- asset_parcels |>
    select(
      location_asset_id = asset_id,
      asset_name,
      asset_address = street_address,
      asset_agency_label = agency_name,
      asset_county = county
    ) |>
    filter(
      # Drop empty geometry
      !sf::st_is_empty(geometry)
    )

  project_details_data |>
    # Filter to projects with parsed or matched location asset ID values
    filter(!is.na(location_asset_id) | project_code %in% project_asset_xwalk$project_code) |>
    # TODO: Should this be select instead of distinct? Shouldn't be any different
    distinct(
      agency_label, project_code, project_name, location_asset_id
    ) |>
    # Combine with the project_asset_xwalk
    # Full join instead of left_join to handle projects with multiple matched assets
    full_join(
      project_asset_xwalk, # |>
      # FIXME: This is only using a subset of the crosswalk
      # filter(project_code_n == 1) |>
      # select(!ends_with("_n")),
      by = join_by(project_code),
      relationship = "many-to-many",
      na_matches = "never"
    ) |>
    # Fill missing location_asset_id values from crosswalk
    mutate(
      location_asset_id = coalesce(
        asset_id_updated,
        location_asset_id
      )
    ) |>
    select(!any_of(c("asset_id_updated", "match"))) |>
    # Join asset asset_agency_label to project locations
    left_join(
      project_asset_locations,
      # FIXME: This is returning a warning due to an unexpected many-to-many
      # relationship
      relationship = "many-to-one",
      by = join_by(location_asset_id),
      na_matches = "never"
    ) |>
    # FIXME: are the next two function calls actually needed?
    # mutate(
    #   location_asset_id = coalesce(
    #     asset_id_updated,
    #     location_asset_id
    #   )
    # ) |>
    # select(!c(asset_id_updated, update_asset_match)) |>
    # Blank asset_agency_label if it is the same as agency_label
    mutate(
      asset_agency_label = if_else(
        asset_agency_label == agency_label,
        NA_character_,
        asset_agency_label
      )
    ) |>
    relocate(
      agency_label,
      asset_agency_label,
      project_code,
      project_name,
      .before = everything()
    )
}

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


## ---- join_project_locations ----
join_project_locations <- function(project_details_data,
                                   project_asset_xwalk,
                                   asset_parcels) {
  project_locations <- prep_project_locations(
    project_details_data = project_details_data,
    project_asset_xwalk = project_asset_xwalk,
    asset_parcels = asset_parcels
  )

  project_locations_nested <- nest_project_locations(project_locations)

  project_details_data |>
    # Join formatted project details to nested list column with locations
    left_join(
      project_locations_nested,
      by = join_by(project_code),
      na_matches = "never"
    ) |>
    select(!location_asset_id) |>
    # Join formatted project details to first location asset ID (to support legacy code)
    left_join(
      project_locations |>
        select(
          project_code,
          asset_name,
          asset_address,
          location_asset_id,
          asset_agency_label
        ),
      # TODO: Document that the location_asset_id for that column is just the
      # first and the nested location data should be used for maps
      multiple = "first",
      by = join_by(project_code),
      na_matches = "never"
    ) |>
    mutate(
      # FIXME: This should be documented
      location = if_else(
        is.na(location) & !is.na(location_asset_id),
        asset_name,
        location
      )
    )
}

## ---- join_agency_reference ----
join_agency_reference <- function(data, agency_reference) {
  data |>
    left_join(
      # FIXME: This data could be kept with agency overviews instead and only
      # joined if needed
      agency_reference |>
        select(
          !overview
        ),
      relationship = "many-to-one",
      by = join_by(agency_label),
      na_matches = "never"
    )
}
