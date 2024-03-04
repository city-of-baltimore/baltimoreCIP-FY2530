#
# fy2025_project_locations <- project_requests |>
#   filter(has_requests_2025 | project_code %in% project_recommendations_update_export$project_code) |>
#   pull(location_data, name = "project_code") |>
#   list_rbind(names_to = "project_code") |>
#   sf::st_as_sf() |>
#   left_join(
#     cip_prj_xwalk_update |>
#       select(
#         project_code,
#         cip_number,
#         cip_project_title = project_title,
#         cip_number_uncertain = uncertain_match,
#         first_fiscal_year
#       ),
#     relationship = "many-to-many"
#   ) |>
#   filter(!sf::st_is_empty(geometry))
#
# FY25_dot_locations <- readr::read_rds(here::here("data/FY25_dot_locations.rds")) |>
#   mutate(
#     agency_label = "Department of Transportation",
#     asset_agency_label = "Department of Transportation",
#     asset_name = case_match(
#       project_name,
#       "FY25 - Alley Reconstruction Program" ~ paste0(street_name, " Alley"),
#       "FY25 - Sidewalk Reconstruction Program" ~ paste0(street_name, " Sidewalk"),
#       .default = street_name),
#     asset_address = coalesce(street_address, street_name),
#     asset_county = "Baltimore city"
#   ) |>
#   select(
#     any_of(names(fy2025_project_locations))
#   )
#
# Locations_FY2025 <- fy2025_project_locations |>
#   filter(!project_code %in% FY25_dot_locations$project_code) |>
#   bind_rows(FY25_dot_locations)
#
# Locations_FY2025 |>
#   sfext::write_sf_ext(
#     "CIP-Locations_FY2025.gpkg",
#     overwrite = TRUE
#   )


#' Join recommendation data as a nested data frame column to a combined data frame with project details and requests
#'
join_recommendation_data <- function(
    cip_data,
    recommendation_data,
    dictionary) {
  recommendation_data <- recommendation_data |>
    rename_with_dictionary(
      dictionary = dictionary
    )

  cip_data |>
    left_join(
      recommendation_data |>
        nest_by(project_code, .key = "recommendation_data", .keep = TRUE),
      by = join_by(project_code)
    )
}

#' Filter CIP data by status and other attributes
#'
filter_cip_data <- function(cip_data) {
  cip_data |>
    filter(
      p_status_name %in% c(
        "Approved",
        "Pending Approval",
        # FIXME: Reserve status is only included temporarily due to miscoding of 2 projects
        "Reserve"
      ),
      (!is.na(project_desc) & !is.na(priority_level)) | has_requests
    )
}


#' Combine CIP Project data with requests and recommendations
#'
#' Also rename columns based on the dictionary
#'
combine_cip_project_data <- function(
    project_data,
    request_data,
    recommendation_data,
    dictionary) {
  adaptive_dictionary <- dictionary
  project_details <- project_data

  # Used to create the has_grant_source column
  cip_grant_requests <- request_data |>
    filter(
      revenue_category_name_short %in% c("State Grant", "Fed. Grant")
    )

  # Used to create has_requests_2025 column
  cip_fy2025_requests <- request_data |>
    filter(
      !is.na(fy2025),
      fy2025 > 0
    )

  project_details_join <- project_details |>
    # Rename columns based on dictionary
    rename_with_dictionary(
      dictionary = dictionary
    ) |>
    mutate(
      # Add prj reference column
      prj = glue::glue("{project_code}: {project_name}"),
      # Add flags for ease of reporting on nested data
      has_requests = project_code %in% request_data[["project_code"]],
      # has_requests = has_requests & !(
      #   project_code %in% cip_requests_no_amt[["project_code"]]
      # ),
      has_requests_2025 = project_code %in% cip_fy2025_requests[["project_code"]],
      has_grant_source = project_code %in% cip_grant_requests$project_code,
      has_est_cost_data = !(is.na(est_cost_design) & is.na(est_cost_construction) &
        is.na(est_cost_other) & is.na(est_cost_total))
    )

  cip_requests_join <- request_data |>
    rename_with_dictionary(dictionary = dictionary)

  cip_requests_join_details <- cip_requests_join |>
    full_join(
      project_details_join |>
        select(project_code) |>
        distinct(),
      by = join_by(project_code),
      relationship = "many-to-one",
      na_matches = "never"
    )

  cip_requests_join_total <- cip_requests_join_details |>
    nest_by(project_code, .key = "request_data", .keep = TRUE) |>
    left_join(
      # Summarize CIP requests by project number
      cip_requests_join |>
        filter(!is.na(fy_total)) |>
        summarise(
          total_request_amt = sum(fy_total, na.rm = TRUE),
          .by = project_code
        ),
      by = join_by(project_code),
      relationship = "one-to-one",
      na_matches = "never"
    )

  # Combine project details and CIP requests
  project_requests <- left_join(
    # FIXME: If cip_requests appears first, project_requests excludes requests with
    # no associated financial request. If project_details appears first, any
    # scripts using this data must expect missing data in financial requests
    project_details_join,
    cip_requests_join_total,
    by = join_by(project_code),
    relationship = "one-to-one",
    na_matches = "never"
  )

  # Save project_requests
  project_requests
}


format_level_cols <- function(data) {
  data |>
    mutate(
      across(
        ends_with("_level"),
        \(x) {
          case_match(
            x,
            "Low" ~ "temperature-empty",
            "Medium" ~ "temperature-half",
            "High" ~ "temperature-full",
            .default = NA_character_
          )
        },
        .names = "{.col}_icon"
      ),
      across(
        ends_with("_level"),
        \(x) {
          case_match(
            x,
            "Low" ~ "#ffae42",
            "Medium" ~ "#C16512",
            "High" ~ "#dd4f00",
            .default = NA_character_
          )
        },
        .names = "{.col}_icon_fill"
      )
    )
}

left_join_asset_data <- function(data,
                                 asset_xwalk,
                                 asset_parcels = NULL) {
  data <- data |>
    left_join(
      asset_xwalk |>
        select(
          project_code,
          xwalk_asset_id = asset_id,
          match
        ) |>
        distinct(project_code, .keep_all = TRUE),
      by = join_by(project_code),
      relationship = "one-to-one",
      na_matches = "never"
    ) |>
    mutate(
      # FIXME: Did I finish this?
      derived_asset_id = is.na(location_asset_id) & !is.na(xwalk_asset_id),
      location_asset_id = coalesce(
        location_asset_id,
        xwalk_asset_id
      )
    ) |>
    rename(
      location_asset_name = asset_name
    )

  if (is.null(asset_parcels)) {
    return(data)
  }

  data |>
    left_join(
      asset_parcels |>
        select(
          location_asset_id = asset_id,
          asset_type_label,
          asset_name,
          asset_name_short,
          asset_county = county
        ) |>
        sf::st_drop_geometry(),
      by = join_by(location_asset_id),
      relationship = "many-to-one",
      na_matches = "never"
    )
}

#' Load CIP Report data by combining requests and recommendations
#'
load_cip_report_data <- function(
    project_data,
    request_data,
# FIXME: This has not yet been updated to combine recommendation data
    recommendation_data,
    dictionary,
    asset_xwalk = NULL, # report_xwalks[["project_asset_xwalk"]],
    asset_parcels = NULL) {
  cip_data <- combine_cip_project_data(
    project_data = project_data,
    request_data = request_data,
    recommendation_data = recommendation_data,
    dictionary = dictionary
  )

  cip_data |>
    filter_cip_data() #|>
  # format_level_cols() |>
  # left_join_asset_data(
  #   asset_xwalk = asset_xwalk,
  #   asset_parcels = asset_parcels
  # ) |>
  # relocate(
  #   agency_label,
  #   project_code,
  #   project_name,
  #   project_desc,
  #   location,
  #   location_data,
  #   total_request_amt,
  #   request_data,
  #   .before = everything()
  # )
}
