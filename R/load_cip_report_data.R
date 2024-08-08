#' Join recommendation data as a nested data frame column to a combined data frame with project details and requests
#'
join_recommendation_data <- function(
    cip_data,
    recommendation_data,
    dictionary,
    .key = "recommendation_data") {
  recommendation_data <- recommendation_data |>
    rename_with_dictionary(
      dictionary = dictionary
    ) |>
    nest_by(project_code, .key = .key, .keep = TRUE)

  cip_data |>
    left_join(
      recommendation_data,
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
        # FIXME: Reserve status is only included temporarily due to miscoding of
        # 2 projects
        "Reserve"
      ),
      # FIXME: Double-check that has_requests and has_recommendations are both
      # still good criteria
      (!is.na(project_desc) & !is.na(priority_level)) | has_requests | has_recommendations
    )
}


#' Combine CIP Project data with requests and recommendations
#'
#' Also rename columns based on the dictionary
#'
combine_cip_project_data <- function(
    project_data,
    request_data,
    pc_recommendation_data = NULL,
    report_stage_data = NULL,
    location_data = NULL,
    current_fy_col = "fy2025",
    summary_col = "total_amt",
    fy_total_col = "fy_total",
    cip_type = "recommendation",
    request_data_col = "request_data",
    pc_recommendation_data_col = "pc_recommendation_data",
    report_stage_data_col = "recommendation_data",
    dictionary) {
  # Rename columns
  project_details <- project_data

  project_data_join_cols <- project_data |>
    select(project_code, agency_label, agency_short_name)

  cip_type <- arg_match(cip_type, c("recommendation", "request"))

  summary_data <- request_data
  if (cip_type == "recommendation") {
    # FIXME: Document this pattern
    summary_data <- report_stage_data %||% pc_recommendation_data
  }

  # Summarize total amount by project code
  summary_data_join <- summary_data |>
    filter(!is.na(.data[[fy_total_col]])) |>
    summarise(
      "{summary_col}" := sum(.data[[fy_total_col]], na.rm = TRUE),
      .by = project_code
    )

  # Pull project codes for projects with a non-zero total
  is_recommended_project <- summary_data |>
    pull_distinct_project_code(
      !is.na(.data[[fy_total_col]]),
      .data[[fy_total_col]] > 0 | .data[[fy_total_col]] < 0
    )

  # Pull project codes for projects with a non-zero total for request data
  is_requested_project <- request_data |>
    pull_distinct_project_code(
      !is.na(.data[[fy_total_col]]),
      .data[[fy_total_col]] > 0 | .data[[fy_total_col]] < 0
    )

  # Subset data to create the has_grant_source column
  is_grant_source_request <- request_data |>
    pull_distinct_project_code(
      revenue_category_name_short %in% c("State Grant", "Fed. Grant")
    )

  # Subset data to create has_requests_2025 column
  is_current_fy_request <- request_data |>
    pull_distinct_project_code(
      !is.na(.data[[current_fy_col]]),
      .data[[current_fy_col]] > 0
    )

  request_data_join <- request_data |>
    prep_to_combine_projects(
      y = project_data_join_cols,
      dictionary = dictionary,
      .key = request_data_col
    )

  # Nest recommendation data
  pc_recommendation_data_join <- pc_recommendation_data |>
    prep_to_combine_projects(
      y = project_data_join_cols,
      dictionary = dictionary,
      .key = pc_recommendation_data_col
    )

  # Nest recommendation data
  report_stage_data_join <- report_stage_data |>
    prep_to_combine_projects(
      y = project_data_join_cols,
      dictionary = dictionary,
      .key = report_stage_data_col
    )

  project_data_join <- project_data |>
    # Rename columns based on dictionary
    rename_with_dictionary(
      dictionary = dictionary
    ) |>
    mutate(
      # Add prj reference column
      prj = glue::glue("{project_code}: {project_name}"),
      # Add flags for ease of reporting on nested data
      has_requests = project_code %in% is_requested_project,
      has_requests_curr_fy = project_code %in% is_current_fy_request,
      has_recommendations = project_code %in% is_recommended_project,
      has_grant_source = project_code %in% is_grant_source_request,
      has_est_cost_data = !(is.na(est_cost_design) & is.na(est_cost_construction) &
        is.na(est_cost_other) & is.na(est_cost_total))
    )

  # Combine project details and CIP requests
  project_data_combined <- project_data_join |>
    left_join(
      # Summarize CIP requests by project number
      summary_data_join,
      by = join_by(project_code),
      relationship = "one-to-one",
      na_matches = "never"
    ) |>
    left_join(
      request_data_join,
      by = join_by(project_code),
      relationship = "one-to-one",
      na_matches = "never"
    ) |>
    left_join(
      pc_recommendation_data_join,
      by = join_by(project_code),
      relationship = "one-to-one",
      na_matches = "never"
    ) |>
    left_join(
      report_stage_data_join,
      by = join_by(project_code),
      relationship = "one-to-one",
      na_matches = "never"
    )

  # Save project_requests
  project_data_combined
}

#' Rename an input data frame based on a dictionary and join to a second data
#' frame then nest by project code
prep_to_combine_projects <- function(
    x,
    y,
    dictionary,
    .key,
    na_matches = "never",
    .keep = TRUE) {
  x |>
    janitor::clean_names() |>
    rename_with_dictionary(
      dictionary = dictionary
    ) |>
    dplyr::left_join(
      y,
      by = join_by(project_code),
      na_matches = na_matches
    ) |>
    dplyr::nest_by(project_code, .key = .key, .keep = .keep)
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
    pc_recommendation_data = NULL,
    report_stage_data = NULL,
    dictionary) {
  cip_data <- combine_cip_project_data(
    project_data = project_data,
    request_data = request_data,
    pc_recommendation_data = pc_recommendation_data,
    report_stage_data = report_stage_data,
    dictionary = dictionary
  )

  cip_data |>
    filter_cip_data() |>
    # format_level_cols() |>
    # left_join_asset_data(
    #   asset_xwalk = asset_xwalk,
    #   asset_parcels = asset_parcels
    # ) |>
    mutate(
      project_desc = str_replace_na(project_desc, "")
    ) |>
    relocate(
      agency_label,
      project_code,
      project_name,
      project_desc,
      total_amt,
      location,
      request_data,
      location_data,
      recommendation_data,
      .before = everything()
    )
}
