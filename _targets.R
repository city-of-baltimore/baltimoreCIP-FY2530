# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(readr)
library(openxlsx2)

suppressPackageStartupMessages(
  library(here)
)

suppressPackageStartupMessages(
  library(dplyr)
)

# Set target options
tar_option_set(
  tidy_eval = TRUE,
  packages = c(
    "arcgislayers",
    "checkmate",
    "dplyr",
    "fs",
    "gt",
    "here",
    "janitor",
    "naniar",
    "purrr",
    "readr",
    "rlang",
    "sf",
    "stringr",
    "tidyr",
    "tibble"
  ),
  format = "qs"
)

options(
  clustermq.scheduler = "multicore",
  readr.show_col_types = FALSE,
  gargle_oauth_email = "eli.pousson@baltimorecity.gov"
)

future::plan(future.callr::callr)

tar_source()

tar_plan(
  # Read Adaptive Planning report data dictionary
  adaptive_dictionary = read_adaptive_dictionary(),
  asset_list_dictionary = read_asset_list_dictionary(),

  # tar_files(basemap_filenames,
  #           fs::dir_ls(path_user_data("basemap"))),
  #
  # basemap_data = read_rds_paths(basemap_filenames),
  report_xwalks = read_sheets_subset(
    url = "https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing",
    pattern = "_xwalk$"
  ),

  # Read agency overviews
  agency_overviews = read_agency_overviews(),

  # Load agency reference data
  agency_reference = load_agency_reference(
    agency_overviews = agency_overviews
  ),

  # baltimore_city = load_baltimore_city(),

  # baltimore_city_basemap = load_baltimore_city_basemap(),

  # Load Baltimore bridge inventory
  baltimore_bridges = load_baltimore_bridge_inventory(),

  # Load Impact Investment Areas
  impact_investment_areas = load_dhcd_iia(),

  # Load supplementary data from OpenStreetMap
  osm_asset_data = load_osm_asset_data(),

  # Load DOT intersections
  dot_intersections = load_dot_intersections(report_xwalks[["project_asset_xwalk"]]),
  dot_location = load_dot_locations(),

  # Read project detail updates information
  project_detail_updates = read_project_detail_updates(),
  project_data_corrections = read_project_data_corrections(),
  p_hierarchy_xwalks = read_p_hierarchy_xwalks(),

  # Read Capital Projects - Six-Year CIP (Requests) - Report
  tar_file_read(
    cip_requests_src,
    path_user_data(
      "Adaptive-Planning",
      "Capital_Projects_-_Six-Year_CIP_Requests.xlsx"
    ),
    read_xlsx(file = !!.x)
  ),

  # Format request data
  cip_requests = format_adaptive_cip_data(
    cip_requests_src,
    dictionary = adaptive_dictionary,
    revenue_category_name_xwalk = report_xwalks[["revenue_category_name_xwalk"]]
  ),

  # Read Capital Projects - Six-Year CIP (Recommendations) - Report
  tar_file_read(
    cip_recommendations_src,
    path_user_data(
      "Adaptive-Planning",
      "Capital_Projects_-_Six-Year_CIP_Recommendations.xlsx"
    ),
    read_xlsx(file = !!.x)
  ),

  # Format recommendation data
  cip_recommendations = format_adaptive_cip_data(
    data = cip_recommendations_src,
    dictionary = adaptive_dictionary,
    revenue_category_name_xwalk = report_xwalks[["revenue_category_name_xwalk"]]
  ),

  # Compare CIP request data and recommendation data
  cip_comparison_summary = summarise_cip_comparison(
    request_data = cip_requests,
    recommendation_data = cip_recommendations
  ),

  # Read source asset list attributes
  tar_file_read(
    asset_list_src,
    path_user_data("DGS", "AssetList-MCC-DGS-2022work.xlsx"),
    read_xlsx(file = !!.x, sheet = "MCC-ASSETS")
  ),

  # Read source asset list attributes
  tar_file_read(
    asset_list_attributes_src,
    path_user_data("DGS", "AssetList-MCC-DGS-2022work.xlsx"),
    read_xlsx(file = !!.x, sheet = "OtherDATA-FMS")
  ),

  # Load asset agency crosswalk
  asset_agency_xwalk = load_asset_agency_xwalk(),

  # Format asset list
  asset_list = format_dgs_asset_list(
    asset_data = asset_list_src,
    attributes_data = asset_list_attributes_src,
    dictionary = asset_list_dictionary |>
      filter(!is.na(clean_name)),
    data_sheet = "MCC-ASSETS",
    attributes_sheet = "OtherDATA-FMS",
    agency_reference = agency_reference,
    additional_data = list(
      impact_investment_areas,
      baltimore_bridges,
      osm_asset_data,
      dot_intersections
    ),
    asset_agency_xwalk = asset_agency_xwalk
  ),

  # Read Capital Projects - Project Details - Report
  tar_file_read(
    cip_project_details_src,
    path_user_data("Adaptive-Planning", "Capital_Projects_-_Project_Details.xlsx"),
    read_xlsx(file = !!.x)
  ),

  # Format project data
  cip_project_details = format_adaptive_project_data(
    data = cip_project_details_src,
    project_detail_updates = project_detail_updates,
    project_data_corrections = project_data_corrections,
    dictionary = adaptive_dictionary,
    report_xwalks = report_xwalks,
    p_hierarchy_xwalks = p_hierarchy_xwalks,
    agency_reference = agency_reference
  ),

  # Format project location data
  cip_locations = load_cip_project_locations(
    project_data = cip_project_details,
    asset_data = asset_list,
    project_asset_xwalk = report_xwalks[["project_asset_xwalk"]],
    agency_reference = agency_reference
  ),
  cip_projects = join_project_locations(
    project_data = cip_project_details,
    location_data = cip_locations
  ),

  # Combine into a single report data frame
  cip_report_data = load_cip_report_data(
    project_data = cip_projects,
    request_data = cip_requests,
    recommendation_data = cip_recommendations,
    dictionary = adaptive_dictionary
  ),

  # Render Quarto project
  tar_quarto(
    report_qmd,
    path = getwd(),
    extra_files = c(
      # Monitor includes and child documents
      fs::dir_ls(
        path = "report",
        recurse = TRUE,
        type = "file",
        glob = "*.qmd"
      ),
      # Monitor table and plot scripts
      fs::dir_ls(
        path = "R",
        type = "file",
        pattern = "^gt_|^kbl_|^plot_"
      )
    )
  )
)
