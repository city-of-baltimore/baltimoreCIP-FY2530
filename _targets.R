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
  # clustermq.scheduler = "multicore",
  readr.show_col_types = FALSE,
  gargle_oauth_email = "eli.pousson@baltimorecity.gov",
  fy_cols = paste0("fy", 2025:2030),
  tbl_fy_cols = c(paste0("fy_", c(2025:2030)), "fy_total"),
  tbl_fy_labels = c(paste0("FY", c(25:30)), "Total ($K)")
)

# future::plan(future.callr::callr)

suppressMessages(
  tar_source()
)

tar_plan(
  # Read Adaptive Planning report data dictionary
  adaptive_dictionary = read_adaptive_dictionary(),
  asset_list_dictionary = read_asset_list_dictionary(),
  report_xwalks = read_sheets_subset(
    url = "https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing",
    pattern = "_xwalk$"
  ),

  # Read agency overviews
  agency_overviews = read_agency_overviews(),

  # Load agency reference data
  agency_reference = load_agency_reference(agency_overviews),

  # Load asset agency crosswalk
  asset_agency_xwalk = load_asset_agency_xwalk(),

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

  # List Capital Projects - Six-Year CIP files
  tar_files(
    cip_program_files,
    path_user_data(
      "Adaptive-Planning",
      c(
        "Capital_Projects_-_Six-Year_CIP_Requests.xlsx",
        "Capital_Projects_-_Six-Year_CIP_PC-Recommendations.xlsx",
        "Capital_Projects_-_Six-Year_CIP_BOE-Recommendations.xlsx",
        "Capital_Projects_-_Six-Year_CIP_BOF-Recommendations.xlsx"
      )
    )
  ),

  # Read Capital Projects - Six-Year CIP
  tar_target(
    cip_program_data,
    purrr::map(
      purrr::set_names(
        cip_program_files,
        c("Requests", "PC", "BOE", "BOF")
      ),
      \(x) {
        x |>
          readxl::read_xlsx() |>
          format_adaptive_cip_data(
            dictionary = adaptive_dictionary,
            revenue_category_name_xwalk = report_xwalks[["revenue_category_name_xwalk"]]
          )
      }
    )
  ),
  cip_requests = cip_program_data[["Requests"]],
  cip_pc_recommendations = cip_program_data[["PC"]],
  cip_bof_recommendations = cip_program_data[["BOF"]],
  cip_boe_recommendations = cip_program_data[["BOE"]],

  # Compare CIP request data and recommendation data
  cip_pc_comparison_summary = summarise_cip_comparison(
    reference_data = cip_requests,
    comparison_data = cip_pc_recommendations,
    nm = c("Request", "Recommendation"),
    names_to = "report",
    fy_cols = getOption("fy_cols")
  ),
  cip_bof_comparison_summary = summarise_cip_comparison(
    reference_data = cip_pc_recommendations,
    comparison_data = cip_bof_recommendations,
    nm = c("PC", "BOF"),
    names_to = "report",
    fy_cols = getOption("fy_cols")
  ),

  # # Read source asset list attributes
  # tar_file_read(
  #   asset_list_src,
  #   command = path_user_data("DGS", "AssetList-MCC-DGS-2022work.xlsx"),
  #   read = readxl::read_xlsx(path = !!.x, sheet = "MCC-ASSETS")
  # ),
  #
  # # Read source asset list attributes
  # tar_file_read(
  #   asset_list_attributes_src,
  #   command = path_user_data("DGS", "AssetList-MCC-DGS-2022work.xlsx"),
  #   read = readxl::read_xlsx(path = !!.x, sheet = "OtherDATA-FMS")
  # ),
  #
  # # Format asset list
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
  #
  # Read Capital Projects - Project Details - Report
  tar_file_read(
    cip_project_details_src,
    command = path_user_data("Adaptive-Planning", "Capital_Projects_-_Project_Details.xlsx"),
    read = readxl::read_xlsx(path = !!.x)
  ),
  tar_quarto(
    baltimoreCIP_qto,
    path = here::here(),
    quiet = FALSE,
    error = "continue"
  ),

  # # Format project data
  cip_project_details = format_adaptive_project_data(
    data = cip_project_details_src,
    project_detail_updates = project_detail_updates,
    project_data_corrections = project_data_corrections,
    dictionary = adaptive_dictionary,
    report_xwalks = report_xwalks,
    p_hierarchy_xwalks = p_hierarchy_xwalks,
    agency_reference = agency_reference
  ),
  cip_additional_locations = path_user_data("FY2025-CIP-Additions.geojson") |>
    sf::read_sf() |>
    sf::st_transform(crs = 3857),
  #
  # Format project location data
  # cip_locations = load_cip_project_locations(
  #   project_data = cip_project_details,
  #   asset_data = asset_list,
  #   project_asset_xwalk = report_xwalks[["project_asset_xwalk"]],
  #   agency_reference = agency_reference,
  #   additional_data = dplyr::bind_rows(
  #     dot_location,
  #     dplyr::filter(
  #       cip_additional_locations,
  #       !(project_code %in% dot_location$project_code)
  #     )
  #   )
  # ),
  #
  # # cip_location_plots = purrr::map(
  # #   set_names(
  # #     cip_locations[["location_data"]],
  # #     cip_locations[["project_code"]]
  # #   ),
  # #   \(data) {
  # #     data <- sf::st_as_sf(data)
  # #
  # #     if (all(sf::st_is_empty(data))) {
  # #       return(NULL)
  # #     }
  # #
  # #     plot_location_data(data)
  # #   }
  # # ),
  #
  # cip_projects = join_project_locations(
  #   project_data = cip_project_details,
  #   location_data = cip_locations
  # ),
  #
  # # Combine into a single report data frame
  # cip_report_data = load_cip_report_data(
  #   project_data = cip_projects,
  #   request_data = cip_requests,
  #   pc_recommendation_data = cip_pc_recommendations,
  #   bof_recommendation_data = cip_bof_recommendations,
  #   dictionary = adaptive_dictionary
  # ),
  # summary_tables = gt_group_summary_tables(
  #   report_data = cip_report_data,
  #   cip_verb = "recommended",
  #   cip_data_col = "recommendation_data"
  # ),
  # kbl_cip_comparison_list = kbl_comparison_list(
  #   comparison_summary = cip_pc_comparison_summary,
  #   project_details = cip_project_details
  # ),
  # kbl_appendix_report_identifier = kbl_project_identifier(
  #   cip_report_data
  # ),
  #
  # # Render Quarto project
  # # tar_quarto(
  # #   report_qmd,
  # #   path = here::here("report", "report_cip_recommendations.qmd"),
  # #   quiet = FALSE,
  # #   error = "continue",
  # #   extra_files = c(
  # #     # Monitor includes and child documents
  # #     fs::dir_ls(
  # #       path = "report",
  # #       recurse = TRUE,
  # #       type = "file",
  # #       glob = "*.qmd"
  # #     ),
  # #     # Monitor table and plot scripts
  # #     fs::dir_ls(
  # #       path = "R",
  # #       type = "file",
  # #       pattern = "^gt_|^kbl_|^plot_"
  # #     )
  # #   )
  # # ),
  #
  #
  # csa_mhhi = load_baltimore_csa_mhhi(),
  # csa_paa = load_baltimore_csa_paa()
  #
  #
  # # tar_target(
  # #   report_qmd_cover,
  # #   combine_pdf_cover(
  # #    input = here::here("_output/FY2530_CIP-Recommendation-Report_v5.pdf"),
  # #    cover = here::here("files/FY2530_CIP-BOE-Recommendation-Report_cover.pdf"),
  # #    output = "_output/FY2530_CIP-Recommendation-Report_with-cover.pdf"
  # #   )
  # # )
)
