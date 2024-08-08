# Load packages ----
library(targets)
library(tarchetypes)

# Options ----

## Set targets options ----
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
    "openxlsx2",
    "readr",
    "rlang",
    "sf",
    "stringr",
    "tidyr",
    "tibble"
  ),
  format = "qs"
)

## Set additional options ----
options(
  readr.show_col_types = FALSE,
  gargle_oauth_email = "eli.pousson@baltimorecity.gov",
  current_report_stage = "Ordinance",
  current_report_filename = "FY25-30_CIP-Ordinance-Report.pdf",
  current_report_title_short = "FY25-30 CIP Report",
  fy_cols = paste0("fy", 2025:2030),
  tbl_fy_cols = c(paste0("fy_", c(2025:2030)), "fy_total"),
  tbl_fy_labels = c(paste0("FY", c(25:30)), "Total ($K)")
)

tar_source()

if (FALSE) {
  download_fy2530_report_references()
}

report_reference_files <- rlang::set_names(
  fs::dir_ls(path_tar_user(), glob = "*.csv"),
  fs::path_ext_remove(
    fs::path_file(
      fs::dir_ls(path_tar_user(), glob = "*.csv")
    )
  )
)

report_stages <- read_curr_fy_report_stages(
  curr_stage = getOption("current_report_stage")
)

# targets pipeline ----

## Load setup data ----
setup_cip_report <- tar_plan(

  # Read reference CSV files downloaded w/ download_fy2530_report_references
  report_references = purrr::map(
    report_reference_files,
    readr::read_csv
  ),
  report_stage_reference = report_references[["fy2530_report_stages"]],
  curr_report_stage_reference = report_stage_reference |>
    dplyr::filter(
      stage == getOption("current_report_stage")
    )
)

## Load CIP Program data ----
load_cip_program <- tar_plan(

  # List Capital Projects - Six-Year CIP files
  tar_files(
    cip_program_files,
    path_tar_user(
      "Adaptive-Planning",
      report_stages$filename
    )
  ),

  # Read Capital Projects - Six-Year CIP reports
  tar_target(
    cip_program_data,
    purrr::map(
      purrr::set_names(
        cip_program_files,
        c("Requests", "PC", "BOE", "BOF", "Ordinance")
      ),
      \(x) {
        if (!fs::file_exists(x)) {
          return(NULL)
        }

        x |>
          readxl::read_xlsx() |>
          format_adaptive_cip_data(
            dictionary = report_references[["adaptive_dictionary"]],
            revenue_category_name_xwalk = report_references[["revenue_category_name_xwalk"]]
          )
      }
    )
  ),
  cip_program_report_stage = cip_program_data[[getOption("current_report_stage")]]
)


# Compare CIP budget at different stages ----
# TODO: These comparisons are not currently executed but should be made
# available in a separate supplement
compare_cip_stages <- tar_plan(
  # Compare CIP request data and recommendation data
  cip_pc_comparison_summary = summarise_cip_comparison(
    reference_data = cip_program_data[["Requests"]],
    comparison_data = cip_program_data[["PC"]],
    nm = c("Request", "PC"),
    names_to = "report",
    fy_cols = getOption("fy_cols")
  ),
  cip_bof_comparison_summary = summarise_cip_comparison(
    reference_data = cip_program_data[["PC"]],
    comparison_data = cip_program_data[["BOF"]],
    nm = c("PC", "BOF"),
    names_to = "report",
    fy_cols = getOption("fy_cols")
  ),
  cip_ordinance_comparison_summary = summarise_cip_comparison(
    reference_data = cip_program_data[["PC"]],
    comparison_data = cip_program_data[["Ordinance"]],
    nm = c("PC", "Ordinance"),
    names_to = "report",
    fy_cols = getOption("fy_cols")
  )
)

## Load CIP Project location data ----
load_cip_locations <- tar_plan(
  # See <https://github.com/city-of-baltimore/baltimore-assets>
  tar_file_read(
    baltimore_asset_list,
    command = path_tar_user("spatial", "Baltimore-City_Asset-List.gpkg"),
    read = read_sf_data(!!.x)
  ),
  # See <https://github.com/city-of-baltimore/baltimore-assets>
  tar_file_read(
    additional_asset_locations,
    command = path_tar_user("spatial", "Baltimore-City_Additional-Locations.gpkg"),
    read = read_sf_data(!!.x)
  ),

  # Load prepared DOT location data (specific to current fiscal year)
  curr_fy_dot_locations = load_dot_locations(
    project_asset_xwalk = report_references[["project_asset_xwalk"]]
  ),

  # Load additional CIP project locations (specific to current fiscal year)
  tar_file_read(
    curr_fy_additional_locations,
    # FIXME: Switch this source to a GeoPackage file for consistency
    command = path_tar_user("spatial", "FY2025-CIP-Additions.geojson"),
    read = read_sf_data(!!.x) |>
      dplyr::filter(
        # FIXME: Remove duplicate locations or combine outside of this pipeline
        !(project_code %in% curr_fy_dot_locations$project_code)
      )
  ),
  asset_list = dplyr::bind_rows(
    baltimore_asset_list,
    additional_asset_locations,
    curr_fy_dot_locations,
    curr_fy_additional_locations
  )
)

load_cip_project_details <- tar_plan(

  # Read Capital Projects - Project Details - Report
  tar_file_read(
    cip_project_details_src,
    command = path_tar_user(
      "Adaptive-Planning",
      "Capital_Projects_-_Project_Details",
      ext = "xlsx"
    ),
    read = readxl::read_xlsx(
      path = !!.x,
    )
  ),

  # Format project details using the dictionary and crosswalk files
  cip_project_details = format_adaptive_project_data(
    data = cip_project_details_src,
    project_detail_updates = format_project_detail_updates(
      report_references[["project_detail_updates"]]
    ),
    project_exclusions = c(
      # FIXME: Only 3 of these excluded projects are flagged as "cancelled" -
      # other projects may need to be updated in Workday
      "PRJ000480", "PRJ003034", "PRJ001889", "PRJ002842", "PRJ002840",
      "PRJ003203", "PRJ003035", "PRJ003241", "PRJ003154", "PRJ003237",
      "PRJ003239"
    ),
    dictionary = report_references[["adaptive_dictionary"]],
    report_xwalks = report_references,
    agency_reference = report_references[["agency_reference"]]
  )

  # TODO: Add agency_contract_xwalk to an appendix
  # agency_contract_xwalk = get_agency_contract_xwalk(cip_project_details),

  # TODO: Restore appendix w/ kbl_cip_comparison_list +
  # kbl_appendix_report_identifier as a separate supplement
  # kbl_cip_comparison_list = kbl_comparison_list(
  #   comparison_summary = cip_pc_comparison_summary,
  #   project_details = cip_project_details
  # ),
  # kbl_appendix_report_identifier = kbl_project_identifier(
  #   cip_report_data
  # ),
)

prep_cip_report_data <- tar_plan(

  # Format project location data
  cip_locations = load_cip_project_locations(
    project_data = cip_project_details,
    asset_data = asset_list,
    project_asset_xwalk = report_references[["project_asset_xwalk"]],
    agency_reference = report_references[["agency_reference"]]
  ),
  cip_projects = join_project_locations(
    project_data = cip_project_details,
    location_data = cip_locations
  ),

  # Combine into a single report data frame
  cip_report_data = load_cip_report_data(
    project_data = cip_projects,
    request_data = cip_program_data[["Requests"]],
    pc_recommendation_data = cip_program_data[["PC"]],
    report_stage_data = cip_program_report_stage,
    dictionary = report_references[["adaptive_dictionary"]]
  ),

  # Prep summary tables
  # TODO: Check if there is any benefit to rendering this in the targets
  # pipeline instead of inline
  summary_tables = gt_group_summary_tables(
    report_data = cip_report_data,
    cip_verb = "recommended",
    cip_data_col = "recommendation_data"
  )
)

# TODO: Refactor to use `tarchetypes::tar_quarto` if possible
# NOTE: Using `quarto::quarto_render` instead of tarchetypes::tar_quarto
# simplifies the process of setting a custom directory and output filename
# Issues w/ this report include:
# - attempting to execute these targets before the dependencies are rendered (run tar_make twice to address this)
# - not detecting changes to the Quarto documents or supporting files (use tar_delete() to address this)
render_cip_report <- tar_plan(
  # Render Quarto project
  # TODO: Add cover PDF as part of the file rendering - it is currently added
  # manually
  tar_target(
    report_qmd,
    quarto::quarto_render(
      input = here::here("report.qmd"),
      output_file = getOption("current_report_filename"),
      quiet = FALSE,
      execute_params = list(
        stage = getOption("current_report_stage"),
        title_short = getOption("current_report_title_short")
      )
    ),
    error = "continue"
  ),
  tar_target(
    report_site_qmd,
    quarto::quarto_render(
      input = here::here("index.qmd"),
      metadata_file = "_site.yml",
      output_file = "index.html",
      execute_params = list(
        stage = getOption("current_report_stage"),
        title_short = getOption("current_report_title_short")
      ),
      quiet = FALSE
    ),
    error = "continue"
  )
)

# Execute the combined target specifications
tar_plan(
  setup_cip_report,
  load_cip_locations,
  load_cip_program,
  load_cip_project_details,
  prep_cip_report_data,
  render_cip_report
)
