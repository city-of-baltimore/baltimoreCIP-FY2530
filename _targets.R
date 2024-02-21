# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(here)
library(readr)
library(openxlsx2)

# Set target options
tar_option_set(
  packages = c(
    "arcgislayers",
    "dplyr",
    "gt",
    "here",
    "janitor",
    "naniar",
    "readr",
    "rlang",
    "sf",
    "stringr"
  ), # packages that your targets need to run
  format = "qs"
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(
  clustermq.scheduler = "multicore",
  readr.show_col_types = FALSE
)

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
future::plan(future.callr::callr)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

tar_plan(
  # Load Baltimore bridge inventory
  baltimore_bridges = load_baltimore_bridge_inventory(),

  # Load Impact Investment Areas
  impact_investment_areas = load_dhcd_iia(),

  # Read Adaptive Planning report data dictionary
  tar_file_read(
    adaptive_dictionary,
    "_targets/user/data/reference/adaptive_dictionary.csv",
    read_csv(file = !!.x)
  ),

  # Read revenue category name crosswalk
  tar_file_read(
    revenue_category_name_xwalk,
    "_targets/user/data/reference/revenue_category_name_xwalk.csv",
    read_csv(file = !!.x)
  ),

  # Read Capital Projects - Six-Year CIP (Requests) - Report
  tar_file_read(
    cip_requests_src,
    "_targets/user/data/Adaptive-Planning/Capital_Projects_-_Six-Year_CIP.xlsx",
    read_xlsx(file = !!.x)
  ),

  # Read Capital Projects - Six-Year CIP (Recommendations) - Report
  tar_file_read(
    cip_recommendations_src,
    "_targets/user/data/Adaptive-Planning/Capital_Projects_-_Six-Year_CIP_Recommendations.xlsx",
    read_xlsx(file = !!.x)
  ),

  # Read Capital Projects - Project Details - Report
  tar_file_read(
    cip_project_details_src,
    "_targets/user/data/Adaptive-Planning/Capital_Projects_-_Project_Details.xlsx",
    read_xlsx(file = !!.x)
  ),

#   cip_recommendations_formatted = format_cip_report(
#     cip_requests_src,
#     adaptive_dictionary = adaptive_dictionary,
#     revenue_category_name_xwalk = revenue_category_name_xwalk
#   ),

  tar_quarto(
    qmd,
    path = getwd()
  )
)
