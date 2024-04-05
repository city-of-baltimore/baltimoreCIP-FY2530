#' Format 6-Year CIP Requests data for FY 2025 from Adaptive Planning export file
library(tidyverse)
library(here)
source(here("data-raw", "functions-adaptive.R"))

# Read Adaptive Workday dictionary
adaptive_dictionary <- read_rds_data("adaptive_dictionary")

# Set input file names
# date <- "2023-11-07"
# filename <- "Capital_-_Six-Year_CIP_FY2025_CAP_Requests.xlsx"
# filename <- "Capital_-_Six-Year_CIP.xlsx"
filename <- "Capital_Projects_-_Six-Year_CIP.xlsx"

# Read data from file
# file <- paste0(date, "_", filename)
file <- filename

# Read revenue_category_name_short reference data
revenue_category_name_xwalk <- readr::read_csv(
  file = here(
    "data-raw", "files", "revenue_category_name_xwalk.csv"
  ),
  show_col_types = FALSE
)

cip_requests_formatted <- read_adaptive_cip_requests(
  file = file,
  adaptive_dictionary = adaptive_dictionary,
  revenue_category_name_xwalk = revenue_category_name_xwalk
)

cip_requests_formatted |>
  readr::write_rds(here("data", "cip_requests.rds"))

recommendations_file <- "Capital_Projects_-_Six-Year_CIP_Recommendations.xlsx"

cip_recommendations_formatted <- read_adaptive_cip_requests(
  file = recommendations_file,
  adaptive_dictionary = adaptive_dictionary,
  revenue_category_name_xwalk = revenue_category_name_xwalk
)

cip_recommendations_formatted |>
  readr::write_rds(here("data", "cip_recommendations.rds"))


#
#
#
# bind_grant_id <- function(data) {
#   data |>
#     group_by(
#       project_code,
#       revenue_category_name,
#       grant_detail_name
#     ) |>
#     mutate(
#       request_id_num = row_number(),
#       # .by = c(),
#       .after = grant_detail_name
#     )
# }
#
# cip_recommendations_formatted |>
#   bind_grant_id() |>
#   View()


#
#
# cip_review |>
#   filter(is.na(grant_detail_code), revenue_category_name %in% c("State Grants", "Federal Grants")) |>
#   select(report, project_code, project_name, revenue_category_name, grant_detail_code, fy_total) |>
#   View()
#   mutate(
#     .by = c(project_code, reve)
#   )
#
# |>
#   select(report, revenue_category_name_short, project_code, fy_total) |>
#   pivot_wider(
#     names_from = report,
#     values_from = starts_with("fy")
#   ) |>
#   View()
#   summarise(
#     new_revenue_sources =
#     .by = c(revenue_category_name_short, project_code)
#   )
#   View()
#   summarise(
#     recommendation_amt_desc = case_when(
#       fy_total
#     ),
#     .by = c(project_code)
#   )
