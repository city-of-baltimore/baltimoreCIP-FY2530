# identity tool splits up the buffer level by CSA creates thousands of records
# 1 local 0.25 mi (half in local for regional/citywide, half for regional/citywide)
# 2 regional 1 mi (distributed based on overlapping area)
# 3 citywide 5 mi

load_fy25_boe_data <- function() {
  fy25_boe_path <- path_user_data("fy25_boe.gdb.zip")

  fy25_boe_layers <- sf::st_layers(fy25_boe_path)

  # FIXME: Unsure if there is a better way to subset these layers
  fy25_boe_layer_names <- fy25_boe_layers[["name"]][9:11]

  fy25_boe_layer_names |>
    purrr::map(
      \(lyr) {
        sf::read_sf(fy25_boe_path, layer = lyr)
      }
    ) |>
    purrr::set_names(fy25_boe_layer_names) |>
    purrr::list_rbind(names_to = "layer") |>
    # TODO: This is not needed due to the specific layer selection above
    # filter(
    #   layer != "csa_noharbor",
    #   !is.na(project_code)
    # ) |>
    sf::st_as_sf() |>
    sf::st_set_geometry("geometry") |>
    janitor::clean_names() |>
    mutate(
      project_code = str_squish(project_code),
      cip_number = coalesce(cip_no, ci_pno),
      equity_level = case_when(
        equity_leve == 1 ~ "local",
        equity_leve == 2 ~ "regional",
        equity_leve == 3 ~ "citywide"
      ),
      fy = fy,
      totals = totals,
      geometry = geometry,
      layer = layer,
      .keep = "none",
      .before = everything()
    )
}

compare_fy25_boe_data <- function(
    reference_data = NULL,
    comparison_data = NULL) {
  reference_data <- reference_data %||% load_fy25_boe_data()

  comparison_data <- comparison_data %||% summarise_budget_year_data(
    budget_data = tar_read(cip_bof_recommendations),
    project_data = tar_read(cip_report_data)
  ) |>
    filter(fy2025 > 0)

  # comparison_data |>
  #   filter(
  #     fy2025 > 0
  #   ) |>
  #   nrow()
  # 237 projects

  location_comparison_data <- comparison_data |>
    filter(has_location_data)

  reference_data |>
    mutate(
      fy2025 = totals
    ) |>
    left_join(
      tar_read(cip_project_details) |>
        select(project_code, project_name, agency_label)
    ) |>
    filter(
      !(project_code %in% location_comparison_data$project_code)
    ) |>
    # names()
    select(
      !c(cup_number, equity_level)
    )

  location_comparison_data |>
    distinct(project_code, .keep_all = TRUE) |>
    filter(!(project_code %in% reference_data$project_code)) |>
    View()
  # nrow()

  fy2025_cip_locations <- location_comparison_data |>
    dplyr::select(!c(project_code, agency_label, location_asset_id)) |>
    tidyr::unnest("location_data") |>
    mutate(
      agency_label = if_else(
        !is.na(source) & is.na(agency_label),
        "Department of Transportation",
        agency_label
      )
    ) |>
    sf::st_as_sf() |>
    filter(
      !sf::st_is_empty(geometry)
    ) |>
    sf::st_write(
      "2024-05-06_FY2025-CIP-Locations.gpkg"
    )


  fy2025_cip_locations |>
    filter(!(project_code %in% reference_data$project_code)) |>
    sf::st_write(
      "2024-05-06_FY2025-CIP-Locations-Added.gpkg"
    )

  reference_data |>
    distinct(project_code) |>
    filter(project_code %in% location_comparison_data$project_code) |>
    # filter(!(project_code %in% location_comparison_data$project_code)) |>
    nrow()

  location_comparison_data |>
    View()

  # reference_data |>
  #   filter(totals > 0) |>
  #   distinct(project_code) |>
  #   nrow()
  # 141 projects in equity analysis data

  # comparison_data |>
  #   filter(
  #     fy2025 > 0,
  #     has_location_data
  #   ) |>
  #   anti_join(
  #     reference_data,
  #     by = "project_code"
  #   ) |>
  #   nrow()
  # 13 projects
}


summarise_budget_year_data <- function(
    budget_data,
    project_data,
    budget_year_col = "fy2025",
    detail_cols = c(
      "project_code", "project_name", "agency_label",
      "location", "location_asset_id", "location_data"
    )) {
  budget_data |>
    summarise(
      "{budget_year_col}" := sum(.data[[budget_year_col]]),
      .by = project_code
    ) |>
    # mutate(
    #   flag = case_when(
    #     fy2025 > 0 ~ "Funded in the FY25 BOF Recommendations",
    #     fy2025 < 0 ~ "Transfer in the FY25 BOF Recommendations",
    #     .default = "Not in the FY25 BOF Recommendations"
    #   )
    # ) |>
    left_join(
      project_data |>
        select(all_of(detail_cols)),
      by = join_by(project_code)
    ) |>
    mutate(
      has_location_data = map_lgl(
        location_data,
        \(x) {
          any(!sf::st_is_empty(sf::st_as_sf(x)))
        }
      )
    )
}
#
# summarise_budget_year_data(
#   budget_data = tar_read(cip_bof_recommendations),
#   project_data = tar_read(cip_report_data)
# ) |>
# full_join(
#   load_fy25_boe_data() |>
#     sf::st_drop_geometry()
# ) |>
#   View()


#
# path <- "/Users/elipousson/Downloads/downloads-as-of-2024-03-23/DOT-FY2025-CIP-Projects.geojson"
#
# dot <- sf::read_sf(path)
#
# library(tidyverse)
#
# cip_bof_recommendations <- tar_read(cip_bof_recommendations)
#
# cip_project_details <- tar_read(cip_project_details)
#
# bof_projects <- cip_bof_recommendations
#
# data |>
#   distinct(project_code, .keep_all = TRUE) |>
#   right_join(
#     bof_projects,
#     na_matches = "never"
#   ) |>
#   filter(
#     is.na(layer)
#   ) |>
#   select(
#     starts_with("project_"),
#     starts_with("location"),
#     flag#,
#     # before = everything()
#   ) |>
#   filter(
#     flag == "Funded in the FY25 BOF Recommendations"
#   ) |>
#   select(!flag) |>
#   sf::st_drop_geometry() |>
#   View()
#   readr::write_csv(
#     "2024-05-03_BOE-Equity-GDB_Compared-BOF-Report.csv"
#   )
