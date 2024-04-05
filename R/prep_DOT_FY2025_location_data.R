# source(here::here("R", "setup.R"))

load_dot_locations <- function() {
  file_list <- list(
    "Footways/Alleys" = path_user_data(
      "DOT_CIP_FY2025_FootwaysAlleys",
      "DOT_CIP_FY2025_FootwaysAlleys.shp"
    ),
    "Locations" = path_user_data(
      "DOT_CIP_FY2025_Locations",
      "DOT_CIP_FY2025_Locations.shp"
    )
  )

  dot_locations <- map(
    file_list,
    \(x){
      data <- sf::read_sf(x) |>
        sf::st_zm() |>
        sf::st_transform(3857) |>
        sf::st_make_valid()

      if (has_name(data, "Proj_Num")) {
        # Format alley/sidewalk data
        data <- data |>
          rename(
            project_code = Proj_Num,
            street_address = Proj_Strt,
            from_street = Frm_Lmt,
            to_street = To_Lmt,
            agency_project_type = Section # ,
            # location_total_amt = Total
          ) |>
          mutate(
            street_address = str_squish(str_to_title(street_address)),
            street_name = str_remove(street_address, "^[:digit:]([:digit:]|[:punct:])+")
          ) |>
          select(!Proj_Titl)
      } else {
        # Format project location data
        data <- data |>
          rename(
            agency_project_type = PrjType,
            project_code = ProjectNum,
            street_name = PrStreet,
            from_street = FrmLimit,
            to_street = ToLimit # ,
            # location_total_amt = TotalFunds
          )
      }

      data
    }
  )

  dot_locations <- dot_locations |>
    list_rbind(names_to = "source") |>
    sf::st_as_sf() |>
    # Drop unnecessary/outdated column names
    select(
      !any_of(c(
        "City", "GIS_ID", "Notes", "Shape_Leng", "ZIP_Code",
        "Total", "TotalFunds",
        "Federal", "General", "Other",
        "FedFund", "GenFund", "OthFund", "CTBFund",
        "CIPTitl"
      ))
    ) |>
    # Tidy and reorder remaining columns
    naniar::replace_with_na(
      replace = list(
        from_street = "N/A",
        to_street = "N/A"
      )
    ) |>
    relocate(
      starts_with("project_"),
      ends_with("_amt"),
      starts_with("street_"),
      ends_with("_street")
    )

  dot_locations_add <- sf::read_sf(path_user_data("DOT-FY2025-CIP-Projects_Additional.geojson")) |>
    dplyr::mutate(
      agency_project_type = "Bike",
      source = "Added",
      street_name = case_match(
        project_code,
        "PRJ002536" ~ "N. Wolfe Street/S. Wolfe Street",
        "PRJ002536" ~ "N. Washington Street/S. Washington Street",
        "PRJ002178" ~ "E. 33rd Street"
      )
    ) |>
    select(dplyr::any_of(names(dot_locations))) |>
    sf::st_transform(3857)

  # Join project name, cost center, and total request amount
  # dot_locations <- dot_locations |>
  #   dplyr::bind_rows(dot_locations_add)

  purrr::list_rbind(
    list(
      dot_locations,
      dot_locations_add
    )
  )
}

# Add missing project locations
# dot_locations |>
#   left_join(
#     reportable_requests |>
#       select(project_code,
#              project_name,
#              cost_center,
#              # project_name_src,
#              total_request_amt),
#     by = join_by(project_code)
#   ) |>
#   mutate_trim_squish()
#
# # Export combined data from
# dot_locations |>
#   sf::write_sf(here::here("files", "FY2025_DOT_Projects.gpkg"))
#
# asset_parcels <- readr::read_rds("data/asset_parcels.rds")
#
# dot_locations <- sf::read_sf(here::here("files", "FY2025_DOT_Projects.gpkg")) |>
#   sfext::rename_sf_col()
#
# missing_dot_fy25_requests <- reportable_requests |>
#   filter(
#     agency_label == "Department of Transportation",
#     !(project_code %in% dot_locations$project_code),
#     !(project_code %in% dot_locations_add$project_code),
#     has_requests_2025
#   ) |>
#   pull(location_data, name = "project_code") |>
#   list_rbind(names_to = "project_code") |>
#   select(!geometry) |>
#   dplyr::left_join(
#     reportable_requests |>
#       select(project_code,
#              asset_id = location_asset_id, # location_asset_name,
#              total_request_amt,
#              cost_center),
#     by = join_by(project_code)
#   ) |>
#   mutate_trim_squish() |>
#   mutate(
#     agency_project_type = "Bike",
#     source = "Added",
#     street_name = case_match(
#       project_code,
#       "PRJ003210" ~ "S. Potomac Street"
#     )
#   ) |>
#   left_join(
#     asset_parcels |>
#       select(asset_id, geometry),
#     by = join_by(asset_id)
#   ) |>
#   sf::st_as_sf() |>
#   sfext::rename_sf_col() |>
#   select(any_of(names(dot_locations)))
#
# dot_locations <- dot_locations |>
#   bind_rows(
#     missing_dot_fy25_requests
#   ) |>
#   mutate(
#     reconstruction_label = case_when(
#       str_detect(project_name, "Alley") ~ "Alley",
#       str_detect(project_name, "Sidewalk") ~ "Sidewalk"
#     ),
#     geometry = case_when(
#       cost_center == "Bridges" ~ sf::st_centroid(geometry),
#       .default = geometry
#     )
#   )
#
# # Join project code count back to location data
# dot_locations <- dot_locations |>
#   left_join(
#     dot_locations |>
#       sf::st_drop_geometry() |>
#       count(project_code, name = "n_project_code"),
#     relationship = "many-to-one",
#     by = join_by(project_code)
#   ) |>
#   mutate(
#     # Calculate estimated location cost by dividing total by number of locations
#     location_request_amt_est = total_request_amt / n_project_code,
#     .after = total_request_amt
#   )
#
# dot_locations |>
#   readr::write_rds(here::here("data/FY25_dot_locations.rds"))
#
# if (FALSE) {
#   dot_locations |>
#     getdata::make_variable_dictionary(
#       details = "full"
#     ) |>
#     View()
# }
