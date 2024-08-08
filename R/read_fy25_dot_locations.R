read_fy25_dot_locations <- function(...) {
  file_list <- list(
    "Footways/Alleys" = path_tar_user(
      "spatial",
      "DOT_CIP_FY2025_FootwaysAlleys",
      "DOT_CIP_FY2025_FootwaysAlleys.shp"
    ),
    "Locations" = path_tar_user(
      "spatial",
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

  dot_locations_add <- sf::read_sf(
    path_tar_user("spatial", "DOT-FY2025-CIP-Projects_Additional.geojson")
  ) |>
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
