#' Format DGS Asset Data
#'
#' @inheritParams source
format_dgs_asset_list <- function(
    asset_data,
    attributes_data,
    dictionary,
    data_sheet = "MCC-ASSETS",
    attributes_sheet = "OtherDATA-FMS",
    rename_xwalk_cols = c("clean_name", "variable"),
    additional_data = NULL,
    crs = 3857,
    asset_agency_xwalk = NULL,
    agency_reference = NULL,
    names_to = rlang::zap()) {
  # return(dictionary)

  attributes_dictionary <- dictionary |>
    dplyr::filter(
      sheet == attributes_sheet
    )

  attributes_data <- attributes_data |>
    janitor::clean_names() |>
    select(!starts_with("na_")) |>
    rename_with_xwalk(
      xwalk = attributes_dictionary,
      xwalk_cols = rename_xwalk_cols
    )

  asset_data_dictionary <- dictionary |>
    dplyr::filter(
      sheet == data_sheet
    )

  asset_data_formatted <- asset_data |>
    janitor::clean_names() |>
    select(!starts_with("na_")) |>
    rename_with_xwalk(
      xwalk = asset_data_dictionary,
      xwalk_cols = rename_xwalk_cols
    ) |>
    left_join(
      y = attributes_data,
      by = join_by(asset_id),
      suffix = c("", "_attr")
    ) |>
    relocate(
      starts_with("asset"),
      .before = everything()
    )

  # return(asset_data_formatted)

  asset_data_formatted <- asset_data_formatted |>
    filter(!is.na(asset_id)) |>
    mutate(
      agency_abb = controlling_agency
    ) |>
    str_squish_across() |>
    format_asset_coords() |>
    format_asset_block_lot() |>
    derive_demolished_col() |>
    flag_asset_start_end_year() |>
    format_asset_name() |>
    format_asset_type_label() |>
    str_squish_across()

  if (!is.null(additional_data)) {
    if (!is.list(additional_data)) {
      additional_data <- list(additional_data)
    }

    additional_data <- sf_list_to_df_list(additional_data)

    asset_data_formatted <- purrr::list_rbind(
      c(
        list(asset_data_formatted),
        additional_data
      ),
      names_to = names_to
    )
  }

  # FIXME: Adjust this section to get coordinates from parcel data for the
  # assets that are do not have lon/lat values
  asset_data_formatted <- asset_data_formatted |>
    filter(!is.na(lon) & !is.na(lat))

  asset_data_formatted <- sf::st_as_sf(
    asset_data_formatted,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  ) |>
    sf::st_transform(crs = crs)

  county_locations <- asset_data_formatted |>
    dplyr::filter(
      !sf::st_is_empty(geometry)
    ) |>
    dplyr::select(asset_id) |>
    sf::st_join(
      mapbaltimore::baltimore_msa_counties |>
        # dplyr::rename(county) |>
        dplyr::select(county = namelsad) |>
        sf::st_transform(crs = crs),
      largest = TRUE
    ) |>
    sf::st_drop_geometry()

  if (FALSE && is_installed(c("mapbaltimore", "sfext"))) {
    # NOTE: The following code could be reproduced using sf and dplyr but uses the
    # custom mapbaltimore and sfext functions for convenience

    # pak::pkg_install("elipousson/mapbaltimore")
    # pak::pkg_install("elipousson/sfext")

    asset_data_mapbaltimore <- asset_data_formatted |>
      st_join_mapbaltimore()

    # Join neighborhoods, counties, and parks to asset list
    asset_data_formatted <- asset_data_formatted |>
      sf::st_drop_geometry() |>
      left_join(
        asset_data_mapbaltimore,
        na_matches = "never",
        relationship = "one-to-one",
        by = join_by(asset_id)
      ) |>
      sf::st_as_sf() |>
      str_squish_across()
  }

  # return(asset_data_formatted)

  asset_data_formatted |>
    format_asset_admin_id() |>
    select(!county) |>
    left_join(
      county_locations,
      by = join_by(asset_id),
      relationship = "one-to-one",
      na_matches = "never"
    ) |>
    left_join(
      asset_agency_xwalk |>
        rename(asset_agency_label = agency_name),
      by = join_by(agency_abb),
      relationship = "many-to-one",
      na_matches = "never"
    ) |>
    mutate(
      asset_agency_label = case_when(
        asset_occupant == "POLICE DEPT." ~ "Baltimore City Police Department",
        asset_occupant == "FIRE DEPT." ~ "Baltimore City Fire Department",
        responsible_agency == "POLICE DEPT." ~ "Baltimore City Police Department",
        responsible_agency == "FIRE DEPT." ~ "Baltimore City Fire Department",
        responsible_agency == "FIRE" ~ "Baltimore City Fire Department",
        responsible_agency == "HEALTH DEPT." ~ "Health Department",
        str_detect(
          asset_name_short,
          "^BCPD"
        ) ~ "Baltimore City Police Department",
        .default = asset_agency_label
      )
    )
  # left_join(
  #   agency_reference |>
  #     select(agency_label, agency_short_name),
  #   by = join_by(agency_name == agency_label)
  # )
}
