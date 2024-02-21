#' Convert a sf object to a data frame of coordinates
#'
#' @inheritParams round
covert_to_coords <- function(data, coords = c("lon", "lat"), digits = 8) {
  data |>
    sf::st_centroid() |>
    suppressWarnings() |>
    sf::st_transform(4326) |>
    sf::st_coordinates() |>
    as.data.frame() |>
    set_names(coords) |>
    mutate(
      across(
        all_of(coords),
        \(x) {
          round(x, digits = digits)
        }
      )
    )
}

#' Format coordinate columns from DGS asset data
#'
#' @inheritParams round
format_asset_coords <- function(data, coords = c("lon", "lat"), digits = 8) {
  stopifnot(
    all(has_name(data, coords))
  )

  # FIXME: lat/lon are hard-coded values here despite being exposed as a parameter
  mutate(
    data,
    # Fix issue with incorrectly formatted latitude field
    lat = if_else(
      str_detect(lat, ","),
      readr::parse_number(str_extract(lat, "([:blank:]|-|[:digit:]+)(?=,)")),
      readr::parse_number(lat)
    ),
    lon = readr::parse_number(lon),
    # Convert lat and lon into numeric values with 6 digits
    across(
      all_of(coords),
      \(x) {
        round(x, digits = digits)
      }
    )
  )
}

#' Format asset block and lot columns by correcting typos and splitting ROW
#' flags into a separate column
#'
#' @param block_lot_col Column name for column containing block lot value
#'   separated by an underscore character
format_asset_blocklot <- function(data,
                                  block_lot_col = "block_lot") {
  data |>
    mutate(
      # Correct non-standard block_lot for consistency
      # FIXME: Figure out a way to catch these programmatically (if needed)
      block_lot := case_when(
        .data[[block_lot_col]] == "3357F010" ~ "3357F_010",
        .default = .data[[block_lot_col]]
      ),
      # Flag ROW data with no corresponding block_lot value
      right_of_way = (block_lot == "R.O.W.")
    ) |>
    naniar::replace_with_na(
      list(
        # Remove county and ROW identifiers from block_lot column
        block_lot = c(
          "Balt_Cnty", "Harf County", "Carroll_Cnty",
          "R.O.W.", "_"
        )
      )
    ) |>
    # FIXME: block_lot records for properties outside of Baltimore result in a
    # warning 1: Expected 2 pieces. Additional pieces discarded in 1 rows
    # [1130].
    tidyr::separate(
      block_lot,
      into = c("block", "lot"),
      sep = "_",
      remove = FALSE
    )
}

#' Flag demolished buildings and structures based on asset_name and asset_type
flag_demolished <- function(data) {
  if (!has_name(data, "demolished")) {
    data <- mutate(
      data,
      demolished = FALSE
    )
  }

  if (has_name(data, "asset_name")) {
    data <- mutate(
      data,
      demolished = str_detect(asset_name, "DEMOLISHED|Demolished") | demolished
    )
  }


  if (has_name(data, "asset_type")) {
    data <- mutate(
      data,
      # FIXME: This previously used asset_type == "DEMOLISHED" - make sure this
      # doesn't flag anything as demolished that isn't actually demolished
      demolished = str_detect(asset_type, "DEMOLISHED|DEMO") | demolished
    )
  }

  data
}

#' Create a short asset name column by abbreviated specified terms
format_asset_name_short <- function(data) {
  stopifnot(
    has_name(data, "asset_name")
  )

  mutate(
    data,
    asset_name_short = asset_name,
    asset_name_short = str_replace(
      asset_name_short,
      "Building",
      "Bldg"
    ),
    asset_name_short = str_replace(
      asset_name_short,
      "Baltimore City Police",
      "BCPD"
    ),
    asset_name_short = str_replace(
      asset_name_short,
      "Department of Transportation",
      "DOT"
    ),
    asset_name_short = str_replace(
      asset_name_short,
      "Recreation Center",
      "Rec Center"
    ),
    asset_name_short = str_replace(
      asset_name_short,
      "Community Action Center",
      "CAC"
    )
  )
}

#' Format asset name by stripping extraneous text
#'
#' Note that this function must be applied *after* [flag_demolition()] or any
#' other function that relies on the supplementary information included in the
#' asset_name column.
format_asset_name <- function(data) {
  mutate(
    data,
    # Remove extra start and end year information from asset names
    asset_name = str_remove(
      asset_name,
      paste0(
        c(
          "\\(NEW (2021|2022|2013)\\)|- New 2013",
          "\\(DEMOLISHED-(2021|2020)\\)|\\(DEMOLISHED 2020\\)|\\(DEMOLISHED\\)|\\(Demolished\\)"
        ),
        collapse = "|"
      )
    ),
    # TODO: Consider adding a field for storing information on the last major
    # renovation for a building
    asset_name = str_remove(
      asset_name,
      " \\(Renovated 2021\\)"
    ),
    asset_name = str_remove(
      asset_name,
      " \\(RENOVATED 2021\\)"
    ),
    asset_name = str_remove(
      asset_name,
      " \\(RENOVATED-2021\\)"
    ),
    asset_name = str_remove(
      asset_name,
      " \\(REMOVED 2021\\)"
    ),
    asset_name = str_remove(
      asset_name,
      " \\(HCD-BDC\\)"
    ),
    asset_name = str_remove(
      asset_name,
      "\\(\\(PENDING SALE 2021\\)"
    ),
    asset_name = str_remove(
      asset_name,
      " \\(PENDING SALE 2021\\)"
    ),
    asset_name = str_remove(
      asset_name,
      " \\(DEMOLISHED 2021\\)"
    ),
    asset_name = str_remove(
      asset_name,
      " \\(MOVED OUT 2021\\)"
    )
  ) |>
    mutate_trim_squish()
}

#' Create asset ID type column based on the leading prefix from the asset id column
format_asset_id_type <- function(data) {
  stopifnot(
    has_name(data, "asset_id")
  )

  mutate(
    data,
    asset_id_type = str_extract(asset_id, "[:alpha:]"),
    .after = asset_id
  )
}


#' Add asset_id_type and asset_type_label columns
format_asset_type_label <- function(data) {
  stopifnot(
    all(has_name(data, c("asset_id", "asset_name", "asset_type")))
  )

  if (!has_name(data, "asset_id_type")) {
    data <- format_asset_id_type(data)
  }

  mutate(
    data,
    asset_type_label = case_when(
      asset_id_type == "L" ~ "Land",
      str_detect(asset_name, "Pool") & !str_detect(asset_name, "Building") ~ "Pool",
      str_detect(asset_name, "Seating") ~ "Seating",
      str_detect(asset_name, "Playground") ~ "Playground",
      str_detect(asset_name, "Modular") ~ "Modular building",
      str_detect(asset_type, "EQUIP") ~ "Equipment",
      str_detect(asset_type, "OPEN SPACE") ~ "Open space",
      str_detect(asset_type, "BUILDING") ~ "Building",
      str_detect(asset_type, "STRUCT") ~ "Structure",
      asset_type == "BCPSS ASSET" ~ "Building",
      str_detect(asset_name, "Building") ~ "Building",
      .default = "Other"
    ),
    .after = asset_type
  )
}

#' Extract administration ID values for schools, parks, BCFD engines, and library branches
format_asset_admin_id <- function(data) {
  stopifnot(
    all(has_name(data, c("asset_name", "asset_id")))
  )

  data |>
    mutate(
      school_id = str_extract(
        asset_name,
        "^P.S. ([:digit:]+)"
      ),
      park_id = str_extract(
        asset_name,
        "PRK-[:digit:]{3}"
      ),
      engine_id = str_extract(
        asset_name,
        "Station Engine [:digit:]{1,2}"
      ),
      branch_num = str_extract(
        asset_name,
        "EPFL No. [:digit:]{1,2}"
      ),
      # Remove park ID and school ID from asset names
      asset_name = str_remove(asset_name, paste0("^", park_id)),
      asset_name = str_remove(asset_name, paste0("^", school_id, " - ")),
      # Convert ID values into numbers
      across(
        all_of(c("park_id", "school_id", "engine_id", "branch_num")),
        str_extract_number
      ),
      .after = asset_name
    ) |>
    mutate(
      park_id = case_when(
        # FIXME: Document this duplication issue
        asset_id == "S06602" ~ NA_integer_,
        .default = park_id
      )
    ) |>
    mutate(
      admin_id = coalesce(
        branch_num, school_id, engine_id, park_id
      ),
      admin_id_type = case_when(
        !is.na(branch_num) ~ "library",
        !is.na(park_id) ~ "park",
        !is.na(school_id) ~ "school",
        !is.na(engine_id) ~ "engine",
        .default = NA_character_
      )
    ) |>
    mutate(
      # Drop admin_id and admin_id_type for "Patterson Park Softball Field"
      # (S00037) and "Clifton Park Baseball Fields" (S07001)
      admin_id_type = if_else(
        asset_id %in% c("S00037", "S07001"),
        NA_character_,
        admin_id_type
      ),
      admin_id = if_else(
        asset_id %in% c("S00037", "S07001"),
        NA_integer_,
        admin_id
      )
    ) |>
    select(
      !any_of(c("branch_num", "school_id", "engine_id", "park_id"))
    )
}

#' Complete spatial joins to planning distict, neighborhood, county, and park data from mapbaltimore
st_join_mapbaltimore <- function(data) {
  stopifnot(
    has_name(data, "asset_id"),
    inherits(data, "sf"),
    is_installed("mapbaltimore"),
    is_installed("sfext")
  )

  # Join to neighborhoods, planning districts, counties, and parks
  data |>
    sfext::st_join_ext(
      mapbaltimore::planning_districts |>
        select(planning_district = name),
      largest = TRUE,
      .id = "planning_district"
    ) |>
    sfext::st_join_ext(
      mapbaltimore::neighborhoods_2020 |>
        select(neighborhood = name),
      largest = TRUE,
      .id = "neighborhood"
    ) |>
    sfext::st_join_ext(
      mapbaltimore::baltimore_msa_counties |>
        select(county = namelsad),
      largest = TRUE,
      .id = "county"
    ) |>
    sfext::st_join_ext(
      mapbaltimore::parks |>
        select(park = name),
      largest = TRUE,
      .id = "park"
    ) |>
    select(
      asset_id, neighborhood, park, planning_district, county
    )
}

#' Format block and lot columns in iMap parcel data to join to city asset data
format_imap_parcel_block_lot <- function(data) {
  stopifnot(
    all(has_name(data, c("block", "lot")))
  )

  mutate(
    data,
    block = if_else(
      str_detect(block, "[:alpha:]"),
      str_pad(block, width = 5, pad = "0"),
      str_pad(block, width = 4, pad = "0")
    ),
    lot = if_else(
      str_detect(lot, "[:alpha:]"),
      str_pad(lot, width = 4, pad = "0"),
      str_pad(lot, width = 3, pad = "0")
    )
  )
}

#' Flag the asset start and end year based on common patterns in the asset name and asset type columns
flag_asset_start_end_year <- function(data) {
  stopifnot(
    all(has_name(data, c("asset_name", "asset_type")))
  )

  if (!has_name(data, "start_year")) {
    data <- mutate(
      data,
      start_year = NA_integer_
    )
  }

  if (!has_name(data, "end_year")) {
    data <- mutate(
      data,
      end_year = NA_integer_
    )
  }

  mutate(
    data,

    # Extract start and end years from asset name
    start_year = case_when(
      str_detect(asset_name, "NEW 2021") ~ 2021,
      str_detect(asset_name, "NEW 2022") ~ 2022,
      str_detect(asset_name, "NEW 2013") ~ 2013,
      str_detect(asset_name, "- New 2013") ~ 2013,
      .default = start_year
    ),
    end_year = case_when(
      str_detect(asset_name, "DEMOLISHED-2021") ~ 2021,
      str_detect(asset_name, "DEMOLISHED 2021") ~ 2021,
      str_detect(asset_type, "DEMO SUMMER 2021") ~ 2021,
      str_detect(asset_name, "DEMOLISHED 2020") ~ 2020,
      str_detect(asset_name, "DEMOLISHED-2020") ~ 2020,
      .default = end_year
    )
  )
}

#' Update asset names based on a crosswalk file or dataframe
update_asset_names <- function(data, update_data = NULL) {
  stopifnot(
    all(has_name(data, c("asset_id", "asset_name", "start_year")))
  )

  if (is.null(update_data)) {
    update_data <- tarchetypes::tar_file_read(
      asset_name_updates,
      "asset_name_updates.csv",
      read = readr::read_csv(file = !!.x, show_col_types = FALSE)
    ) |>
      select(asset_id = id, name_update, name_alt, year_built_update)
  }

  stopifnot(
    all(has_name(
      update_data,
      c("asset_id", "name_update", "year_built_update")
    ))
  )

  data |>
    left_join(
      update_data,
      by = join_by(asset_id)
    ) |>
    mutate(
      asset_name = if_else(
        !is.na(name_update),
        name_update,
        asset_name
      )
    ) |>
    mutate(
      start_year = if_else(
        !is.na(year_built_update),
        year_built_update,
        start_year
      )
    ) |>
    select(!c(name_update, year_built_update))
}


