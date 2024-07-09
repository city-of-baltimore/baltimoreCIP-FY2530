#' Extract administration ID values for schools, parks, BCFD engines, and library branches
#'
#' Must include columns named "asset_id", "asset_name"
#' @inheritParams asset_data
format_asset_admin_id <- function(asset_data) {
  check_names(asset_data, must.include = c("asset_id", "asset_name"))

  asset_data |>
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
