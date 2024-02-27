#' Update asset names based on a crosswalk file or dataframe
update_asset_names <- function(asset_data, update_data = NULL) {
  stopifnot(
    all(has_name(asset_data, c("asset_id", "asset_name", "start_year")))
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

  asset_data |>
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
