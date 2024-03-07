#' Load cached Maryland iMap parcel data from the `{mapbaltimore}` package cache
#'
#' @param data,pkg Passed to [sfext::read_sf_pkg()]. Only change these values if
#'   the file name or package name used in caching the dataset changes.
load_baltimore_imap_parcels <- function(data = "baltimore_imap_parcels",
                                        pkg = "mapbaltimore") {
  baltimore_imap_parcels_src <- sfext::read_sf_pkg(
    data = data,
    pkg = pkg
  )

  baltimore_imap_parcels <- baltimore_imap_parcels_src |>
    # Pad block and lot values to match asset data
    format_imap_parcel_block_lot() |>
    # Select block and lot join columns, account ID, year built, ownership
    # columns, and descriptive columns (from CAMA data)
    select(
      section, block, lot, acctid,
      yearblt, sqftstrc,
      starts_with("own"),
      contains("use"), contains("lu"),
      contains("cnst"), contains("bldg"),
      contains("styl")
    ) |>
    mutate(
      across(
        where(is.character),
        \(x) {
          if_else(
            x == "",
            NA_character_,
            x
          )
        }
      )
    ) |>
    filter(
      # FIXME: I think this property has an incorrect block lot value and is
      # matching in error
      acctid != "04090923503140"
    ) |>
    str_squish_across() |>
    sf::st_set_geometry("geometry")

  baltimore_imap_parcel_coords <- baltimore_imap_parcels |>
    select(
      block,
      lot
    ) |>
    sf::st_drop_geometry() |>
    bind_cols(
      convert_to_coords(baltimore_imap_parcels)
    )
}
