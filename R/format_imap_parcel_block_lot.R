#' Format block and lot columns in iMap parcel data to join to city asset data
format_imap_parcel_block_lot <- function(property_data) {
  stopifnot(
    all(has_name(property_data, c("block", "lot")))
  )

  mutate(
    property_data,
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
