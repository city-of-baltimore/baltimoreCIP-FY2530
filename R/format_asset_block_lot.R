#' Format asset block and lot columns by correcting typos and splitting ROW
#' flags into a separate column
#'
#' @param block_lot_col Column name for column containing block lot value
#'   separated by an underscore character
format_asset_block_lot <- function(asset_data,
                                   block_lot_col = "block_lot") {
  asset_data |>
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
