#' Combine the Baltimore City DOT Bridge inventory with geometry of bridge
#' extents from Maryland iMap
#'
#' @inheritParams read_baltimore_dot_bridges
load_baltimore_bridge_inventory <- function(
    url = "https://geodata.baltimorecity.gov/egis/rest/services/CitiMap/DOT_Layers/MapServer/7",
    type = "extent",
    county = c(5, 510),
    crs = 3857) {
  dot_bridge_inventory <- read_baltimore_dot_bridges(
    url = url,
    crs = crs
  ) |>
    mutate(
      bridge_id_short = str_remove(
        asset_id,
        " (B|C|D|R|X|1|2|3|4)$"
      ),
      .before = asset_id
    ) |>
    st_drop_geometry()

  dot_bridge_inventory_join <- dot_bridge_inventory |>
    left_join(
      dot_bridge_inventory |>
        count(
          bridge_id_short,
          name = "asset_id_count"
        ),
      by = join_by(bridge_id_short)
    ) |>
    filter(
      asset_id_count < 2
    )

  read_imap_maryland_bridges(
    type = type,
    county = county,
    fields = c("COUNTY_CODE", "BRIDGE_NUMBER", "STRUCTURE_NUMBER"),
    col_names = c("county_code", "bridge_number", "structure_number"),
    crs = crs
  ) |>
    mutate(
      bridge_id_short = str_sub(
        bridge_number,
        end = -4
      )
    ) |>
    left_join(
      dot_bridge_inventory_join,
      by = join_by(bridge_id_short)
    ) |>
    filter(!is.na(asset_id))
}
