read_baltimore_dot_bridges <- function(
    ...,
    crs = 3857) {
  dot_bridges_src <- arc_read(
    url = "https://geodata.baltimorecity.gov/egis/rest/services/CitiMap/DOT_Layers/MapServer/7",
    ...,
    crs = crs
  ) |>
    filter(
      is.na(NOTE_) | !str_detect(NOTE_, "REFER")
    )

  dot_bridges_coords <- dot_bridges_src |>
    convert_to_coords()

  dot_bridges_src |>
    st_set_geometry(value = "geometry") |>
    replace_with_na(
      replace = list(
        UNDERWTR_CMPLT = "N/A",
        INSP_START = c("", " "),
        FEDERAL_AID = c("", " "),
        ADT = -9,
        POSTING = "N/A",
        BSRLBL = "NA"
      )
    ) |>
    remove_empty("cols") |>
    select(
      !c("GlobalID")
    ) |>
    transmute(
      bridge_object_id = OBJECTID,
      asset_id = str_remove(BRDGNMBSR, "[:blank:]"),
      asset_name = str_to_title(
        LOCATION
      ),
      agency_name = "Department of Transportation",
      asset_type_label = "Bridge",
      block_lot_n = 1,
      asset_id_type = "BC",
      agency_abb = "DOT",
      right_of_way = TRUE,
      demolished = FALSE
    ) |>
    mutate(
      asset_name = str_replace_all(
        asset_name,
        "Jfx",
        "JFX"
      ),
      asset_name = str_replace(
        asset_name,
        "Csx",
        "CSX"
      ),
      asset_name = str_replace(
        asset_name,
        "Mta",
        "MTA"
      ),
      asset_name = str_replace(
        asset_name,
        "Over ",
        "over "
      ),
      asset_name = str_replace(
        asset_name,
        " Between",
        " between"
      ),
      asset_name = str_replace(
        asset_name,
        "And",
        "and"
      ),
      asset_name = str_replace(
        asset_name,
        "At ",
        "at "
      ),
      asset_name = str_replace(
        asset_name,
        "R.r.",
        "RR"
      ),
      asset_name = str_replace(
        asset_name,
        " Rr",
        " RR"
      ),
      asset_name = str_replace(
        asset_name,
        "To ",
        "to "
      ),
      asset_name = str_replace(
        asset_name,
        "From",
        "from"
      ),
      asset_name = str_replace(
        asset_name,
        "Ov ",
        "over "
      ),
      asset_name = str_replace(
        asset_name,
        "Under ",
        "under "
      ),
      asset_name = str_replace(
        asset_name,
        "Strm ",
        "Stream "
      ),
      asset_name = str_replace(
        asset_name,
        "Betwn\\.",
        "between"
      ),
      asset_name = str_replace(
        asset_name,
        "Of ",
        "of "
      ),
      asset_name = str_replace(
        asset_name,
        "Resersvoir",
        "Reservoir"
      ),
      asset_name = str_replace(
        asset_name,
        "(Eb|Eastbound|Eastbnd) ",
        "EB "
      ),
      asset_name = str_replace(
        asset_name,
        "(Wb|Westbound) ",
        "WB "
      ),
      asset_name = str_replace(
        asset_name,
        "(Nb|Northbound|Northbnd|N.b.) ",
        "NB "
      ),
      asset_name = str_replace(
        asset_name,
        "(Sb|Southbound|S.b.) ",
        "SB "
      ),
      asset_name = str_replace(
        asset_name,
        "Mt.pleasant",
        "Mt. Pleasant"
      ),
      asset_name = str_replace(
        asset_name,
        "O'donnell",
        "O'Donnell"
      ),
      asset_name = str_replace(
        asset_name,
        "Mckeldin",
        "McKeldin"
      ),
      asset_name = str_replace(
        asset_name,
        " Abandoned",
        " abandoned"
      ),
      asset_name = str_replace(
        asset_name,
        "RR.s.",
        "RRs"
      ),
      asset_name = str_replace(
        asset_name,
        "Hiway",
        "Highway"
      ),
      asset_name = str_replace(
        asset_name,
        "Stret",
        "Street"
      ),
      asset_name = str_replace(
        asset_name,
        "Nrthn",
        "Northern"
      )
      # asset_name = str_replace(
      #   asset_name,
      #   "Parkway",
      #   "Pkwy"
      # )
    ) |>
    bind_cols(dot_bridges_coords)
}

read_imap_maryland_bridges <- function(
    type = c("extent", "location", "condition"),
    year = NULL,
    county = NULL,
    where = NULL,
    col_select = NULL,
    col_names = TRUE,
    ...,
    crs = 3857) {
  type <- arg_match(type)

  if (type == "condition") {
    year <- as.character(year)
    stopifnot(is_string(year))
    year <- arg_match(year, c("2017", "2016"))
    type <- paste0(type, year)
  }

  url <- switch(type,
    extent = "https://services.arcgis.com/njFNhDsUCentVYJW/ArcGIS/rest/services/Maryland_Bridge_External/FeatureServer/0",
    location = "https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/Maryland_Bridge_External/FeatureServer/1",
    condition2017 = "https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/Bridge_Condition_NHS_2017/FeatureServer/0",
    condition2016 = "https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/Bridge_Condition_NHS_2016/FeatureServer/0"
  )

  if (!is.null(county) && is.null(where)) {
    where <- where_in_sql(county, "COUNTY_CODE")
  }

  arc_read(
    url,
    col_select = col_select,
    where = where,
    col_names = col_names,
    crs = crs,
    ...
  ) |>
    st_set_geometry("geometry")
}

load_baltimore_bridge_inventory <- function(
    crs = 3857) {
  dot_bridge_inventory <- read_baltimore_dot_bridges(crs = crs) |>
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
    county = c(5, 510),
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
