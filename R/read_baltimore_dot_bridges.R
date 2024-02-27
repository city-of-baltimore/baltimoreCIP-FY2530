#' Read Baltimore DOT Bridge Inventory data from ArcGIS Data Service
#'
#' Read data for the Baltimore DOT Bridge Inventory, tidy the attributes and add
#' additional attributes to combine with the DGS asset data.
#'
#' @param url url to access with [arcgislayers::arc_read()]. Defaults to the
#'   CitiMap MapServer layer with the bridge inventory data.
#' @inheritParams arcgislayers::arc_read
#' @inheritDotParams arcgislayers::arc_read
read_baltimore_dot_bridges <- function(
    url = "https://geodata.baltimorecity.gov/egis/rest/services/CitiMap/DOT_Layers/MapServer/7",
    ...,
    crs = 3857) {
  dot_bridges_src <- arc_read(
    url = url,
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
