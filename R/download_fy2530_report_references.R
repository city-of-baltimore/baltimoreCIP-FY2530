download_fy2530_report_references <- function(
    fileext = "csv",
    path = path_tar_user()) {
  # Load from Airtable
  p_hierarchy_xwalks <- load_p_hierarchy_xwalks()

  # Load crosswalks from Google Sheet
  xwalk_sheets <- load_sheets_list(
    pattern = "_xwalk$"
  )

  # Load additional reference data from Google Sheet
  reference_sheets <- load_sheets_list(
    sheets = c(
      "adaptive_dictionary",
      "fy2530_report_stages",
      "agency_reference",
      "project_detail_updates",
      "location_updates"
    )
  )

  stopifnot(fileext == "csv")

  iwalk(
    c(p_hierarchy_xwalks, xwalk_sheets, reference_sheets),
    \(x, nm) {
      readr::write_csv(
        x,
        file = fs::path(path, fs::path_ext_set(nm, fileext))
      )
    }
  )
}
