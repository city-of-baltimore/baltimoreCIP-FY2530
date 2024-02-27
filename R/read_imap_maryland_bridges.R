#' Read Maryland iMap Bridge data
#'
#' @param type Type of service to retrieve: "extent", "location", or "condition" (2017 or 2016).
#' @param year Year to use if type is "condition"
#' @param county FIPS code(s) for Maryland county. Ignored if `where` is supplied.
#' @inheritParams arcgislayers::arc_read
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
