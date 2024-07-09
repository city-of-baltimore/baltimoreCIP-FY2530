match_csa_geography <- function(geography, error_call = caller_env()) {
  geography <- arg_match0(geography, c("csa", "city"), error_call = error_call)

  switch(geography,
    csa = 0,
    city = 1
  )
}

load_baltimore_csa_mhhi <- function(..., geography = "csa", crs = 3857) {
  url <- paste0(
    "https://services1.arcgis.com/mVFRs7NF4iFitgbY/ArcGIS/rest/services/Mhhi/FeatureServer/",
    match_csa_geography(geography)
  )

  arcgislayers::arc_read(
    url = url,
    ...,
    crs = crs
  )
}

load_baltimore_csa_paa <- function(..., geography = "csa", crs = 3857) {
  url <- paste0(
    "https://services1.arcgis.com/mVFRs7NF4iFitgbY/ArcGIS/rest/services/Paa/FeatureServer/",
    match_csa_geography(geography)
  )

  arcgislayers::arc_read(
    url = url,
    ...,
    crs = crs
  )
}
