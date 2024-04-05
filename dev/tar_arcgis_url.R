#' @export
arc_meta <- function(url,
                     token = NULL,
                     ...) {
  meta <- arcgisutils::fetch_layer_metadata(
    url = url,
    token = token,
    ...
  )

  if (hasName(meta, "editingInfo")) {
    meta[["editingInfo"]] <- lapply(
      meta[["editingInfo"]],
      arcgisutils::from_esri_date
    )
  }

  meta
}


tar_arcgis_url <- function(url,
                           token = NULL) {
  meta <- arc_meta(url, token = token)
  meta
}
