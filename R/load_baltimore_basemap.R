basemap_simplify <- function(data,
                             keep = 0.004,
                             crs = 3857,
                             smooth_method = NULL,
                             weighting = 0.7,
                             ...) {
  data <- data |>
    sf::st_transform(crs)

  data <- suppressWarnings(
    rmapshaper::ms_simplify(
      data,
      keep = keep,
      weighting = weighting
    )
  )

  if (is.null(smooth_method)) {
    return(data)
  }

  smoothr::smooth(
    data,
    method = smooth_method,
    ...
  )
}

basemap_prep <- function(data,
                         keep = 0.004,
                         crs = 3857,
                         smooth_method = "chaikin",
                         refinements = 1,
                         remove_slivers = TRUE,
                         clip = NULL) {
  data <- basemap_simplify(
    data = data,
    keep = keep,
    crs = crs,
    smooth_method = smooth_method,
    refinements = refinements
  )

  if (!is.null(clip)) {
    clip <- sf::st_transform(clip, crs = crs)

    data <- rmapshaper::ms_clip(
      data,
      clip = clip,
      remove_slivers = remove_slivers
    )
  }

  data
}

load_baltimore_basemap_water <- function(crs = 3857) {
  baltimore_water |>
    dplyr::filter(
      !is.na(name),
      !(name %in% c("Druid Lake", "Lake Ashburton")),
      type != "NON"
    ) |>
    rmapshaper::ms_dissolve() |>
    basemap_prep(crs = crs, clip = baltimore_city)
}

load_baltimore_msa_basemap_water <- function(crs = 3857) {
  data <- suppressWarnings(rmapshaper::ms_dissolve(baltimore_msa_water))

  basemap_prep(
    data = data,
    crs = crs,
    clip = baltimore_msa_counties
  )
}

load_baltimore_basemap_streets <- function(msa = FALSE) {
  if (msa) {
    cli_abort("MSA streets is not available")
  }

  streets <- streets |>
    sf::st_transform(3857) |>
    dplyr::filter(
      sha_class %in% c("INT", "MART", "PART")
    ) |>
    sf::st_intersection(baltimore_city)

  highways <- streets |>
    dplyr::filter(
      sha_class == "INT"
    ) |>
    sf::st_combine() |>
    sf::st_union()
}
