# base_size <- 12
# family_mono <- "Source Code Pro"
# family_sans <- "Raleway"
# family_mono <- "Raleway"
## ---- locator_basemap_theme
#' Create a locator basemap theme
locator_basemap_theme <- function() {
  check_installed("maplayer")

  list(
    theme_void(),
    theme(
      panel.border = element_blank(),
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_blank()
    ),
    maplayer::theme_sf_axis()
  )
}
