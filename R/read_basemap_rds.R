#' Read prepared RDS data with citywide or regional basemap from file
#'
#' @param file File name without extension.
#' See <https://github.com/elipousson/tigris-basemap> for pipeline used to
#' prepare basemap plot
#' @importFrom readr read_rds
read_basemap_rds <- function(file) {
  path <- path_tar_user("basemap", paste0(file, ".rds"))

  readr::read_rds(path)
}
