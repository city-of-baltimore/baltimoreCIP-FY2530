read_basemap_rds <- function(file) {
  path <- path_user_data("basemap", paste0(file, ".rds"))

  readr::read_rds(path)
}
