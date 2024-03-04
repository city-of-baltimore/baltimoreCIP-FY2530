read_rds_paths <- function(path) {
  data <- purrr::map(
    path,
    readr::read_rds
  )

  nm <- fs::path_ext_remove(fs::path_file(names(data)))
  set_names(data, nm)
}
