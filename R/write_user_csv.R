#' Write CSV file to the user data reference folder
#'
#' @keywords targets
write_user_csv <- function(data,
                           file = NULL,
                           path = c("_targets", "user", "data", "reference")) {
  ext <- "csv"

  file <- path_user_data(
    fs::path_ext_set(file, ext),
    path = path
  )

  readr::write_csv(
    data,
    file = file
  )
}
