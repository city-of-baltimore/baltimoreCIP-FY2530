#' Create path to targets user data folder
#'
#' @keywords targets utility
path_tar_user <- function(..., ext = "", dir = "data") {
  fs::path(
    "_targets",
    "user",
    dir,
    ...,
    ext = ext
  )
}
