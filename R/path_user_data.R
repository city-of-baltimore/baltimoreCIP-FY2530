#' Create path to targets user data folder
#'
#' @keywords targets utility
path_user_data <- function(...,
                           path = c("_targets", "user", "data")) {
  here::here(
    paste0(path, collapse = .Platform$file.sep),
    ...
  )
}
