load_packages <- function(pkg) {
  invisible(sapply(pkg, \(x) {
    suppressPackageStartupMessages(require(x, character.only = TRUE))
  }))
}

load_packages(
  c(
    "targets",
    "tarchetypes",
    "rlang",
    "cli",
    "gt",
    "quartools",
    "here",
    "naniar",
    "janitor",
    "glue",
    "dplyr",
    "stringr",
    "sf",
    "arcgislayers",
    "checkmate",
    "ggplot2",
    "purrr"
  )
)
