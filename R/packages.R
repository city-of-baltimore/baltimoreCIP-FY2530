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
    "gt", # pak::pkg_install("gt@0.10.1")
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
    "purrr",
    "nombre",
    "testthat",
    "readr",
    "tibble",
    "lubridate",
    "janitor",
    "forcats",
    "kableExtra",
    "osmdata",
    "rmapshaper"
  )
)
