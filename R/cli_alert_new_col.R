#' Alert highlighting the addition of a new column based on an existing column
cli_alert_new_col <- function(new_col, existing_col) {
  cli::cli_alert_success(
    "Adding new {.arg {new_col}} column{?s} based on {.arg {existing_col}} column{?s}"
  )
}
