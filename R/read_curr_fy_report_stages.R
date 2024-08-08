#' Load CIP Report Stage details from prepared CSV file
read_curr_fy_report_stages <- function(
    curr_stage = NULL,
    path = path_tar_user("fy2530_report_stages.csv")) {
  report_stages <- readr::read_csv(
    path,
    show_col_types = FALSE
  )

  if (is.null(curr_stage)) {
    return(report_stages)
  }

  report_stages |>
    dplyr::mutate(
      is_curr_stage = stage == curr_stage
    )
}
