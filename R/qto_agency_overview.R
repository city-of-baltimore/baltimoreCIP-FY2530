#' Create a Markdown text block with an agency overview using `{quartools}`
#'
#' Used in `_agency_report.qmd`
#' @seealso [qto_external_link()]
qto_agency_overview <- function(
    agency,
    agency_level = 2,
    heading = "Agency overview",
    heading_level = agency_level + 1,
    ...,
    call = caller_env()) {
  check_data_frame(agency)
  agency_link <- NULL

  if (is_string(agency[["agency_url"]])) {
    agency_link <- qto_external_link(agency[["agency_url"]])
  }

  quartools::qto_div(
    quartools::qto_heading(
      agency[["agency_label"]],
      level = agency_level
    ),
    quartools::qto_heading(heading, level = heading_level),
    quartools::qto_block(
      agency[["overview"]],
      call = call
    ),
    agency_link,
    collapse = "\n\n",
    # ...,
    call = call
  )
}
