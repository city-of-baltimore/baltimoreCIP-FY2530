## ---- qto_callout_agency_requests ----
#' Display summary text about agency projects with requests
#'
#' Used in _agency_project_request.qmd and _agency_report_standalone.qmd
#'
qto_callout_agency_requests <- function(
    data,
    agency_label,
    type = "note",
    appearance = "minimal",
    cip_type = "recommendations",
    total_amt_col = "total_amt",
    callout = TRUE) {
  # Format request amount
  total_amt <- vec_fmt_currency(
    sum(data[[total_amt_col]]),
    decimals = 0
  )

  n_projects <- nrow(data)

  n_projects_text <- n_what(n_projects)

  n_locations <- sum(!is.na(data[["location"]]))

  n_citywide <- sum(data[["location"]] %in% c(
    "Citywide",
    "Multiple areas citywide",
    "Multiple locations citywide",
    "Citywide infrastructure"
  ))

  n_location_text <- n_what(
    n_locations - n_citywide,
    .f = str_to_sentence
  )

  cip_type <- str_to_title(cip_type)

  amt_text <- glue(
    "{cip_type} for {agency_label} total {total_amt} for {n_projects_text}."
  )

  type_text <- ""
  location_text <- ""

  n_locations <- n_locations - n_citywide

  if (n_locations == n_projects) {
    location_text <- pluralize(
      " All ", str_to_lower(n_location_text),
      " have a location mapped at right."
    )
  } else if (n_locations > 1) {
    location_text <- pluralize(
      " ", n_location_text,
      " have a location mapped at right."
    )
  }

  if (n_citywide > 0) {
    if (n_locations > 0) {
      location_text <- pluralize(
        " ", n_what(n_locations, .f = str_to_sentence),
        "{qty(n_citywide)} {?has/have} a specific location and ",
        n_what(n_citywide, what = "", plural = ""), " {?has/have} a citywide scope."
      )
    } else {
      location_text <- pluralize(" ", n_location_text, "{qty(n_citywide)} {?has/have} a citywide scope.")
    }
  }

  if (!callout) {
    block_text <- quartools::qto_block(
      amt_text,
      type_text,
      location_text
    )

    return(block_text)
  }

  quartools::qto_callout(
    request_text,
    type_text,
    high_priority_text,
    location_text,
    type = type,
    appearance = appearance
  )
}
