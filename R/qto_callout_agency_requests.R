## ---- qto_callout_agency_requests ----
#' Display summary text about agency projects with requests
#'
#' Used in _agency_project_request.qmd and _agency_report_standalone.qmd
#'
qto_callout_agency_requests <- function(data,
                                        agency_label,
                                        type = "note",
                                        appearance = "minimal",
                                        callout = TRUE) {
  # Format request amount
  total_request_amt <- vec_fmt_currency(
    sum(data[["total_request_amt"]]),
    decimals = 0
  )

  n_projects <- nrow(data)

  n_projects_text <- n_what(n_projects)

  n_types <- length(unique(data[["project_type"]]))

  n_high_priority <- sum(data[["priority_level"]] == "High")

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

  request_text <- glue(
    "{agency_label} has requested {total_request_amt} for {n_projects_text}."
  )

  type_text <- ""
  high_priority_text <- ""
  location_text <- ""

  # FIXME: Text for project types disabled as a comment as of 2024-01-02
  # if (n_types > 1) {
  #   # Create overview request text
  #   type_text <-
  #     paste0(
  #       " The table below summarizes requests across ",
  #       n_what(n_types, what = "different project type"), "."
  #     )
  # }

  # FIXME: Text for agency priorities disabled as a comment as of 2024-01-02
  #   if (n_high_priority > 1) {
  # .    # Create text about high priority projects
  #     high_priority_text <-
  #       " More than nine in ten are identified as a high priority."
  #
  #     prop_high_priority <- n_high_priority / n_projects
  #
  #     if (prop_high_priority <= 0.9) {
  #       prop_high_priority_text <- nombre::nom_ratio(
  #         prop_high_priority,
  #         denom = 10
  #       )
  #
  #       high_priority_text <- glue(
  #         " Around {prop_high_priority_text} are identified as a high priority."
  #       )
  #     }
  #   }

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
      request_text,
      type_text,
      high_priority_text,
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
