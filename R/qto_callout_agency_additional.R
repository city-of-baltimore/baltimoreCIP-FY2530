## ---- qto_callout_agency_additional ----
qto_callout_agency_additional <- function(data,
                                          agency_label,
                                          type = "note",
                                          appearance = "minimal",
                                          callout = TRUE) {
  request_num <- nrow(data)
  if (request_num > 1) {
    request_label <- paste0(request_num, " additional projects")
  } else {
    request_label <- paste0(request_num, " additional project")
  }
  # priority_count <- count(project_requests, priority_level)

  pattern <- "{agency_label} has identified {request_label} as a high priority."

  if (!callout) {
    text <- glue::glue(pattern)
    return(quartools::qto_block(text))
  }

  quartools::qto_callout(
    glue::glue(pattern),
    type = type,
    appearance = appearance
  )
}
