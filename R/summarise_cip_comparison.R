#' Summarise CIP Request and Recommendation data by comparing both sources
#'
#' Uses [bind_cip_comparison_cols()] and [pivot_cip_fy_wider()]
#'
#' @param fy_cols Fiscal year column names to summarise by. Defaults to
#'   `paste0("fy", 2025:2030)`
summarise_cip_comparison <- function(
    reference_data,
    comparison_data,
    nm = c("Request", "Recommendation"),
    names_to = "report",
    fy_cols = paste0("fy", 2025:2030)) {
  input_list <- list(
    reference_data,
    comparison_data
  )

  stopifnot(
    has_length(input_list, 2),
    has_length(nm, 2)
  )

  # Combine reference and comparison data into a single data frame
  combined_data <- input_list |>
    set_names(nm) |>
    purrr::list_rbind(names_to = "report") |>
    group_by(project_code) |>
    arrange(
      rev(report),
      rev(revenue_category_sort),
      .by_group = TRUE
    ) |>
    ungroup()

  report_cols <- janitor::make_clean_names(nm)

  # Summarise comparison across all fiscal years
  fy_total_summary <- combined_data |>
    summarise_cip_fy_comparison(
      fy_col = "fy_total",
      col_reference = report_cols[[1]],
      col_comparison = report_cols[[2]]
    )

  # Summarise comparison for each fiscal year
  fy_annual_list <- map(
    set_names(fy_cols, toupper(fy_cols)),
    \(fy_col) {
      combined_data |>
        summarise_cip_fy_comparison(
          fy_col = fy_col,
          col_reference = report_cols[[1]],
          col_comparison = report_cols[[2]]
        )
    }
  )

  # Combine total and annual summaries
  fy_summary <- list_rbind(
    c(
      fy_annual_list,
      set_names(
        list(fy_total_summary),
        # FIXME: What is going on with this renaming?
        toupper(paste0(first(fy_cols), "-", last(fy_cols)))
      )
    ),
    names_to = "fy"
  )

  fy_summary
}

#' Helper function to summarise a single fiscal year column
#' Input data must have columns named "project_code" and "report"
summarise_cip_fy_comparison <- function(data,
                                     fy_col,
                                     col_reference,
                                     col_comparison) {
  data |>
    summarise(
    "{fy_col}" := sum(.data[[fy_col]]),
    # FIXME: Make the report column accessible via names_to parameter
    .by = c(project_code, report)
  ) |>
    pivot_cip_fy_wider() |>
    bind_cip_comparison_cols(
      col_reference = report_cols[[1]],
      col_comparison = report_cols[[2]]
    )
}
