#' Summarise CIP Request and Recommendation data by comparing both sources
#'
#' Uses [bind_cip_comparison_cols()] and [pivot_cip_fy_wider()]
#'
#' @param fy_cols Fiscal year column names to summarise by. Defaults to
#'   `paste0("fy", 2025:2030)`
summarise_cip_comparison <- function(request_data,
                                     recommendation_data,
                                     fy_cols = paste0("fy", 2025:2030)) {
  combined_data <- list(
    "Request" = request_data,
    "Recommendation" = recommendation_data
  ) |>
    purrr::list_rbind(names_to = "report") |>
    group_by(project_code) |>
    arrange(
      # project_code,
      rev(report),
      rev(revenue_category_sort),
      .by_group = TRUE
    ) |>
    ungroup()

  fy_total_summary <- combined_data |>
    summarise(
      fy_total = sum(fy_total),
      .by = c(project_code, report)
    ) |>
    pivot_cip_fy_wider() |>
    bind_cip_comparison_cols()

  fy_annual_list <- map(
    set_names(fy_cols, toupper(fy_cols)),
    \(fy_col) {
      combined_data |>
        summarise(
          "{fy_col}" := sum(.data[[fy_col]]),
          .by = c(project_code, report)
        ) |>
        pivot_cip_fy_wider() |>
        bind_cip_comparison_cols()
    }
  )

  fy_summary <- list_rbind(
    c(
      fy_annual_list,
      set_names(
        list(fy_total_summary),
        toupper(paste0(first(fy_cols), "-", last(fy_cols)))
      )
    ),
    names_to = "fy"
  )
}
