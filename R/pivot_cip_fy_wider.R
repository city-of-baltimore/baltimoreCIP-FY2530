#' Pivot CIP Request data into a wide format with [tidyr::pivot_wider]
#'
#' Pivot wider and then apply [janitor::clean_names()] to returned data frame.
#' Used by [summarise_cip_comparison()]
#'
#' @param data Data frame with columns starting with "fy"
#' @param names_from String passed to `all_of()` and to [tidyr::pivot_wider()]
pivot_cip_fy_wider <- function(data,
                                     names_from = "report") {
  data |>
    pivot_wider(
      names_from = all_of(names_from),
      values_from = starts_with("fy")
    ) |>
    janitor::clean_names()
}
