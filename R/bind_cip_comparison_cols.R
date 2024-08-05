#' Bind columns comparing the reference and comparisong column.
#'
#' Adds three comparison columns: fy_total_diff, fy_total_pct_diff, diff_desc.
#' Used by [summarise_cip_comparison()]
bind_cip_comparison_cols <- function(
    data,
    reference_col = "request",
    comparison_col = "recommendation") {
  data |>
    filter(
      !is.na(.data[[reference_col]]) | !is.na(.data[[comparison_col]]),
      !(is.na(.data[[reference_col]]) & (.data[[comparison_col]] == 0)),
      !((.data[[reference_col]] == 0) & (.data[[comparison_col]] == 0))
    ) |>
    mutate(
      "{comparison_col}" := tidyr::replace_na(.data[[comparison_col]], 0),
      fy_total_diff = .data[[comparison_col]] - .data[[reference_col]],
      fy_total_pct_diff = if_else(
        .data[[comparison_col]] != 0 & .data[[reference_col]] != 0,
        (.data[[comparison_col]] / .data[[reference_col]]) - 1,
        NA_real_
      ),
      fy_total_pct_diff = if_else(
        .data[[comparison_col]] == 0 & .data[[reference_col]] > 0,
        -1,
        fy_total_pct_diff
      ),
      diff_desc = case_when(
        (is.na(.data[[reference_col]]) | (.data[[reference_col]] == 0)) & .data[[comparison_col]] > 0 ~ "Add funding",
        (is.na(.data[[reference_col]]) | (.data[[reference_col]] == 0)) & .data[[comparison_col]] < 0 ~ "Transfer funding",
        fy_total_diff > 0 ~ "Increase funding",
        .data[[reference_col]] > 0 & .data[[comparison_col]] == 0 ~ "Cut funding",
        fy_total_diff < 0 ~ "Decrease funding",
        fy_total_diff == 0 ~ "No change"
      )
    )
}
