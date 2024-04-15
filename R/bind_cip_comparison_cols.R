#' Bind CIP Comparison columns
#'
#' Used by [summarise_cip_comparison()]
#'
bind_cip_comparison_cols <- function(data,
                                     col_reference = "request",
                                     col_comparison = "recommendation") {
  data |>
    filter(
      !is.na(.data[[col_reference]]) | !is.na(.data[[col_comparison]]),
      !(is.na(.data[[col_reference]]) & (.data[[col_comparison]] == 0)),
      !((.data[[col_reference]] == 0) & (.data[[col_comparison]] == 0))
    ) |>
    mutate(
      # is_new = is.na(.data[[col_reference]]) | (.data[[col_reference]] == 0),
      "{col_comparison}" := tidyr::replace_na(.data[[col_comparison]], 0),
      fy_total_diff = .data[[col_comparison]] - .data[[col_reference]],
      fy_total_pct_diff = if_else(
        .data[[col_comparison]] != 0 & .data[[col_reference]] != 0,
        (.data[[col_comparison]] / .data[[col_reference]]) - 1,
        NA_real_
      ),
      fy_total_pct_diff = if_else(
        .data[[col_comparison]] == 0 & .data[[col_reference]] > 0,
        -1,
        fy_total_pct_diff
      ),
      diff_desc = case_when(
        (is.na(.data[[col_reference]]) | (.data[[col_reference]] == 0)) & .data[[col_comparison]] > 0 ~ "Add funding",
        (is.na(.data[[col_reference]]) | (.data[[col_reference]] == 0)) & .data[[col_comparison]] < 0 ~ "Transfer funding",
        fy_total_diff > 0 ~ "Increase funding",
        .data[[col_reference]] > 0 & .data[[col_comparison]] == 0 ~ "Cut funding",
        fy_total_diff < 0 ~ "Decrease funding",
        fy_total_diff == 0 ~ "No change"
      )
    )
}
