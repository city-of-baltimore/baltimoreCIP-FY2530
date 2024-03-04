#' Bind CIP Comparison columns
#'
#' Used by [summarise_cip_comparison()]
#'
bind_cip_comparison_cols <- function(data) {
  data |>
    mutate(
      is_new = is.na(request),
      recommendation = replace_na(recommendation, 0),
      fy_total_diff = recommendation - request,
      fy_total_pct_diff = ifelse(
        recommendation != 0 & request != 0,
        (recommendation / request) - 1,
        NA_real_
      ),
      fy_total_pct_diff = ifelse(
        recommendation == 0 & request > 0,
        -1,
        fy_total_pct_diff
      ),
      diff_desc = case_when(
        is_new ~ "Added funding",
        fy_total_diff > 0 ~ "Increased funding",
        recommendation == 0 ~ "No funding",
        fy_total_diff < 0 ~ "Decreased funding",
        fy_total_diff == 0 ~ "No change"
      )
    )
}
