#' Format funding requests or recommendations from Adaptive Planning
#'
#'
format_cip_cols <- function(data,
                            revenue_category_name_xwalk,
                            drop_no_amt = TRUE,
                            fy_cols = paste0("fy", 2025:2030),
                            .before = NULL) {
  check_names(
    data,
    must.include = c(
      "revenue_category_code", "revenue_category_name",
      "r_account_code", "r_account_name", "request_id"
    )
  )

  revenue_category_name_xwalk <- format_revenue_category_name_xwalk(
    revenue_category_name_xwalk
  )

  data <- data |>
    mutate(
      revenue_category_name = str_remove_trim(
        revenue_category_name,
        revenue_category_code
      ),

      # clean up revenue account and category variables
      r_account_code = str_extract(r_account_code, "^[:digit:]+"),
      r_account_name = str_remove_trim(r_account_name, paste0(r_account_code, ":"))
    ) |>
    tidyr::replace_na(
      replace = rep_named(fy_cols, list(0))
    ) |>
    mutate(
      # FIXME: Add SO link where I found this solution
      fy_total = rowSums(across(starts_with("fy"))),
      # fy_total = sum(c_across(starts_with("fy"))),
      .after = starts_with("fy")
    ) |>
    mutate(
      grant_program = case_when(
        grant_detail_name == "FHWA" ~ "Federal Highway Administration",
        grant_detail_name == "Program Open Space" ~ "Program Open Space",
        fund_grant_special_purpose_name == "9980 FND Community Development Block Grant Capital Project Fund" ~ "Community Development Block Grant",
        .default = NA_character_
      )
    ) |>
    # Join revenue_category_name_short to project requests
    left_join(
      revenue_category_name_xwalk |>
        select(!c(r_object_name, revenue_category_name)),
      by = join_by(revenue_category_code)
    )

  if (!drop_no_amt) {
    return(data)
  }

  # Identify records with no request
  data_no_amt <- filter(
    data,
    if_all(all_of(fy_cols), \(x) {
      x == 0
    })
  )

  data |>
    mutate(
      has_no_request_amt = request_id %in% data_no_amt[["request_id"]]
    ) |>
    filter(
      !has_no_request_amt
    )
}
