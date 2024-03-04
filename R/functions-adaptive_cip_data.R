#' Format an exported CIP data report
#'
format_adaptive_cip_data <- function(data,
                                     dictionary,
                                     revenue_category_name_xwalk) {
  stopifnot(
    all(has_name(dictionary, c("adaptive_export_name", "adaptive_import_col_type")))
  )

  # Subset dictionary to fields in file
  cip_dictionary <- filter(
    dictionary,
    adaptive_export_name %in% names(data)
  )

  cip_dictionary_numeric <- filter(
    cip_dictionary,
    adaptive_import_col_type == "numeric"
  )

  data |>
    mutate(
      request_id = row_number(),
      across(
        all_of(cip_dictionary_numeric[["adaptive_export_name"]]),
        \(x) {
          if (is.character(x)) {
            readr::parse_number(x)
          } else {
            x
          }
        }
      )
    ) |>
    janitor::clean_names("snake") |>
    filter(
      # Filter rows appearing after "Total" summary row
      !cumany(cost_center_code == "Total"),
      !is.na(project_code)
    ) |>
    format_fgs_fund_cols() |>
    format_cip_cols(
      revenue_category_name_xwalk = revenue_category_name_xwalk
    ) |>
    select(
      "project_code",
      "request_id",
      # FIXME: Document that this data doesn't include the project name
      # "project_name",
      starts_with(
        c(
          "fund_", "fgs_grant_", "fgss_purpose_",
          "r", # r object + revenue category
          "g", # grant
          "fy"
        )
      )
    )
}


#' Format funding requests or recommendations from Adaptive Planning
#'
#'
format_cip_cols <- function(data,
                            revenue_category_name_xwalk,
                            drop_no_amt = TRUE,
                            .before = NULL) {
  stopifnot(all(tibble::has_name(data, c(
    "revenue_category_code", "revenue_category_name",
    "r_account_code", "r_account_name"
  ))))

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
      list(
        fy2025 = 0,
        fy2026 = 0,
        fy2027 = 0,
        fy2028 = 0,
        fy2029 = 0,
        fy2030 = 0
      )
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
    fy2025 == 0,
    fy2026 == 0,
    fy2027 == 0,
    fy2028 == 0,
    fy2029 == 0,
    fy2030 == 0
  )

  data |>
    mutate(
      has_no_request_amt = request_id %in% data_no_amt$request_id
    ) |>
    filter(
      !has_no_request_amt
    )
}
