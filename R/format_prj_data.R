## ---- format_prj_data
#' Format data exported from Adaptive Planning
#'
#' [format_prj_data()] handles the reformatting of the common column names
#' between the two data exports from Adaptive Planning.
#'
format_prj_data <- function(data,
                            .before = NULL,
                            drop_columns = c(
                              "p_code_code", "p_group_code", "p_status_code",
                              "p_description_code", "p_percent_complete_code",
                              "p_problem_statement_code", "p_objective_code",
                              "p_in_scope_code", "p_outof_scope_code",
                              "p_measuresof_success_code", "p_project_overview_code",
                              "r_object_code"
                            )) {
  if (all(has_name(data, c("project_name", "project_code")))) {
    data <- format_project_name(data)

    if (has_name(data, "p_description_name")) {
      data <- data |>
        mutate(
          cip_number = coalesce(
            cip_number,
            # Legacy project codes are also appended to end of some descriptions
            # (mainly DOT)
            str_extract_cip_number(p_description_name)
          ),
          p_description_name = case_when(
            is.na(cip_number) ~ p_description_name,
            !is.na(cip_number) ~ str_remove_trim(p_description_name, cip_number)
          )
        )
    }
  }

  ##  ---- Cost Center ----

  if (all(has_name(data, c("cost_center_code", "cost_center_name")))) {
    data <- mutate(
      data,
      cost_center_cat = str_sub(cost_center_code, 1, 3),
      cost_center_name = str_remove_trim(cost_center_name, cost_center_code),
      cost_center_name = str_remove_trim(cost_center_name, cost_center_cat),
      .before = .before %||% everything()
    )
  }

  ##  ---- Priority and Importance levels ----

  if (all(has_name(data, c(
    "p_importance_code", "p_importance_name",
    "p_priority_code", "p_priority_name"
  )))) {
    data <- mutate(
      data,
      p_importance_code = str_extract(p_priority_code, "[:digit:]+"),
      p_importance_name = str_extract(p_priority_name, "[:alpha:]+"),
      p_priority_code = str_extract(p_priority_code, "[:digit:]+"),
      p_priority_name = str_extract(p_priority_name, "[:alpha:]+"),
      .before = .before %||% everything()
    )
  }

  ##  ---- Fund and grant ----

  if (all(has_name(data, c(
    "fgs_fund_code", "fgs_fund_name",
    "fund_grant_s_purpose_code",
    "fund_grant_s_purpose_name"
  )))) {
    data <- data |>
      format_fgs_fund_cols() |>
      format_fund_grant_s_purpose_code_cols()
  }

  # drop_columns <- drop_columns[drop_columns %in% names(data)]

  select(
    data,
    !any_of(drop_columns)
  )
}
