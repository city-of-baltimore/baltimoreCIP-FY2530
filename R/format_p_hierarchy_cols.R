## ---- format_workday_hierarchy
format_p_hierarchy_cols <- function(
    data,
    p_hierarchy_xwalks = NULL,
    ...) {
  check_names(
    data,
    must.include = c(
      "p_hierarchy1_code", "p_hierarchy2_code",
      "p_hierarchy1_name", "p_hierarchy2_name"
    )
  )

  data |>
    mutate(
      # Remove project names from hierarchy code columns
      p_hierarchy1_code = str_extract(p_hierarchy1_code, "^([\\S]+)"),
      p_hierarchy2_code = str_extract(p_hierarchy2_code, "^([\\S]+)"),
      # Remove project code from hierarchy name columns
      p_hierarchy1_name = str_remove_trim(p_hierarchy1_name, p_hierarchy1_code),
      p_hierarchy2_name = str_remove_trim(p_hierarchy2_name, p_hierarchy2_code)
    ) |>
    # Join project details to project hierarchy crosswalk
    left_join(
      p_hierarchy_xwalks[["p_hierarchy1_xwalk"]],
      by = join_by(p_hierarchy1_code),
      na_matches = "never"
    ) |>
    left_join(
      p_hierarchy_xwalks[["p_hierarchy2_xwalk"]],
      by = join_by(p_hierarchy2_code),
      na_matches = "never"
    ) |>
    ##  ---- Division and agency labels ----
    mutate(
      agency_label = coalesce(agency_label, division),
      agency_label = case_when(
        division == "Baltimore Development Corporation" ~ division,
        division == "Mayor's Office of Recovery Programs" ~ division,
        .default = agency_label
      ),
      division = if_else(
        division == agency_label,
        NA_character_,
        division
      )
    )
}
