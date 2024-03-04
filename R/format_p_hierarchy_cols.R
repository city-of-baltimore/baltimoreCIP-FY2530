## ---- format_workday_hierarchy
format_p_hierarchy_cols <- function(
    data,
    p_hierarchy_xwalks = NULL,
    ...) {
  stopifnot(
    all(has_name(data, c(
      "p_hierarchy1_code", "p_hierarchy2_code",
      "p_hierarchy1_name", "p_hierarchy2_name"
    )))
  )

  # FIXME: This is a manual correction that should be
  # removed when the project is updated in Workday
  data <- data |>
    mutate(
      ## PRJ002543 (added on 2024-01-10)
      p_hierarchy1_name = if_else(
        project_code == "PRJ002543",
        "PJH6100 Public Works",
        p_hierarchy1_name
      ),
      p_hierarchy1_code = if_else(
        project_code == "PRJ002543",
        "PJH6100 Public Works",
        p_hierarchy1_code
      ),
      p_hierarchy2_name = if_else(
        project_code == "PRJ002543",
        "PJHCIP0525 Capital Projects - Pollution and Erosion Control",
        p_hierarchy2_name
      ),
      p_hierarchy2_code = if_else(
        project_code == "PRJ002543",
        "PJHCIP0525 Capital Projects - Pollution and Erosion Control",
        p_hierarchy2_code
      ),

      ## PRJ002321 (added on 2024-01-12)
      p_hierarchy1_name = if_else(
        project_code == "PRJ002321",
        "PJH3100 Housing and Community Development",
        p_hierarchy1_name
      ),
      p_hierarchy1_code = if_else(
        project_code == "PRJ002321",
        "PJH3100 Housing and Community Development",
        p_hierarchy1_code
      ),
      p_hierarchy2_name = if_else(
        project_code == "PRJ002321",
        "PJHCIP0588 Capital Projects - Housing and Community Development",
        p_hierarchy2_name
      ),
      p_hierarchy2_code = if_else(
        project_code == "PRJ002321",
        "PJHCIP0588 Capital Projects - Housing and Community Development",
        p_hierarchy2_code
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
