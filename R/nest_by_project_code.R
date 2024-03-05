## ---- nest_by_project_code
nest_by_project_code <- function(data) {
  if (all(unique(data[["asset_label"]]) == "Department of General Services")) {
    data <- data |>
      arrange(
        asset_agency_label,
        division,
        asset_name,
        project_name
      )
  } else if (all(unique(data[["asset_label"]]) == "Department of Public Works")) {
    data <- data |>
      arrange(
        division,
        asset_name,
        project_name
      )
  } else {
    data <- data |>
      arrange(
        division,
        asset_agency_label,
        asset_name,
        project_name
      )
  }

  data_nested <- data |>
    nest_by(
      project_code,
      .key = "project",
      .keep = TRUE
    )

  data |>
    select(project_code) |>
    left_join(
      data_nested,
      by = join_by(project_code),
      relationship = "one-to-one",
      na_matches = "never"
    )
}
