#' Create a table with `kbl_tbl` grouped by `agency_label` to compare the
#' original project name in Workday to the project name used in the report
#'
#' @importFrom dplyr select mutate if_else group_by
kbl_project_identifier <- function(report_data,
                                   widths = paste0(c(2.5, 2.5), "in"),
                                   col_names = c("Project code", "Name (Workday)", "Name (Report)")) {
  report_data |>
    select(agency_label, project_code, project_name_src, project_name) |>
    mutate(
      project_name_src = str_remove_trim(
        project_name_src,
        paste0("^", project_code)
      ),
      project_name = if_else(
        project_name == project_name_src,
        "---",
        project_name
      )
    ) |>
    group_by(agency_label) |>
    kbl_tbl(
      rowname_col = "project_code",
      widths = widths,
      width_cols = c(2, 3),
      col_names = col_names,
      latex_options = "repeat_header",
      rowname_col_label = "Project code"
    )
}
