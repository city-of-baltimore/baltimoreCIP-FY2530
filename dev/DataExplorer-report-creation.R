cip_project_details_src <- tar_read(cip_project_details_src)
adaptive_dictionary <- tar_read(adaptive_dictionary)


cip_project_details_src |>
  filter(!is.na(`PPriority Name`) | !is.na(`PDescription Name`)) |>
  # select(!ends_with("Code")) |>
  # select(
  #   contains("Estimate"),
  #   ends_with("Score"),
  # ) |>
  select(
    any_of(c("PHierarchy1 Name", report_dictionary$adaptive_export_name))
  ) |>
  select(
    !c(
      ends_with("Justification"), starts_with("PDescription"), starts_with("PProjectOverview"), starts_with("PProblemStatement"),
      starts_with("PObjective"), contains("Scope"),
      any_of(c("PRiskLevel Code", "PPriority Code", "PImportance Code"))
    )
  ) |>
  mutate(
    across(
      all_of(
        c("PRiskLevel Name", "PPriority Name", "PImportance Name")
      ),
      factor
    )
  ) |>
  rename(
    agency = `PHierarchy1 Name`
  ) |>
  # names()
  nest_by(
    agency
  ) |>
  slice_head(n = 3) |>
  pmap(
    \(agency, data, ...) {
      nm <- janitor::make_clean_names(agency)
      create_report(
        data,
        add_plot_str = FALSE,
        add_plot_correlation = FALSE,
        add_plot_prcomp = FALSE,
        report_title = glue::glue(
          "{agency}: Review of project detail, scoring, and cost estimates attributes"
        ),
        output_file = glue::glue("{nm}_report.html"),
        config = configure_report(
          global_ggtheme = hrbrthemes::theme_ipsum_pub()
        )
      )
    }
  )
