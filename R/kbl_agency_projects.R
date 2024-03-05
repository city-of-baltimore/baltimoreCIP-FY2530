#' Create a summary table of agency projects with or without requests using
#' `kableExtra::kbl`
#'
#' TODO: This should be re-implemented using the `kbl_tidy()` function based on
#' this function.
#'
kbl_agency_projects <- function(data,
                                caption = NULL,
                                show_requests = TRUE,
                                code_width = 0.78125,
                                project_name_width = 3.375,
                                amt_width = 0.8125,
                                width_unit = "in",
                                latex_gap_space = "1.5em",
                                latex_options = c(
                                  "repeat_header",
                                  "HOLD_position"
                                ),
                                groupname_col = group_vars(data),
                                summary_col = "total_amt",
                                latex_wrap_text = FALSE,
                                full_width = FALSE) {
  tbl_col_names <- c("", "Project", "Total ($K)")
  tbl_cols <- c("project_code", "project_name", summary_col)
  tbl_col_align <- "rlr"

  if (!is.null(caption)) {
    caption <- as.character(caption)
  }

  if (show_requests) {
    # Summarize agency requests overall
    overall_summary_tbl_data <- data |>
      ungroup() |>
      summarise(
        project_code = "",
        project_name = "Total — All Projects",
        "{summary_col}" := sum(.data[[summary_col]])
      )
  }

  if (!show_requests) {
    tbl_col_names <- tbl_col_names[1:2]
    tbl_col_align <- "rl"
    tbl_cols <- c("project_code", "project_name")
    agency_tbl_data <- data
  } else if (!is_empty(groupname_col)) {
    # If group is supplied
    if (!is_grouped_df(data)) {
      # Ensure data is grouped
      data <- data |>
        group_by(.data[[groupname_col]])
    }

    stopifnot(
      groupname_col %in% c("division", "asset_agency_label", "project_type")
    )

    # Create group summary
    agency_tbl_data_summary <- data |>
      summarise(
        "{summary_col}" := sum(.data[[summary_col]]),
        project_name = paste0("Total", " - ", unique(.data[[groupname_col]])),
        project_code = "",
        total_row = TRUE
      )

    agency_tbl_data <- data |>
      mutate(
        total_row = FALSE
      ) |>
      bind_rows(
        agency_tbl_data_summary
      ) |>
      group_by(.data[[groupname_col]]) |>
      arrange(
        total_row,
        asset_name,
        project_name,
        .by_group = TRUE
      ) |>
      select(-total_row)

    agency_group_size <- group_size(agency_tbl_data)
    agency_group_name <- group_keys(agency_tbl_data)[[1]]

    groupname_col_index <- set_names(
      as.integer(agency_group_size),
      as.character(agency_group_name)
    )
  } else {
    agency_tbl_data <- data |>
      arrange(
        division,
        asset_agency_label,
        # asset_type_label,
        asset_name,
        project_name
      )
  }

  request_count <- nrow(agency_tbl_data)
  agency_tbl_data <- ungroup(agency_tbl_data)

  if (show_requests) {
    agency_tbl_data <- agency_tbl_data |>
      bind_rows(
        overall_summary_tbl_data
      )
  }

  agency_tbl <- agency_tbl_data |>
    mutate(
      "{summary_col}" := vec_fmt_currency_plain(
        .data[[summary_col]],
        escape = FALSE
      )
    ) |>
    select(all_of(tbl_cols)) |>
    kableExtra::kbl(
      col.names = tbl_col_names,
      format = "latex",
      booktabs = TRUE,
      longtable = TRUE,
      centering = TRUE,
      position = "h",
      align = tbl_col_align,
      caption = caption,
      linesep = "\\addlinespace"
    ) |>
    kableExtra::column_spec(
      column = 1,
      width = paste0(code_width, width_unit),
      color = "darkerblue"
    ) |>
    kableExtra::kable_styling(
      latex_options = latex_options,
      full_width = full_width
    )

  if (!is_empty(groupname_col)) {
    agency_tbl <- agency_tbl |>
      kableExtra::pack_rows(
        index = groupname_col_index,
        # hline_before = TRUE,
        latex_gap_space = latex_gap_space,
        latex_wrap_text = latex_wrap_text
      )
  }

  if (!show_requests) {
    agency_tbl <- agency_tbl |>
      kableExtra::column_spec(
        column = 2,
        width = paste0(project_name_width + amt_width, width_unit),
      )

    return(agency_tbl)
  }

  agency_tbl <- agency_tbl |>
    # Spacing for total requests line
    kableExtra::row_spec(
      row = request_count + 1,
      bold = TRUE,
      color = "#00415F" # ,
      # extra_latex_after = "\\addlinespace"
    ) |>
    kableExtra::row_spec(
      row = request_count,
      extra_latex_after = "\\addlinespace",
      hline_after = TRUE
    )

  if (!is_empty(groupname_col)) {
    agency_tbl <- agency_tbl |>
      kableExtra::row_spec(
        row = cumsum(agency_group_size) - 1,
        hline_after = TRUE
      ) |>
      kableExtra::row_spec(
        row = cumsum(agency_group_size),
        hline_after = FALSE,
        bold = TRUE
      )
  }

  agency_tbl |>
    kableExtra::column_spec(
      column = 2,
      width = paste0(project_name_width, width_unit),
    ) |>
    kableExtra::column_spec(
      column = 3,
      width = paste0(amt_width, width_unit)
    )
}
