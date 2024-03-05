## ---- gt_simple
#' Create a simple gt table
#'
#' [gt_simple()] is a wrapper for [gt()] that drops a sf class and
#' optionally applies a theme.
#'
gt_simple <- function(data,
                      columns = NULL,
                      labels = NULL,
                      label_columns = NULL,
                      rowname_col = "rowname",
                      rownames_to_stub = FALSE,
                      ...,
                      use_select = TRUE,
                      widths = NULL,
                      textwidth = 7.5,
                      unit = "in",
                      gt_theme = NULL) {
  # FIXME: gt_theme_details was not working as of
  # 2023-11-07
  # gt_theme = gt_theme_details) {
  if (inherits_any(data, "sf")) {
    data <- sf::st_drop_geometry(data)
  }

  group_vars <- group_vars(data)

  if (use_select && !is.null(columns)) {
    # FIXME: Will this break if using columns to select data and data is grouped
    data <- select(data, any_of(c(columns, group_vars)))
  }

  data <- gt(data, rowname_col = rowname_col, rownames_to_stub = rownames_to_stub, ...)

  data <- cols_label_ext(
    data,
    labels = labels,
    columns = label_columns
  )

  if (!is.null(widths)) {
    width_columns <- label_columns[!(label_columns %in% group_vars)]

    if (rownames_to_stub) {
      width_columns <- c(rowname_col, width_columns)
    }

    data <- cols_width_ext(
      data,
      columns = label_columns,
      widths = widths,
      textwidth = textwidth,
      unit = unit
    )
  }

  if (is.null(gt_theme)) {
    return(data)
  }

  # FIXME: gt_theme was broken and not carried over
  # gt_theme(data)
}
