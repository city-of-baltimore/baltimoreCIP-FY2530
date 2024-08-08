#' Set column labels for a gt table
#'
#' @seealso [gt::cols_label()]
#' @keywords gt
cols_label_ext <- function(data,
                           columns = NULL,
                           labels = NULL,
                           .list = NULL) {
  if (!is.null(.list)) {
    return(cols_label(data, .list = .list))
  }

  if (is.null(labels)) {
    return(data)
  }

  if (is_named(labels)) {
    has_columns <- has_name(data[["_data"]], names(labels))
    return(cols_label(data, .list = labels[has_columns]))
  }

  if (!is.null(columns)) {
    has_columns <- has_name(data[["_data"]], columns)
    columns <- columns[has_columns]
    labels <- labels[has_columns]

    if (has_length(labels, 1)) {
      labels <- set_names(labels, columns[[1]])
    } else {
      labels <- set_names(labels, columns)
    }
  }

  cols_label(data, .list = labels)
}
