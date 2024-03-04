
## ---- perc
# https://gist.github.com/andrewheiss/36da0529a5dd4f7a8241fd886b34f5db
perc <- function(x,
                 textwidth = 7.5,
                 dpi = 96,
                 unit = "in",
                 scale = TRUE,
                 use_pct = knitr::is_html_output()) {
  if (scale && (x < 1)) {
    x <- x * 100
  }

  if (use_pct) {
    return(pct(x))
  }

  paste0(textwidth * (x / dpi), unit)
}

## ---- cols_width_ext
# https://github.com/rstudio/gt/issues/119
cols_width_ext <- function(data,
                           columns = NULL,
                           widths = NULL,
                           .list = NULL,
                           textwidth = 7.5,
                           dpi = 96,
                           unit = "in",
                           scale = TRUE) {
  if (is.null(.list)) {
    if (is.null(widths)) {
      return(data)
    }

    if (is_named(widths)) {
      columns <- columns %||% names(widths)
    }

    stopifnot(all(purrr::map_lgl(columns, is.character)))

    .list <- lapply(
      seq_along(widths),
      \(i) {
        new_formula(
          columns[[i]],
          perc(
            widths[[i]],
            textwidth = textwidth,
            dpi = dpi,
            unit = unit,
            scale = scale
          )
        )
      }
    )
  }

  cols_width(
    data,
    .list = .list
  )
}

## ---- cols_label_ext
cols_label_ext <- function(data,
                           columns = NULL,
                           labels = NULL,
                           .list = NULL) {
  if (is.null(.list)) {
    if (is.null(labels)) {
      return(data)
    }

    if (is_named(labels)) {
      has_columns <- has_name(data[["_data"]], names(labels))

      labels <- labels[has_columns]
    } else if (!is.null(columns)) {
      has_columns <- has_name(data[["_data"]], columns)
      columns <- columns[has_columns]
      labels <- labels[has_columns]

      if (has_length(labels, 1)) {
        labels <- set_names(labels, columns[[1]])
      } else {
        labels <- set_names(labels, columns)
      }
    }
  }

  cols_label(data, .list = labels)
}

## ---- vec_fmt_currency_plain
vec_fmt_currency_plain <- function(x,
                                   escape = knitr::is_latex_output(),
                                   scale_by = 0.001,
                                   decimals = 0,
                                   accounting = TRUE) {
  output <- "auto"

  if (knitr::is_latex_output()) {
    output <- "plain"
  }

  x <- vec_fmt_currency(
    x,
    output = output,
    scale_by = scale_by,
    decimals = decimals,
    accounting = accounting
  )

  if (escape) {
    return(escape_latex(x))
  }

  x
}

## ---- fmt_currency_plain
fmt_currency_plain <- function(data, ..., columns = everything()) {
  fmt(
    data = data,
    columns = columns,
    fns = \(x){
      vec_fmt_currency_plain(x, ...)
    }
  )
}
