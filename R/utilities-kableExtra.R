check_kableExtra <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (inherits(x, "kableExtra")) {
    return(invisible(NULL))
  }

  cli::cli_abort(
    "{.arg {arg}} must be a kableExtra object",
    call = call
  )
}

#' Create a formatted table with `kableExtra::kbl` and
#' `kableExtra::kable_styling`
#'
#' @inheritParams kableExtra::kbl
#' @inheritParams kableExtra::kable_styling
kbl_tbl <- function(x,
                    format = "latex",
                    col_names = NULL,
                    align = NULL,
                    booktabs = TRUE,
                    longtable = TRUE,
                    position = "h",
                    index = NULL,
                    caption = NULL,
                    linesep = "",
                    ...,
                    rowname_col = "rowname",
                    rowname_col_label = "",
                    groupname_col = group_vars(x),
                    rownames_to_stub = FALSE,
                    latex_gap_space = "1em",
                    latex_wrap_text = TRUE,
                    extra_latex_after = NULL,
                    indent = TRUE,
                    summary_cols = NULL,
                    latex_options = "basic",
                    full_width = FALSE,
                    table_position = "center",
                    font_size = NULL,
                    row_label_position = "l",
                    repeat_header_text = "\\textit{(continued)}",
                    repeat_header_method = c("append", "replace"),
                    repeat_header_continued = FALSE,
                    stripe_color = "gray!6",
                    stripe_index = NULL,
                    latex_table_env = NULL,
                    protect_latex = TRUE,
                    widths = NULL,
                    width_cols = NULL,
                    table.envir = "table") {
  col_n <- ncol(x)

  if (tibble::has_rownames(x) && !rownames_to_stub) {
    x <- tibble::remove_rownames(x)
  }

  rowname_col_spec <- FALSE

  if (rowname_col %in% names(x)) {
    x <- tibble::remove_rownames(x)
    x <- relocate(
      x,
      all_of(rowname_col),
      .before = everything()
    )

    if (is.character(col_names)) {
      col_names[[1]] <- rowname_col_label
    }

    # FIXME: This broke the grouping process
    # x <- set_names(x, c("", names(x)[2:ncol(x)]))
    rowname_col_spec <- TRUE
  }

  if (!is_empty(groupname_col) || is_grouped_df(x)) {
    stopifnot(
      length(groupname_col) == 1
    )

    if (!is_grouped_df(x)) {
      group_x <- group_by(x, .data[[groupname_col]])
    } else {
      group_x <- x
      x <- ungroup(x)
    }

    x <- select(x, !all_of(groupname_col))
    col_n <- col_n - 1

    stopifnot(is.null(index))

    index <- set_names(
      as.integer(group_size(group_x)),
      as.character(group_keys(group_x)[[1]])
    )
  }

  tbl <- x |>
    kableExtra::kbl(
      col.names = col_names %||% NA,
      format = format,
      booktabs = booktabs,
      longtable = longtable,
      position = position,
      align = align,
      caption = caption,
      linesep = linesep,
      ...
    ) |>
    kableExtra::kable_styling(
      latex_options = latex_options,
      full_width = full_width,
      position = table_position,
      font_size = font_size,
      row_label_position = row_label_position,
      repeat_header_text = repeat_header_text,
      repeat_header_method = repeat_header_method,
      repeat_header_continued = repeat_header_continued,
      stripe_color = stripe_color,
      stripe_index = stripe_index,
      latex_table_env = latex_table_env,
      protect_latex = protect_latex,
      table.envir = table.envir
    )

  if (rowname_col_spec) {
    tbl <- tbl |>
      kableExtra::column_spec(
        column = 1,
        border_right = TRUE,
        include_thead = FALSE
      )
  }

  if (!is.null(widths) && !is.null(width_cols)) {
    widths <- vctrs::vec_recycle(
      widths,
      size = length(width_cols)
    )

    tbl <- purrr::reduce2(
      width_cols,
      widths,
      \(input, col, width) {
        kableExtra::column_spec(
          kable_input = input,
          column = col,
          width = width
        )
      },
      .init = tbl
    )
  }

  if (is.null(index)) {
    return(tbl)
  }

  tbl |>
    kableExtra::pack_rows(
      index = index,
      indent = indent,
      extra_latex_after = extra_latex_after,
      latex_gap_space = latex_gap_space,
      latex_wrap_text = latex_wrap_text
    )
}


## ---- kbl_summary ----
#'
#' Create a table with a grand summary row
#'
kbl_summary <- function(data,
                        cols = NULL,
                        .fns = \(x) {
                          sum(x, na.rm = TRUE)
                        },
                        replace_na = "---",
                        ...,
                        format = "latex",
                        hline_before = TRUE,
                        hline_after = TRUE) {
  if (!is.null(cols)) {
    sum_data <- data |>
      summarise(
        across(
          all_of(cols),
          .fns = .fns
        )
      )

    data <- bind_rows(data, sum_data)

    data <- data |>
      mutate(
        across(
          everything(),
          \(x) {
            str_replace_na(as.character(x), replacement = replace_na)
          }
        )
      )
  }

  tbl <- data |>
    kbl_tbl(format = format, ...)


  if (is.null(cols)) {
    return(tbl)
  }

  tbl |>
    kableExtra::row_spec(
      row = nrow(data) - 1,
      hline_after = hline_before
    ) |>
    kableExtra::row_spec(
      row = nrow(data),
      hline_after = hline_after
    )
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
