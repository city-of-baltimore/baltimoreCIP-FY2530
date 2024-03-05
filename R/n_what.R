## ---- n_what ----
n_what <- function(n,
                   ...,
                   .f = NULL,
                   what = "project",
                   plural = "{?s}",
                   .envir = current_env()) {
  if (is.data.frame(n)) {
    n <- nrow(n)
  }

  n_text <- nombre::nom_card(n)

  if (!is.null(.f)) {
    n_text <- .f(n_text)
  }

  cli::pluralize(
    paste0("{n_text} {cli::qty(n)}", what, plural),
    ...,
    .envir = .envir
  )
}
