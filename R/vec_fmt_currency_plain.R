## ---- vec_fmt_currency_plain
vec_fmt_currency_plain <- function(x,
                                   escape = knitr::is_latex_output(),
                                   scale_by = 0.001,
                                   decimals = 0,
                                   accounting = TRUE,
                                   output = "plain") {
  # output <- "auto"

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
