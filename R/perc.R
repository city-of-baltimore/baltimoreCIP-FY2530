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
