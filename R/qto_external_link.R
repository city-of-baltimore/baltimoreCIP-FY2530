#' Format external link for including in a rendered Quarto document
#'
qto_external_link <- function(src,
                              text = "Learn more at ",
                              icon = "{{< fa external-link-alt >}}",
                              use_icon = knitr::is_latex_output(),
                              call = caller_env()) {
  link_text <- src |>
    str_remove("^(http|https)://") |>
    str_remove("^www\\.")

  if (use_icon) {
    link_text <- paste0(link_text, " ", icon)
  }

  quartools::qto_link(
    src = src,
    text = paste0(text, link_text),
    call = call
  )
}
