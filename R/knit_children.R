#' Knit children and optionally display results
#'
knit_children <- function(.l,
                          ...,
                          .input,
                          .params = list2(),
                          .quiet = TRUE,
                          .display = TRUE) {
  purrr::pwalk(
    .l,
    \(...) {
      child <- knitr::knit_child(
        input = .input,
        envir = rlang::env(
          ...,
          !!!.params
        ),
        quiet = .quiet
      )

      if (!.display) {
        return(child)
      }

      cat(child)
    }
  )
}
