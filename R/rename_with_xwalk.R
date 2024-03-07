#' Rename variables using a crosswalk data frame with columns for existing
#' and updated names
#'
#' Adapted from [getdata::rename_with_xwalk()]
#'
#' @inheritParams getdata::rename_with_xwalk
rename_with_xwalk <- function(x,
                              xwalk,
                              xwalk_cols = c("new_name", "name"),
                              strict = FALSE) {
  if (is.data.frame(xwalk)) {
    xwalk <- xwalk |>
      select(all_of(xwalk_cols)) |>
      deframe() |>
      as.list()
  }

  if (!strict) {
    xwalk <- xwalk[xwalk %in% names(x)]
  }

  x <- dplyr::rename_with(
    x,
    ~ names(xwalk)[which(xwalk == .x)],
    .cols = any_of(as.character(xwalk))
  )
}
