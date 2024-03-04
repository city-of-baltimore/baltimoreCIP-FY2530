#' Rename variables using a crosswalk data frame with columns for existing
#' and updated names
#'
#' Adapted from [getdata::rename_with_xwalk()]
#'
#' @inheritParams getdata::rename_with_xwalk
rename_with_xwalk <- function(x,
                              xwalk,
                              xwalk_cols = c("new_name", "name")) {
  if (is.data.frame(xwalk)) {
    xwalk <- xwalk |>
      select(all_of(xwalk_cols)) |>
      # filter(!is.na(.data[[xwalk_cols[[1]]]])) |>
      deframe() |>
      as.list() #|>
    # vctrs::list_drop_empty()
  }

  x <- dplyr::rename_with(
    x,
    ~ names(xwalk)[which(xwalk == .x)],
    .cols = any_of(as.character(xwalk))
  )
}
