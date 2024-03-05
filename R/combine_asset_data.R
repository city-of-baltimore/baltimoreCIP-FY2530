#' Combine asset data with additional data using sf_list_rbind
#'
combine_asset_data <- function(asset_data,
                               additional_data,
                               names_to = purrr::zap()) {
  sf_list_rbind(
    .list = c(
      list(asset_data),
      additional_data
    ),
    names_to = names_to
  )
}
