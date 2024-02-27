#' Format asset name by stripping extraneous text
#'
#' Must include "asset_name" column
#'
#' Note that this function must be applied *after* [derive_demolished_col()] or any
#' other function that relies on the supplementary information included in the
#' asset_name column.
format_asset_name <- function(asset_data) {
  check_names(asset_data, must.include = "asset_name")

  mutate(
    asset_data,
    # Remove extra start and end year information from asset names
    asset_name = str_remove(
      asset_name,
      paste0(
        c(
          "\\(NEW (2021|2022|2013)\\)|- New 2013",
          "\\(DEMOLISHED-(2021|2020)\\)|\\(DEMOLISHED 2020\\)|\\(DEMOLISHED\\)|\\(Demolished\\)"
        ),
        collapse = "|"
      )
    ),
    # TODO: Consider adding a field for storing information on the last major
    # renovation for a building
    asset_name = str_remove(
      asset_name,
      " \\(Renovated 2021\\)"
    ),
    asset_name = str_remove(
      asset_name,
      " \\(RENOVATED 2021\\)"
    ),
    asset_name = str_remove(
      asset_name,
      " \\(RENOVATED-2021\\)"
    ),
    asset_name = str_remove(
      asset_name,
      " \\(REMOVED 2021\\)"
    ),
    asset_name = str_remove(
      asset_name,
      " \\(HCD-BDC\\)"
    ),
    asset_name = str_remove(
      asset_name,
      "\\(\\(PENDING SALE 2021\\)"
    ),
    asset_name = str_remove(
      asset_name,
      " \\(PENDING SALE 2021\\)"
    ),
    asset_name = str_remove(
      asset_name,
      " \\(DEMOLISHED 2021\\)"
    ),
    asset_name = str_remove(
      asset_name,
      " \\(MOVED OUT 2021\\)"
    )
  ) |>
    trim_squish_across()
}
