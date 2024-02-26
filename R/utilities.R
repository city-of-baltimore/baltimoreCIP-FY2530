#' Tidy values in a matched pair of name and code columns
#'
#' Use [str_remove_all()] (by default) and [str_remove_trim()] to extract a code
#' column value from a combined name-code column and then remove the code value
#' from the name column.
#'
#' @param data Input data frame
#' @param update_data Update data with columns `by_col` and `update_col`
#' @param col Existing values from this column are retained if the corresponding
#'   update_col value is `NULL`.
#' @param update_col Name of column in `update_data` to coalesce with `col`.
#' @param by_col String to pass to by in [left_join()], Default: 'project_code'
#' @param relationship Passed to [left_join()], Default: 'one-to-one'
#' @param na_matches Passed to [left_join()], Default: 'never'
#' @family adaptive
#' @returns A data frame
format_code_name_cols <- function(
    data,
    code_col,
    name_col,
    pattern,
    .f = str_remove_all) {
  check_function(.f)

  mutate(
    data,
    "{code_col}" := .f(.data[[code_col]], pattern),
    "{name_col}" := str_remove_trim(.data[[name_col]], .data[[code_col]])
  )
}

#' Left join two data frames and then coalesce values from specified columns
#'
#' Use [dplyr::left_join()] to join a dataframe to a data frame with a shared
#' join column and a column of replacement values then [coalesce()] values
#' across two columns.
#'
#' @param data Input data frame
#' @param update_data Update data with columns `by_col` and `update_col`
#' @param col Existing values from this column are retained if the corresponding
#'   update_col value is `NULL`.
#' @param update_col Name of column in `update_data` to coalesce with `col`.
#' @param by_col String to pass to by in [left_join()], Default: 'project_code'
#' @param relationship Passed to [left_join()], Default: 'one-to-one'
#' @param na_matches Passed to [left_join()], Default: 'never'
#' @returns A data frame
left_join_coalesce <- function(
    data,
    update_data,
    col,
    update_col,
    by = "project_code",
    relationship = "one-to-one",
    na_matches = "never") {
  check_names(data, must.include = c(by, col))
  check_names(update_data, must.include = c(by, update_col))

  data |>
    left_join(
      update_data,
      by = by, # join_by(by),
      relationship = relationship,
      na_matches = na_matches,
    ) |>
    mutate(
      "{col}" := coalesce(
        .data[[update_col]],
        .data[[col]]
      )
    ) |>
    select(!any_of(update_col))
}


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

#' Modify character columns using `str_squish` and `str_trim`
#'
#' [trim_squish_across()] is a convenience function for squishing and removing
#' whitespace from all character columns in a data frame.
#' @inheritParams stringr::str_trim
trim_squish_across <- function(data, side = c("both", "left", "right")) {
  mutate(
    data,
    across(
      where(is.character),
      \(x) {
        str_trim(str_squish(x), side = side)
      }
    )
  )
}

#' Extract and parse digits from a string
#'
#' [str_extract_number()] is a wrapper for `stringr::str_extract` and
#' `readr::parse_number`.
#'
#' @inheritParams stringr::str_extract
str_extract_number <- function(string, pattern = "[:digit:]+") {
  readr::parse_number(str_extract(string, pattern))
}

#' Remove part of a string matching a pattern and then trim whitespace
#'
#' [str_remove_trim()] is a wrapper for `str_remove` and
#' `str_trim`. The function is used by [format_prj_data()] to
#' remove codes from name columns.
#'
#' @inheritParams stringr::str_remove
#' @inheritParams stringr::str_trim
str_remove_trim <- function(string, pattern, side = c("both", "left", "right")) {
  str_trim(str_remove(string, pattern), side = side)
}

#' Extract attributes from character vectors
#'
#' @inheritParams stringr::str_extract
#' @rdname wd_str_extract
NULL

#' Extract legacy capital account number or CIP number
#'
#' [str_extract_legacy_proj_num()] is a helper for extracting the legacy project
#' number from a project name. It is called by [format_prj_data()]
#' @rdname wd_str_extract
str_extract_legacy_proj_num <- function(string) {
  str_trim(str_extract(
    string,
    # FIXME: Is this a project number or an account number?
    "(^[:digit:]{6})|([:digit:]{3}-[:digit:]{3})"
  ))
}

#' Extract a CIP number
#' @rdname wd_str_extract
#' @name str_extract_cip_number
str_extract_cip_number <- function(string) {
  str_extract(
    string,
    "[:digit:]{3}\\-[:digit:]{3}"
  )
}

#' Extract Project Codes (a.k.a. PRJ number)
#' @rdname wd_str_extract
#' @name str_extract_project_code
str_extract_project_code <- function(string) {
  str_extract(
    string,
    "PRJ[:digit:]+"
  )
}

#' Extract a leading Cost Center Code
#' @rdname wd_str_extract
str_extract_cost_center_code <- function(string) {
  str_extract(
    string,
    "^CCA[:digit:]+"
  )
}

#' Extract a leading Spend Category Code
#' @rdname wd_str_extract
str_extract_spend_category_code <- function(string) {
  str_extract(
    string,
    "^SC[:digit:]+"
  )
}

#' Extract an Agency Contract Identifier
#'
#' @param pattern Pattern used to extract
#' @param extract_all If `TRUE`, use [stringr::str_extract_all()] and return a
#'   list of extracted identifiers. This supports multiple identifiers per
#'   string.
str_extract_agency_contract_id <- function(
    string,
    pattern = "(TR |Tr |TR|TR-|SWC|SWC |SWC-|WC|WC-|WC |SDC|SDC |SDC-|SC |SC|SC-|ER |ER-|ER)[:digit:]+",
    extract_all = FALSE,
    ...) {
  fn <- str_extract

  if (extract_all) {
    fn <- str_extract_all
  }

  string <- fn(
    string,
    pattern,
    ...
  )

  remove_pattern <- "[:space:]|[:punct:]"

  if (!is.list(string)) {
    return(str_remove_trim(toupper(string), pattern = remove_pattern))
  }

  lapply(
    string,
    \(x) {
      str_remove_trim(toupper(x), pattern = remove_pattern)
    }
  )
}
