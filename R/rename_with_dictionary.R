rename_with_dictionary <- function(
    data,
    dictionary,
    existing_name_col = "import_clean_names",
    new_name_col = "requests_clean_names") {
  dictionary <- dictionary |>
    dplyr::filter(
      .data[[existing_name_col]] %in% names(data),
      !is.na(.data[[new_name_col]])
    )

  name_xwalk <- set_names(
    dictionary[[existing_name_col]],
    dictionary[[new_name_col]]
  )

  data |>
    dplyr::rename(
      all_of(name_xwalk)
    )
}
