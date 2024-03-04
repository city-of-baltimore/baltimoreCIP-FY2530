dictionary_to_xwalk <- function(dictionary,
                                data) {
  # Rename project_requests data based on dictionary values
  dictionary |>
    filter(
      !is.na(requests_clean_names),
      import_clean_names %in% names(data),
      rename_flag
    ) |>
    select(requests_clean_names, import_clean_names) |>
    # FIXME: This is only needed because of an issue with the input data dictionary
    distinct(import_clean_names, .keep_all = TRUE) |>
    tibble::deframe()
}
