cip_project_details_src <- tar_read(cip_project_details_src)

cip_project_details_src |>
  dplyr::filter(
    !is.na(`PDescription Name`)
  ) |>
  dplyr::select(ends_with("Score")) |>
  skimr::skim()

cip_project_details_src |>
  dplyr::select(
    ends_with("Score")
  ) |>
  pointblank::scan_data(
    sections = "OVS"
  )
