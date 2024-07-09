function(report_data) {
  purrr::map(
    report_data[["location_data"]],
  )


  report_data[["location_data"]] |>
    purrr::list_rbind() |>
    sf::st_as_sf() |>
    dplyr::filter(!is.na(asset_id))
}

page <- httr2::request(
  "https://checkccmd.org/SearchResults.aspx?ft=&fn=&sn=&z=&c=Baltimore&co=Baltimore%20City"
) |>
  httr2::req_perform()

page_list <- page |>
  httr2::resp_body_html() |>
  xml2::as_list()

page_list$html$body$form$div$div$table$tbody
