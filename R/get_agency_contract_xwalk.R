get_agency_contract_xwalk <- function(project_data) {
  agency_contract_requests <- project_data |>
    filter(!is.na(agency_contract_id)) |>
    mutate(
      agency_contract_id_list_add = str_extract_agency_contract_id(
        project_name,
        extract_all = TRUE
      )
    )

  agency_contract_requests$agency_contract_id_list <- map2(
    agency_contract_requests$agency_contract_id_list,
    agency_contract_requests$agency_contract_id_list_add,
    \(x, y) {
      unique(c(x, y))
    }
  )

  agency_contract_requests |>
    select(
      -agency_contract_id
    ) |>
    select(
      agency_label,
      agency_contract_id_list,
      project_code,
      project_name
    ) |>
    tidyr::unnest_longer(
      col = all_of("agency_contract_id_list"),
      values_to = "agency_contract_id"
    )
}
