#' Format project name
#'
#' Creates new columns named project_name_src, legacy_account_number,
#' cip_number, agency_contract_id, agency_contract_id_list, and fiscal_year
#' (using [format_fiscal_year()])
#'
format_project_name <- function(data) {
  check_names(data, must.include = c("project_name", "project_code"))

  cli::cli_alert_info(
    "Formatting {.arg project_name} column"
  )

  data |>
    mutate(
      project_name_src = project_name,

      ##  ---- Legacy Account Number ----

      project_name = str_remove_trim(project_name, project_code),
      # piece of the 26 digit accounting string to identify a specific project
      legacy_account_number = str_extract_legacy_proj_num(project_name),
      project_name = if_else(
        !is.na(legacy_account_number),
        str_remove_trim(project_name, legacy_account_number),
        str_trim(project_name)
      )
    ) |>
    derive_fiscal_year_col() |>
    mutate(

      ##  ---- Legacy CIP Project Number ----
      # Extract cip_number from project names
      cip_number = str_extract_cip_number(project_name),

      # Strip legacy project number from name and description
      project_name = case_when(
        is.na(cip_number) ~ project_name,
        !is.na(cip_number) ~ str_remove_trim(project_name, cip_number),
        .default = project_name
      ),

      # Strip project code from project names
      project_name = str_remove_trim(project_name, paste0(project_code, "$")),

      ##  ---- Agency Project ID ----

      # Extract agency project ID
      agency_contract_id = str_extract_agency_contract_id(
        project_name
      ),
      agency_contract_id_list = str_extract_agency_contract_id(
        project_name,
        extract_all = TRUE
      ),

      # FIXME: Added the code below to try to strip agency project ID values
      # project_name = str_remove(
      #   project_name,
      #   agency_contract_id
      # ),
      # project_name = str_remove(
      #   project_name,
      #   "\\(\\)"
      # ),
      .before = everything()
    )
}
