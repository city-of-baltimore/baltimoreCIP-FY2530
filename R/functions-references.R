#' @keywords targets utility
path_user_data <- function(...,
                           path = c("_targets", "user", "data")) {
  here::here(
    paste0(path, collapse = .Platform$file.sep),
    ...
  )
}

#' Write CSV file to the user data reference folder
#'
#' @keywords targets
write_user_csv <- function(data,
                           file = NULL,
                           path = c("_targets", "user", "data", "reference")) {
  ext <- "csv"

  file <- path_user_data(
    fs::path_ext_set(file, ext),
    path = path
  )

  readr::write_csv(
    data,
    file = file
  )
}

#' @keywords targets
read_sheet_ext <- function(
    url = "https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing",
    sheet = NULL,
    .f = NULL,
    save = TRUE,
    path = c("_targets", "user", "data", "reference"),
    ...) {
  data <- googlesheets4::read_sheet(
    url,
    sheet = sheet
  )

  if (!is.null(.f)) {
    data <- .f(data)
  }

  if (!save) {
    return(data)
  }

  write_user_csv(
    data,
    file = sheet,
    path = path
  )
}

#' @keywords targets
read_sheets_subset <- function(
    url = "https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing",
    pattern = "_xwalk$",
    sheets = NULL,
    save = TRUE,
    path = c("_targets", "user", "data", "reference")) {
  stopifnot(
    is_string(url),
    is_string(pattern)
  )

  sheets <- sheets %||% googlesheets4::sheet_names(url)

  if (!is.null(pattern)) {
    sheets <- str_subset(sheets, pattern)
  }

  sheets_data <- map(
    sheets,
    \(sheet) {
      googlesheets4::read_sheet(
        url,
        sheet = sheet
      )
    }
  )

  if (!save) {
    return(set_names(sheets_data, sheets))
  }

  walk2(
    sheets_data,
    sheets,
    \(x, y) {
      write_user_csv(
        data = x,
        file = y,
        path = path
      )
    }
  )

  set_names(sheets_data, sheets)
}

#' @keywords adaptive
read_adaptive_dictionary <- function(
    sheet = "adaptive_dictionary",
    ...) {
  read_sheet_ext(
    sheet = sheet,
    .f = \(x) {
      x |>
        mutate(
          rename_flag = (requests_clean_names != import_clean_names)
        )
    },
    ...
  )
}

read_revenue_definitions <- function(
    sheet = "revenue_definitions",
    ...) {
  read_sheet_ext(
    sheet = sheet,
    .f = \(x) {
      x |>
        filter(
          !is.na(pos)
        ) |>
        select(
          type, description
        )
    },
    ...
  )
}

#' Read project detail updates (names, descriptions, and locations) from a reference Google Sheet
#'
read_project_detail_updates <- function(
    sheet = "project_detail_updates",
    ...) {
  project_detail_updates <- read_sheet_ext(
    sheet = sheet,
    ...
  )

  list(
    "project_name_updates" = project_detail_updates |>
      filter(
        !is.na(project_name_updated)
      ) |>
      select(
        project_code,
        project_name_updated
      ),
    "project_desc_updates" = project_detail_updates |>
      filter(
        !is.na(project_desc_updated)
      ) |>
      select(
        project_code,
        project_desc_updated
      ),
    "location_updates" = project_detail_updates |>
      filter(
        !is.na(location_updated)
      ) |>
      select(
        project_code,
        location_updated
      )
  )
}

#' Read project data corrections sheet
#'
read_project_data_corrections <- function(
    sheet = "project_data_corrections",
    ...) {
  read_sheet_ext(
    sheet = sheet,
    ...
  )
}


#' Read Workday Project Hierarchy-Baltimore City Public Entity Crosswalk
#'
#' `p_hierarchy1_xwalk` and `p_hierarchy2_xwalk` are crosswalks between the
#' responsible agency identified in the p_hierarchy1 and p_hierarchy2 columns
#' from Workday and a standard public entity name. This crosswalk is used by
#' `dop_cip_projects.R` to prep `project_details` and other objects
read_p_hierarchy_xwalks <- function(
    url = "https://airtable.com/app1lcJCwi0mpQGqZ/tbl81zsVzjBxVZePB/viwlrhbxPrDasYqzp?blocks=hide",
    cell_format = "string",
    ...) {
  # pak::pkg_install("matthewjrogers/rairtable@dev")
  # FIXME: This doesn't need to be a live link — replace with a CSV export
  url <- "https://airtable.com/app1lcJCwi0mpQGqZ/tbl81zsVzjBxVZePB/viwlrhbxPrDasYqzp?blocks=hide"

  entity_xwalk <- rairtable::list_records(url = url, cell_format = cell_format, ...)

  # Read crosswalk for Project Hierarchy 1
  p_hierarchy1_xwalk <- entity_xwalk |>
    filter(
      source == "Adaptive Planning CIP Reports - Project Hierarchy 1"
    ) |>
    select(p_hierarchy1_code = id, agency_label = entity)

  # Read crosswalk for Project Hierarchy 2
  p_hierarchy2_xwalk <- entity_xwalk |>
    filter(
      source == "Adaptive Planning CIP Reports - Project Hierarchy 2"
    ) |>
    select(p_hierarchy2_code = id, division = entity)

  list(
    "p_hierarchy1_xwalk" = p_hierarchy1_xwalk,
    "p_hierarchy2_xwalk" = p_hierarchy2_xwalk
  )
}


#' Read DGS Asset List Dictionary
#'
read_asset_list_dictionary <- function(
    filename = "AssetList-MCC-DGS-2022work_Data-Dictionary.xlsx",
    sheet = "variables") {
  print("!")
  readxl::read_xlsx(
    path_user_data("DGS", filename),
    sheet = sheet
  )
}
