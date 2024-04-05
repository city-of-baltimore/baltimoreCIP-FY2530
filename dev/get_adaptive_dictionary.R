#' Get a dictionary for the Adaptive file exports pre and post-processing from
#' the reference Google Sheet
#'
#' This script gets the data from the Google Sheet and saves it to a reference
#' CSV file in data-raw and a rds file in data.

## ---- adaptive_dictionary

adaptive_dictionary |>
  readr::write_csv(
    here(
      "data-raw",
      "files",
      "adaptive_dictionary.csv"
    )
  )

adaptive_dictionary |>
  readr::write_rds(
    here(
      "data",
      "adaptive_dictionary.rds"
    )
  )

## ---- asset_name_updates
url <- "https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing"
asset_name_updates <- googlesheets4::read_sheet(url, sheet = "asset_name_updates")
readr::write_csv(asset_name_updates, here("data-raw", "files", "asset_name_updates.csv"))

## ---- location_updates
url <- "https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing"
location_updates <- googlesheets4::read_sheet(url, sheet = "location_updates") |>
  filter(is.na(fixed))
readr::write_csv(location_updates, here("data-raw", "files", "location_updates.csv"))

## ---- project_asset_xwalk
url <- "https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing"
project_asset_xwalk <- googlesheets4::read_sheet(url, sheet = "project_asset_xwalk")
readr::write_csv(project_asset_xwalk, here("data-raw", "files", "project_asset_xwalk.csv"))

## ---- operating_budget_impact_xwalk
url <- "https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing"
operating_budget_impact_xwalk <- googlesheets4::read_sheet(url, sheet = "operating_budget_impact_xwalk")
operating_budget_impact_xwalk <- operating_budget_impact_xwalk |>
  mutate(
    impact_on_operating_budget = as.character(impact_on_operating_budget)
  )

readr::write_csv(operating_budget_impact_xwalk, here("data-raw", "files", "operating_budget_impact_xwalk.csv"))

## ---- revenue_definitions
url <- "https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing"

revenue_definitions <- googlesheets4::read_sheet(url, sheet = "revenue_definitions") |>
  filter(
    !is.na(pos)
  ) |>
  select(
    type, description
  )

readr::write_csv(revenue_definitions, here("data-raw", "files", "revenue_definitions.csv"))

## ---- exclude_projects
url <- "https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing"

exclude_projects <- googlesheets4::read_sheet(
  url,
  sheet = "project_data_corrections"
) |>
  filter(
    exclude_flag == "Y"
  ) |>
  select(
    project_code
  )

readr::write_csv(exclude_projects, here("data-raw", "files", "exclude_projects.csv"))
