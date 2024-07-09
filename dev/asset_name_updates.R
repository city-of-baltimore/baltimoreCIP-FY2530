url <- "https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing"
asset_name_updates <- googlesheets4::read_sheet(url, sheet = "asset_name_updates")
readr::write_csv(asset_name_updates, path_user_data("asset_name_updates.csv"))
