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
