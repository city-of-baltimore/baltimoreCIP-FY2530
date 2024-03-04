format_fgs_fund_cols <- function(data) {
  format_code_name_cols(
    data,
    code_col = "fgs_fund_code",
    name_col = "fgs_fund_name",
    pattern = "^([\\S]+)",
    .f = str_extract
  )
}
