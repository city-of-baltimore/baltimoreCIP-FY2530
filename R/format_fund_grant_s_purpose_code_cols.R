format_fund_grant_s_purpose_code_cols <- function(data) {
  format_code_name_cols(
    data,
    code_col = "fund_grant_s_purpose_code",
    name_col = "fund_grant_s_purpose_name",
    pattern = "_\\[blank\\]"
  )
}
