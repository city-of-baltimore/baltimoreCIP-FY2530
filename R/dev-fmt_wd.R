# NOTE: These functions have not been adapted for the other formatting functions yet

#' Format a pair of code and name columns from a Workday or Adaptive Planning report
fmt_wd_code_name <- function(data,
                             code_col,
                             name_col,
                             code_pattern = NULL,
                             new_code_col = code_col,
                             new_name_col = name_col) {
  data |>
    dplyr::mutate(
      "{new_code_col}" := str_extract(.data[[code_col]], code_pattern),
      "{new_name_col}" := str_trim(str_remove(.data[[name_col]], paste0("^", .data[[new_code_col]])))
    )
}

#' Format CIP data by dropping select columns and formatting code/name column pairs
fmt_cip_data <- function(
    data,
    drop_cols = c(
      "PCode Code", "PCode Name",
      "Fund, Grant, Special Purpose Code",
      "Fund, Grant, Special Purpose Name",
      "RObject Code", "Grant_Detail Code",
      "Fund_Grant_SPurpose Code", "Fund_Grant_SPurpose Name",
      "FGSSPurpose Code", "FGSSPurpose Name",
      "FGSGrant Code", "FGSGrant Name",
      "PGroup Code", "PStatus Code", "PDescription Code",
      "PObjective Code", "PProblemStatement Code",
      "PInScope Code", "POutofScope Code",
      "PMeasuresofSuccess Code", "PProjectOverview Code"
    )) {
  data <- data |>
    select(!any_of(drop_cols)) |>
    filter(!is.na(`Project Code`)) |>
    fmt_wd_code_name(
      "FGSFund Code",
      "FGSFund Name",
      "^[:digit:]+"
    ) |>
    fmt_wd_code_name(
      "Cost Center Code",
      "Cost Center Name",
      "^(CIP|CAP|RES|zzDNU_CAP|DNU_CAP)[:digit:]+"
    ) |>
    fmt_wd_code_name(
      "Project Code",
      "Project Name",
      "^PRJ[:digit:]+",
      new_name_col = "Project Name Short"
    )

  if (has_name(data, "Revenue Category Code")) {
    # Budget data
    data <- data |>
      fmt_wd_code_name(
        "Revenue Category Code",
        "Revenue Category Name",
        "^RC[:digit:]+"
      ) |>
      fmt_wd_code_name(
        "RAccount Code",
        "RAccount Name",
        "^([:digit:]|:)+"
      ) |>
      mutate(
        `RAccount Name` = str_remove(`RAccount Name`, "^:")
      )
  } else {
    data <- data |>
      fmt_wd_code_name(
        "PHierarchy1 Code",
        "PHierarchy1 Name",
        "^(PJH|PJHCIP)[:digit:]+"
      ) |>
      fmt_wd_code_name(
        "PHierarchy2 Code",
        "PHierarchy2 Name",
        "^(PJH|PJHCIP)[:digit:]+"
      )
  }

  data
}
