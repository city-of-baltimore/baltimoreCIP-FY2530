#' Join request program type based on request_program_name column
join_request_program_type <- function(
    data,
    program_col = "request_program_name",
    request_type_col = "request_type",
    .default = "City-owned Projects",
    ...) {
  # <https://docs.google.com/spreadsheets/d/1j4IA5HSaag98Ex2Jaz-6pD4btr37FXxisobNGjQ_eo0/edit?gid=0#gid=0>
  request_program_name_xwalk <- tibble::tribble(
    ~request_program_name, ~request_type,
    "Court Resurfacing", "City-owned Programs",
    "Alleys Reconstruction", "City-owned Programs",
    "Footway Reconstruction", "City-owned Programs",
    "Whole Block Demolition Program", "City-owned Programs",
    "INSPIRE Program", "City-owned Programs",
    "Community Parks and Playgrounds", "City-owned Programs",
    "Direct Homeownership", "Grant and Loan Programs",
    "Community Catalyst Grant Program", "Grant and Loan Programs",
    "Affordable Homeownership Program", "Grant and Loan Programs",
    "Heritage Area Grant Program", "Grant and Loan Programs",
    "Lead Hazard Reduction Program", "Grant and Loan Programs",
    "Urgent Needs Water Infrastructure", "City-owned Programs",
    "Urgent Demolition", "City-owned Programs",
    "Materials and Compliance Testing", "City-owned Programs",
    "Low-Income Mortgage Program", "Grant and Loan Programs",
    "Facade Improvement Program", "Grant and Loan Programs",
    "Highway Resurfacing", "City-owned Programs",
    "Federal Resurfacing", "City-owned Programs",
    "Sidewalk Reconstruction", "City-owned Programs",
    "Curb and Slab Repair", "City-owned Programs",
    "ADA Ramps", "City-owned Programs",
    "Innovation Fund", "Grant and Loan Programs",
    "Bridge Repairs", "City-owned Programs",
    "Affordable Housing Trust Fund", "Grant and Loan Programs",
    "Emergency Roof Repair Program", "City-owned Programs",
    "Traffic Signal Programs", "City-owned Programs"
  )

  request_program_name_xwalk <- set_names(
    request_program_name_xwalk,
    c(program_col, request_type_col)
  )

  data |>
    dplyr::left_join(
      request_program_name_xwalk,
      by = program_col,
      relationship = "many-to-one",
      na_matches = "never"
    ) |>
    tidyr::replace_na(
      set_names(
        list(
          .default
        ),
        request_type_col
      )
    )
}
