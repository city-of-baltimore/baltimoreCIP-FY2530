#' Match a vector of project names to a corresponding program name
#'
#' @details
#' Based on this Google Sheet: https://docs.google.com/spreadsheets/d/1j4IA5HSaag98Ex2Jaz-6pD4btr37FXxisobNGjQ_eo0/edit?usp=sharing
#'
match_request_program <- function(x, ...) {
  case_when(
    str_detect(x, "Court Resurfacing") ~ "Court Resurfacing",
    str_detect(x, "Alley Co|Reconstruction Alleys|Reconstruction of Alleys|Recon of Alleys|Alley Reconstruction") ~ "Alleys Reconstruction",
    str_detect(x, "Footway Co|Reconstruction of Footways|Footways TR|Reconstruction Footways|Recon Footway|Reconstruc of Footways") ~ "Footway Reconstruction",
    str_detect(x, "Whole Block") ~ "Whole Block Demolition Program",
    str_detect(x, "Inspire") ~ "INSPIRE Program",
    str_detect(x, "Parks and Playground") ~ "Community Parks and Playgrounds",
    str_detect(x, "Direct Homeownership") ~ "Direct Homeownership",
    str_detect(x, "Community Catalyst Grants") ~ "Community Catalyst Grant Program",
    str_detect(x, "Affordable Homeownership") ~ "Affordable Homeownership Program",
    str_detect(x, "Heritage Area") ~ "Heritage Area Grant Program",
    str_detect(x, "Lead Hazard Reduction Program") ~ "Lead Hazard Reduction Program",
    str_detect(x, "Urgent Needs Water (Facilities|Infrastructure)") ~ "Urgent Needs Water Infrastructure",
    str_detect(x, "Urgent Demolition") ~ "Urgent Demolition",
    str_detect(x, "Materials and Compliance Testing|Material Testing & Compliance") ~ "Materials and Compliance Testing",
    str_detect(x, "Income Mortgage Program") ~ "Low-Income Mortgage Program",
    str_detect(x, "Facade Improvement") ~ "Facade Improvement Program",
    str_detect(x, "Resurfacing (Hwy|Highway)") ~ "Highway Resurfacing",
    str_detect(x, "Federal (Resurface|Resurfacing)") ~ "Federal Resurfacing",
    str_detect(x, "Sidewalk (Repair|Reconstruction|Construction)") ~ "Sidewalk Reconstruction",
    str_detect(x, "(Curb Repair|Slab Concrete|Concrete Slab)") ~ "Curb and Slab Repair",
    str_detect(x, "ADA (Ramps|Curb Ramp)") ~ "ADA Ramps",
    str_detect(x, "Innovation Fund") ~ "Innovation Fund",
    str_detect(x, "Annual Bridge|Urgent Needs Bridge") ~ "Bridge Repairs",
    str_detect(x, "AHTF") ~ "Affordable Housing Trust Fund",
    str_detect(x, "Emergency Roof") ~ "Emergency Roof Repair Program",
    str_detect(x, "Traffic Signal Con|Traffic Signal Recon|Traffic Signals Citywide") ~ "Traffic Signal Programs"
  )
}
