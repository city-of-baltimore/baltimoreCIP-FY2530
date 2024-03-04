## ---- format_project_type_short
derive_project_type_short_col <- function(data) {
  check_names(data, must.include = "project_name")

  cli_alert_new_col("project_type_short", "project_name")

  mutate(
    data,
    # FIXME: This should potentially be split into multiple categories and
    # aligned with some standard values
    # TODO: This should also be moved into a function
    project_type_short = case_when(
      str_detect(
        project_name,
        "HVAC Replacement"
      ) ~ "HVAC Replacement",
      str_detect(
        project_name,
        "HVAC|Heat Pump"
      ) ~ "HVAC Repair/Upgrades",
      str_detect(
        project_name,
        "Elevator Replacement"
      ) ~ "Elevator Replacement",
      str_detect(
        project_name,
        "Elev"
      ) ~ "Elevator Repair/Upgrade",
      str_detect(
        project_name,
        "Fire Suppression|Fire Protection"
      ) ~ "Fire Protection Upgrades",
      str_detect(
        project_name,
        "Fire Alarm"
      ) ~ "Fire Alarm Improvements",
      str_detect(
        project_name,
        "Pump|Pumping"
      ) ~ "Pump Facility Upgrades",
      str_detect(
        project_name,
        "Boiler Replacement"
      ) ~ "Boiler Replacement",
      str_detect(
        project_name,
        "Waterproof|Intrusion|Basement Flooding"
      ) ~ "Waterproofing Improvements",
      str_detect(
        project_name,
        "Roof Repla"
      ) ~ "Roof Replacement",
      str_detect(
        project_name,
        "Roof"
      ) ~ "Roof Repair",
      str_detect(
        project_name,
        "Window Replace"
      ) ~ "Window Replacement",
      str_detect(
        project_name,
        "Footways"
      ) ~ "Footway Repair",
      str_detect(
        project_name,
        "Alleys"
      ) ~ "Alley Repair",
      str_detect(
        project_name,
        "Water Main|Pump|Pumping|Water Service"
      ) ~ "Water Supply Upgrades",
      str_detect(
        project_name,
        "Sewer|Drain|Siphon|Interceptor"
      ) ~ "Drainage/Sewer Upgrades",
      str_detect(
        project_name,
        "Stormwater|Storm Water|Bio Retention"
      ) ~ "Stormwater Mitigation",
      str_detect(
        project_name,
        "Traffic Signal"
      ) ~ "Traffic Signal Upgrades",
      str_detect(
        project_name,
        "Resufacing|Resurface"
      ) ~ "Resurfacing",
      str_detect(
        project_name,
        "Renovation|Bathroom|Lobby|Renovate"
      ) ~ "Building Renovation",
      str_detect(
        project_name,
        "Expansion"
      ) ~ "Building Expansion",
      str_detect(
        project_name,
        "Demolition"
      ) ~ "Building Demolition",
      str_detect(
        project_name,
        "Relocation"
      ) ~ "Relocation",
      str_detect(
        project_name,
        "Acquisition"
      ) ~ "Building Acquisition",
      str_detect(
        project_name,
        "Park Improvement"
      ) ~ "Park Improvements",
      (cost_center_name == "Recreation and Parks") &
        str_detect(
          project_name,
          "Center"
        ) ~ "Rec Center Improvements",
      str_detect(
        project_name,
        "^ADA|[:space:]ADA"
      ) ~ "ADA Upgrades",
      str_detect(
        project_name,
        "Lighting|Lights"
      ) ~ "Lighting Upgrades",
      str_detect(
        project_name,
        "Electrical|Generator|Electric|Range Conversion|Elec"
      ) ~ "Electrical Upgrades",
      str_detect(
        project_name,
        "Lead Hazard|Lead Prevention"
      ) ~ "Lead Hazard Reduction",
      str_detect(
        project_name,
        "Network Card|Unmanaged Network|Network Management|Network and|Fiber Optic"
      ) ~ "Networking",
      str_detect(
        project_name,
        "Streetscaping|Streetscape"
      ) ~ "Streetscaping Improvements",
      str_detect(
        project_name,
        "Homeowner"
      ) ~ "Homeowner Support/Incentives",
      str_detect(
        project_name,
        "Incentive|Rehab Program|Booster"
      ) ~ "Development Incentives",
      str_detect(
        project_name,
        "Windows"
      ) ~ "Window Upgrades",
      str_detect(
        project_name,
        "Concrete Apron"
      ) ~ "Site Improvements",
      str_detect(
        project_name,
        "Bike|Bicycle"
      ) ~ "Bike Facilities",
      str_detect(
        project_name,
        "Action Center|Library|Station|Envelope|Facility Renovation|Facility Improvements|Facility Building|Masonry|Stairway|Cornice|Security|Garage"
      ) ~ "Other Facility Repairs/Improvements",
      str_detect(
        project_name,
        "Staff Cost|Operating|Operation"
      ) ~ "Operating Support",
      str_detect(
        project_name,
        "Conduit"
      ) ~ "Conduit Construction/Repair"
    ),
    .after = all_of("project_name")
  )
}
