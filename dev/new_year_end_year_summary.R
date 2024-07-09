new_fy2026 <- cip_bof_recommendations |>
  summarise(
    fy2025 = sum(fy2025, na.rm = TRUE),
    fy2026 = sum(fy2026, na.rm = TRUE),
    .by = project_code
  ) |>
  filter(
    is.na(fy2025) | fy2025 == 0,
    fy2026 > 0
  ) |>
  left_join(
    cip_project_details |>
      select(project_code, agency_label),
    by = "project_code"
  ) |>
  count(agency_label)

end_fy2025 <- cip_bof_recommendations |>
  summarise(
    fy2025 = sum(fy2025, na.rm = TRUE),
    fy2026 = sum(fy2026, na.rm = TRUE),
    fy2027 = sum(fy2027, na.rm = TRUE),
    .by = project_code
  ) |>
  filter(
    fy2025 > 0,
    fy2026 == 0,
    fy2027 == 0
  ) |>
  left_join(
    cip_project_details |>
      select(project_code, agency_label),
    by = "project_code"
  ) |>
  count(agency_label)
