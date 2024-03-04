## ---- join_agency_reference ----
join_agency_reference <- function(data, agency_reference) {
  data |>
    left_join(
      # FIXME: This data could be kept with agency overviews instead and only
      # joined if needed
      agency_reference |>
        select(
          !overview
        ),
      relationship = "many-to-one",
      by = join_by(agency_label),
      na_matches = "never"
    )
}
