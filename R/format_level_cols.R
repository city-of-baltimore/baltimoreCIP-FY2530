format_level_cols <- function(data) {
  data |>
    mutate(
      across(
        ends_with("_level"),
        \(x) {
          case_match(
            x,
            "Low" ~ "temperature-empty",
            "Medium" ~ "temperature-half",
            "High" ~ "temperature-full",
            .default = NA_character_
          )
        },
        .names = "{.col}_icon"
      ),
      across(
        ends_with("_level"),
        \(x) {
          case_match(
            x,
            "Low" ~ "#ffae42",
            "Medium" ~ "#C16512",
            "High" ~ "#dd4f00",
            .default = NA_character_
          )
        },
        .names = "{.col}_icon_fill"
      )
    )
}
