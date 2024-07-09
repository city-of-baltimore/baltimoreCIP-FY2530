library(ggplot2)
library(tidyverse)

data <- tibble::tribble(
  ~start_date, ~end_date, ~report, ~name, ~icon, ~description, ~update, ~Location, ~date,
  "2023-12-08", NA, "X", "DOP Pubilished CIP with Agency Requests", "🏗️", "DOP publishes agency capital requests.", "Updated where", "N/A", NA,
  "2024-03-07", NA, "X", "Planning Commission Recommended", "🗳️", "PC votes on agency capital requests.", NA, "417 E. Fayette St., 8th Floor", NA,
  "2024-03-25", NA, "X", "Board of Finance Review", "🏦", NA, NA, "Contact Bureau of Treasury for more information", NA,
  "2024-05-15", NA, "X", "Board of Estimates Vote", "🗳️", NA, NA, "City Hall", "Approximate",
  "2024-06-15", NA, "X", "City Council Budget Vote", "🏛️", NA, NA, NA, NA
)


data |>
  arrange(
    start_date
  ) |>
  mutate(
    x = row_number(),
    y = 1,
    color = case_when(
      as_date(start_date) > today() ~ "white",
      .default = "#FCB826"
    ),
    # label = paste0(icon, name),
    xend = if_else(
      x < max(x),
      x + 1,
      x
    ),
    yend = 1
  ) |>
  ggplot(aes(x, y)) +
  geom_line(linewidth = 6) +
  geom_point(size = 9) +
  geom_segment(aes(
    xend = xend,
    yend = yend,
    color = color
  ), linewidth = 3) +
  geom_point(aes(color = color), size = 6) +
  # geom_point(
  #   shape = 3,
  #   hjust = 0,
  #   size = 6
  # ) +
  geom_text(
    aes(label = name),
    angle = 30,
    hjust = 0,
    nudge_y = 0.025
  ) +
  geom_text(
    aes(label = icon) # ,
    # family = "OpenEmoji"#,
    # fontface = "medium"
    # weight = "medium"
    # angle = 30,
    # hjust = 0,
    # nudge_y = 0.025
  ) +
  scale_y_continuous(limits = c(0.95, 1.2)) +
  scale_x_continuous(expand = c(0.1, 0.4)) +
  scale_color_identity() +
  theme_void()


plot |>
  ggsave(
    filename = "test-notoemoji-plot.png",
    device = ragg::agg_png()
  )
