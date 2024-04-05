# DOT_plot_theme <- list(
#   cols4all::scale_color_discrete_c4a_cat(
#     palette = "met.juarez"
#   ),
#   cols4all::scale_fill_discrete_c4a_cat(
#     palette = "met.juarez"
#   ),
#   maplayer::theme_legend(
#     "bottomleft"
#   ),
#   theme(
#     plot.title.position = "plot",
#     plot.title = element_text(
#       family = "Raleway",
#       face = "bold",
#       size = 16,
#       margin = margin(6, 0, 6, 0)
#     ),
#     legend.text = element_text(
#       family = "Raleway",
#       size = 12
#     ),
#     legend.title = element_text(
#       family = "Raleway",
#       size = 13
#     ),
#     plot.caption = element_text(
#       family = "Raleway"
#     )
#   )
# )
#
# FY25_dot_locations <- readr::read_rds(here::here("data/FY25_dot_locations.rds"))
#
# reconstruction_projects<- FY25_dot_locations |>
#   filter(!is.na(reconstruction_label))
#
# plot_agency_project_map(
#   data = reconstruction_projects,
#   project_layer = list(
#     geom_sf(
#       data = reconstruction_projects,
#       aes(
#         color = reconstruction_label,
#         shape = reconstruction_label
#       ),
#       size = 1.5,
#       alpha = 0.85
#     ),
#     labs(
#       title = "Planned FY25 alley/sidewalk project locations",
#       color = "Project type",
#       shape = "Project type",
#       caption = "Alley/sidewalk project locations are proposed for FY25\nbut may not be completed until a later fiscal year."
#     ),
#     DOT_plot_theme
#   )
# )
#
# additional_projects <- FY25_dot_locations |>
#   filter(is.na(reconstruction_label))
#
# plot_agency_project_map(
#   data = additional_projects,
#   project_layer = list(
#     geom_sf(
#       data = additional_projects,
#       fill = "white",
#       color = "white",
#       linewidth = 1.2,
#       alpha = 0.75
#     ),
#     geom_sf(
#       data = additional_projects,
#       aes(
#         fill = cost_center,
#         color = cost_center
#       ),
#       size = 2.5,
#       linewidth = 0.9,
#       alpha = 0.85
#     ),
#     labs(
#       title = "Planned FY25 project requests by cost center",
#       color = "Cost center",
#       fill = "Cost center"
#     ),
#     DOT_plot_theme
#   )
# )
