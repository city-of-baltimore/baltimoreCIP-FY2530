## ---- qto_dl_prj_info ----
qto_dl_prj_info <- function(data,
                            nm = c(
                              "project_type", "priority_level",
                              "location", "related_plan",
                              "cost_center", "operating_impact"
                            ),
                            .sep = "\n",
                            .drop_na = TRUE) {
  .list <- list(
    "Project type" = as.character(data[["project_type_name_src"]]),
    # "Project type" = as.character(data[["project_type"]]),
    # "Priority level" = as.character(data[["priority_level"]]),
    "Location" = data[["location"]],
    "Related plan" = data[["related_plan"]],
    "Cost center" = data[["cost_center"]],
    "Division" = data[["division"]],
    "Operating impact" = data[["operating_impact"]]
    # "Agency use" = data[["asset_agency_label"]],
  )

  .names <- str_to_sentence(
    str_replace(nm, "_", " ")
  )

  if (("est_cost_total" %in% nm) &&
    !is.na(data[["est_cost_total"]]) &&
    data[["est_cost_total"]] > 0) {
    # nm <- c(nm, "Estimated costs ($K)")
    data[["est_cost_total"]] <- vec_fmt_currency(
      data[["est_cost_total"]],
      scale_by = 0.001,
      decimals = 0,
      accounting = TRUE
    )

    .list <- c(
      .list,
      list("Estimated costs ($K)" = data[["est_cost_total"]])
    )

    .names <- c(.names, "Estimated costs ($K)")
  }

  .list <- quartools:::list_drop_empty(.list[.names])

  if (!is_empty(.list)) {
    quartools::qto_dl(
      # Convert project type factor to character
      .list = .list,
      .sep = .sep,
      .drop_na = .drop_na
    )
  }
}
