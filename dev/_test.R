tar_source()

store_path <- here(targets::tar_config_get("store"))

cip_project_details_src <- tar_read(cip_project_details_src)

adaptive_dictionary <- tar_read(adaptive_dictionary)

report_xwalks <- tar_read(report_xwalks, store = store_path)

cip_requests <- tar_read(cip_requests, store = store_path)

cip_recommendations <- tar_read(cip_recommendations, store = store_path)

cip_comparison_summary <- tar_read(cip_comparison_summary, store = store_path)

cip_project_details <- tar_read(cip_project_details, store = store_path)

cip_locations <- tar_read(cip_locations, store = store_path)

asset_list <- tar_read(asset_list, store = store_path)

agency_reference <- tar_read(agency_reference, store = store_path)

report_data <- tar_read(cip_report_data)

project_detail_updates <- tar_read(project_detail_updates)

p_hierarchy_xwalks <- tar_read(p_hierarchy_xwalks)

project_data_corrections <- tar_read(project_data_corrections)

cip_projects <- tar_read(cip_projects)

sum(cip_recommendations$fy2025)

sum(purrr::list_rbind(report_data[["recommendation_data"]]) |> pull(fy_2025))
