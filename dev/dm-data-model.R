library(targets)

cip_requests <- tar_read(cip_requests_src)
cip_recommendations <- tar_read(cip_recommendations_src)
cip_project_details <- tar_read(cip_project_details_src)
revenue_category_name_xwalk <- tar_read(revenue_category_name_xwalk)

library(dm)

cip_dm <- dm(
  cip_requests,
  cip_recommendations,
  cip_project_details,
  revenue_category_name_xwalk
)

cip_dm <- cip_dm |>
  dm_add_pk(cip_project_details, `Project Code`) |>
  dm_add_fk(cip_project_details, `Project Code`, cip_recommendations, `Project Code`) |>
  dm_add_fk(cip_project_details, `Project Code`, cip_requests, `Project Code`) |>
  dm_add_fk(cip_recommendations, `Revenue Category Code`, revenue_category_name_xwalk, revenue_category_code)

dm_gui(dm = cip_dm)
