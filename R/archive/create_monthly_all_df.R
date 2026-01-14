source("r/functions.R")

monthly_raw_df <- file_tibble(path_archived_folder, "xlsx", recursive = TRUE) %>%
  get_raw_xlsx(sheets_regex = "Monthly|form")

monthly_raw_df <- monthly_raw_df %>%
  update_raw_xlsx()

database_it(monthly_raw_df)

monthly_all <- monthly_raw_df %>%
  add_form_name_version("monthly.v1.0.0") %>%
  reform_tib() %>%
  clean_monthly() %>% 
  get_distinct_rows() %>%
  calculate_construction_status()

database_it(monthly_all)
