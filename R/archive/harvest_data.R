
source(here2("R", "functions.R"))

dir.create(path_newly_archived_files)

#Copy files to archived folder
file_tibble(path, file_type = "xlsx$") %>%
  filter_out(file, "All Monthly Updates.xlsx$") %>%
  mutate (to = str_c(path_newly_archived_files, file),
          from = path) %>%
  select(to, from) %>%
  pwalk(file.copy, .progress = "text")

#Read Files From Archived Folder

raw_df <- file_tibble(path_archived_folder, recursive = TRUE, file_type = "xlsx") %>%
  filter_in(file, as.character(date_harvest)) %>%
  filter_in(file, " - Monthly Tracker.xlsx") %>%
  get_raw_xlsx(sheets_regex = "Monthly|form") %>%
  add_form_name_version("monthly.v1.0.0")

monthly <- raw_df %>% 
  reform_tib() %>%
  clean_monthly()

database_it(monthly)
monthly<- load_data("monthly")

monthly %>%
  select(-where(is.list)) %>%
  ezql_edit("monthly", rosetta = ezql_rosetta(), delete_missing_rows = TRUE)

monthly_all <- load_data("monthly_all")

monthly_all <- bind_rows(monthly_all, monthly) %>%
  select(-any_of("date_modified")) %>%
  get_distinct_rows ()

database_it(monthly_all)

