
#Load Libraries & Helpers----
source("r/libraries.R")
source("r/helpers.R")

ent_folder_path <- "B:/^Project Monthly Updates"
new_archive_folder <- str_c("B:/^Project Monthly Updates/1.0 - OLD - Monthly Updates - All Boards/Archive/", today())
dir_create(new_archive_folder)
files_to_move <- file_tibble(current_folder_path)


files_to_move %>%
  filter_in(file, "\\.xlsx$") %>%
  mutate(new_path = str_c(new_archive_folder, "/", file)) %>%
  select(path, new_path) %>%
  pmap(file_copy, .progress = "Copying Files...")



metadata_path <- "data/metadata_template.xlsx"

metadata <- summarize_metadata(metadata_path)
all_info <- metadata %>% pull_cell(all_info)

pt <- pt()

df_raw <- file_tibble(new_archive_folder, file_type = "xlsx") %>%
  mutate(raw = map(path, read_excel_all, .progress = "Read in...")) %>%
  mutate(monthly = map(raw, xlr8_read, all_info = all_info, fix_dates_regex = "$^", .progress = "Extract..."))

df <- df_raw %>%
  mutate(monthly = map(monthly, 
                       ~.x %>%
                         select(-ends_with("formula"),
                                -c(pc, 
                                   board_number_name, 
                                   missing_data_count)) %>%
                         mutate(basic_project_info = map(basic_project_info,
                                                         ~.x %>%
                                                           filter_out_na(project_id)
                                                           ),
                                monthly = map(monthly,
                                                         ~.x %>%
                                                           filter_out_na(project_id)
                                ),
                         )
                       )
  ) %>%
  unnest(monthly) %>%
  mutate(upload = map2(
    monthly, basic_project_info,
    ~full_join(
      .x %>% 
        select(- c(project_type, project_category)) %>%
        mutate(monthly = TRUE),
      .y %>%
        select(-c(project_name, project_type, project_category)) %>%
        mutate(basic_info = TRUE)
      ,
      by = "project_id") %>%
      select(
        -c(
          starts_with("temp_formula"),
          starts_with("risk_"),
          starts_with("funding_"),
          starts_with("meeting_minutes_minus")
           )
        )
  )) %>%
  #add path, file, harvest date, and meeting date to upload table
  mutate(
    upload = map2(upload, file, 
                  ~.x %>%
                    mutate(
                      file = .y,
                      .before = 1
                      )
    ),
    upload = map2(upload, path, 
                  ~.x %>%
                    mutate(
                      path = .y,
                      .before = 1
                      )
    ),
    upload = map2(upload, date_meeting, 
                  ~.x %>%
                    mutate(
                      date_meeting = .y,
                      date_harvest = today(),
                      .before = 1
                      )
    )
  ) %>%
  #Add board_number
  mutate(board_number = str_remove(file, " - .*$") %>%
           as.numeric(), 
         .before = 1) %>%
  #Get rid of duplicate files. This methodology selects for the most recent
  #modified date and drops data from the earlier file.
  mutate(modified_date = map(metadata, pull_cell, mtime)) %>%
  unnest(modified_date) %>%
  group_by(board_number) %>%
  filter(modified_date == max(modified_date)) %>%
  ungroup() %>%
  select(-modified_date)

monthly_all_names <- ezql_table("monthly_all") %>%
  names()

monthly_names <- ezql_table("monthly") %>%
  names()

upload_names <- df %>%
  pull_cell(upload) %>%
  names()


dead_columns <- setdiff(
  monthly_all_names,
  upload_names
        )


upload <- df %>%
  select(upload) %>%
  mutate(upload = map(upload,
    ~.x %>%
      quickm(starts_with("date"), as_date)
                        )) %>%
  unnest(upload) %>%
  filter_out_na(date_meeting)

monthly_all <- ezql_table("monthly_all")

upload %>%
  filter_out_na(date_meeting) %>%
  select(any_of(names(monthly_all))) %>%
  bind_rows(
    monthly_all %>%
      #This just makes sure that if there has already been an upload today and
      #we're running this again, it'll only take the info from the upload
      #tibble.
      filter(date_harvest != today())
    ) %>%
  #Select the most recent harvest date for any duplicate project id/meeting 
  #date pairs.
  arrange(project_id, date_meeting, date_harvest) %>% 
  group_by(project_id, date_meeting) %>%
  slice_tail(n=1) %>%
  group_by(project_id) %>%
  add_index(col_name = "event_number") %>%
  fill(record_status, .direction = "down") %>%
  ungroup() %>%
  mutate(event_id = str_c(project_id, "|", event_number)) %>% 
  relocate(event_id) %>%
  select(-event_number) %>%
  ezql_edit("monthly_all", rosetta = ezql_rosetta(), delete_missing_rows = TRUE)

monthly_all <- ezql_table("monthly_all")

#Update monthly table
new_monthly <- monthly_all %>%
  select(all_of(monthly_names)) %>%
  arrange(project_id, date_meeting) %>%
  group_by(project_id) %>%
  slice_tail(n=1)  %>%
  ungroup()

source("r/email_if_removed.R")
email_if_removed(new_monthly, test = FALSE)

new_monthly %>%
  ezql_edit("monthly", rosetta = ezql_rosetta(), delete_missing_rows = TRUE)

source("r/update_official_name_locations.R")

tina::update_frank(update = TRUE)

