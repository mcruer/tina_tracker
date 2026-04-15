# =============================================================================
# TINA Project Tracker: Data Harvest Pipeline
# =============================================================================
# This script handles the data extraction side of the TINA tracker workflow.
# It reads project tracker files from school boards, extracts updated data,
# archives modified files, and uploads new records to the database.
#
# Key steps:
#   1. Compare current folder state to previous snapshot to identify changes
#   2. Alert data team if unexpected files added/removed
#   3. Archive only files that have been modified (to avoid duplicate copies)
#   4. Read and process ALL expected tracker files
#   5. Upload new/updated records to monthly_all and monthly tables
#
# Note: File regeneration and final snapshot save happen in the parent 
# read_write script that calls this one.
# =============================================================================

# Setup -----------------------------------------------------------------------
# Load libraries, helpers, and configuration

source("r/libraries.R")
source("r/helpers.R")
source("r/folder_and_file_locations.R")

data_team_emails <- ezql_table("contacts") %>%
  filter_in(role, "data") %>%
  filter_out(role, "financial") %>%
  pull(email)

pt <- pt()

# File Change Detection -------------------------------------------------------
# Compare current folder contents against the previous snapshot to identify
# which files have been modified. Only modified files get archived.

new_archive_folder <- str_c(current_folder_path, "Archive/", today())
dir_create(new_archive_folder)

expected_project_tracker_files <- databased::load_data("expected_project_tracker_files")
current_file_tibble <- file_tibble(current_folder_path, file_type = "xlsx")
comparison_file_tibble <- load_data("comparison_file_tibble")

files_to_move <- current_file_tibble %>%
  #anti_join(comparison_file_tibble) %>%
  filter(file %in% expected_project_tracker_files)

# Folder Integrity Check ------------------------------------------------------
# Alert the data team if files have been unexpectedly added or removed from
# the tracker folder. This helps catch accidental deletions or rogue files.


send_alert <- !identical(
  current_file_tibble$file,
  expected_project_tracker_files
)

files_removed <- setdiff(expected_project_tracker_files, current_file_tibble$file) %>%
  str_c(collapse = ", ")
files_removed <- if (files_removed == "") "[NOTHING HERE]" else files_removed

files_added <- setdiff(current_file_tibble$file, expected_project_tracker_files) %>%
  str_c(collapse = ", ")
files_added <- if (files_added == "") "[NOTHING HERE]" else files_added

email_body <- str_c(
  "Bad news, folks. Something's amiss.\n",
  "The following templates appear to have been removed:\n ", files_removed, "\n",
  "The following templates have been added:\n ", files_added, "\n",
  "Sorry to be bearing bad news!\n-TINA"
)

if (send_alert) {
  send(data_team_emails,
       subject = "TINA Project Tracker Folder screwed up :(",
       body = email_body)
}

# Archive Modified Files ------------------------------------------------------
# Copy only the files that have changed since last run. This preserves the
# analyst-submitted versions before we overwrite them with regenerated files.

files_to_move %>%
  mutate(new_path = str_c(new_archive_folder, "/", file)) %>%
  select(path, new_path) %>%
  pmap(file_copy, .progress = "Copying Files...")

# Read All Tracker Files ------------------------------------------------------
# Read data from ALL expected tracker files (not just modified ones) to ensure
# we have complete, up-to-date information. Filter to expected files only to
# prevent duplicates or unexpected files from entering the system.

metadata <- summarize_metadata(metadata_path)
all_info <- metadata %>% pull_cell(all_info)


df_raw <- current_file_tibble %>%
  #Only upload data from expected files
  filter(file %in% expected_project_tracker_files) %>%
  mutate(raw = map(path, read_excel_all, .progress = "Read in...")) %>%
  mutate(all_harvested_data = map(raw, xlr8_read, all_info = all_info, 
                                  fix_dates_regex = "$^", .progress = "Extract..."))

# Process and Clean Extracted Data --------------------------------------------
# Clean up the harvested data: remove formula columns, filter out junk rows
# (NA or single-character project_ids), and combine monthly + basic_project_info
# tables into a single upload-ready format.


df <- df_raw %>%
  mutate(all_harvested_data = map(
    all_harvested_data,
    ~ .x %>%
      select(-ends_with("formula")) %>%
      mutate(
        basic_project_info = map(basic_project_info, ~ .x %>%
                                   filter_out_na(project_id) %>%
                                   filter_out(project_id, "^.$")),
        monthly = map(monthly, ~ .x %>%
                        filter_out_na(project_id) %>%
                        filter_out(project_id, "^.$"))
      )
  )) %>%
  unnest(all_harvested_data) %>%
  # Create upload table by joining monthly and basic_project_info
  
  mutate(upload = map2(
    monthly,
    basic_project_info,
    ~ full_join(
      .x %>%
        select(-c(project_type, project_category)) %>%
        mutate(monthly = TRUE),
      .y %>%
        select(-c(project_name, project_type, project_category)) %>%
        mutate(basic_info = TRUE),
      by = "project_id"
    ) %>%
      select(-c(
        starts_with("temp_formula"),
        starts_with("risk_"),
        starts_with("funding_"),
        starts_with("meeting_minutes_minus")
      ))
  )) %>%
  # Add metadata columns to upload table
  mutate(
    upload = map2(upload, file, ~ .x %>% mutate(file = .y, .before = 1)),
    upload = map2(upload, path, ~ .x %>% mutate(path = .y, .before = 1)),
    upload = map2(upload, date_meeting, ~ .x %>%
                    mutate(date_meeting = .y, date_harvest = today(), .before = 1))
  ) %>%
  mutate(board_number = str_remove(file, " - .*$") %>% as.numeric(), .before = 1)

# Prepare Upload Table --------------------------------------------------------
# Filter to only files with calculated error_count (i.e., files that have 
# actually been opened/saved by analysts). Coerce column types to ensure
# consistency when combining with existing database records.

monthly_all <- ezql_table("monthly_all")
monthly_all_names <- names(monthly_all)
monthly_names <- ezql_table("monthly") %>% names()
upload_names <- df %>% pull_cell(upload) %>% names()
dead_columns <- setdiff(monthly_all_names, upload_names)


last_meeting_dates <- monthly_all %>%
  select(project_id, date_meeting) %>%
  left_join(pt %>% select(project_id, board_number)) %>%
  group_by(board_number) %>%
  slice_max(date_meeting, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(board_number, last_date_meeting = date_meeting)

if(df %>% filter_out_na(error_count) %>% nrow() == 0){
  return(invisible(NULL))
}

upload <- df %>%
  filter_out_na(error_count) %>%
  select(board_number, upload) %>%
  mutate(upload = map(upload, ~ .x %>% quickm(starts_with("date"), as_date))) %>%
  nested_mutate(upload,
                c(meeting_minutes,
                  project_name_official,
                  sign_installed,
                  starts_with("error_explained"),
                  cost_basis,
                  pretender_package_type,
                  posttender_package_type,
                  pre_tender_documents_reviewed,
                  post_tender_documents_reviewed,
                  site_status,
                  design_status),
                as.character) %>%
  unnest(upload) %>%
  left_join(last_meeting_dates, by = "board_number") %>% 
  mutate(date_meeting = coalesce(date_meeting, last_date_meeting)) %>%
  select(-last_date_meeting, -board_number)

# Update monthly_all Table ----------------------------------------------------
# Merge new upload data with existing records. For duplicate project_id + 
# date_meeting pairs, keep only the most recent harvest. Generate event_ids
# and forward-fill record_status.



upload %>%
  filter_out_na(date_meeting) %>%
  select(any_of(names(monthly_all))) %>%
  bind_rows(
    # Exclude any records already harvested today (allows re-runs)
    monthly_all %>% filter(date_harvest != today())
  ) %>%
  arrange(project_id, date_meeting, date_harvest) %>%
  group_by(project_id, date_meeting) %>%
  slice_tail(n = 1) %>%
  group_by(project_id) %>%
  add_index(col_name = "event_number") %>%
  fill(record_status, .direction = "down") %>%
  ungroup() %>%
  mutate(event_id = str_c(project_id, "|", event_number)) %>%
  relocate(event_id) %>%
  select(-event_number) %>%
  # Standardize cost_basis values
  
  mutate(cost_basis = case_when(
    cost_basis == "Class A" ~ "Based on Class A or B Cost Report",
    cost_basis == "Class A Estimate" ~ "Based on Class A or B Cost Report",
    cost_basis == "Class B" ~ "Based on Class A or B Cost Report",
    cost_basis == "Class B Estimate" ~ "Based on Class A or B Cost Report",
    cost_basis == "Class D" ~ "Based on Class C or lower Cost Report",
    cost_basis == "Class D Estimate" ~ "Based on Class C or lower Cost Report",
    cost_basis == "Par analogie aux coûts actuels des projets similaires" ~ 
      "Based on repeat design",
    cost_basis == "Par analogie avec les projets Kanata Sud et Barrhaven Sud" ~ 
      "Based on repeat design",
    cost_basis == "Based on Tender results" ~ "Based on Tender Results",
    cost_basis == "Tender Results" ~ "Based on Tender Results",
    TRUE ~ cost_basis
  )) %>%
  ezql_edit("monthly_all", rosetta = ezql_rosetta(), delete_missing_rows = FALSE)

# Update monthly Table --------------------------------------------------------
# The monthly table contains only the most recent record for each project.
# Check for removed projects and alert if any disappear unexpectedly.

monthly_all <- ezql_table("monthly_all")

new_monthly <- monthly_all %>%
  select(all_of(monthly_names)) %>%
  arrange(project_id, date_harvest) %>%
  group_by(project_id) %>%
  slice_tail(n = 1) %>%
  ungroup()

new_monthly %>%
  ezql_edit("monthly", rosetta = ezql_rosetta(), delete_missing_rows = TRUE)

#Update the general meeting minutes ------------------------

meeting_minutes_general <- df_raw %>%
  select(all_harvested_data) %>%
  unnest_keep(all_harvested_data) %>%
  mutate(board_number = str_remove(board_number_name, " - .*") %>%
           as.numeric(), 
         .before = 1) %>%
  filter_out_na(date_meeting) %>%
  select(board_number, date_meeting, 
         meeting_minutes_general = meeting_minutes_general_current) %>%
  mutate (meeting_minutes_general = str_remove(meeting_minutes_general, 
                                               "^.PLEASE COMPLETE.$"),
          key = str_c(board_number, "-", date_meeting)) %>%
  relocate(key)

meeting_minutes_general %>%
  ezql_edit("meeting_minutes_general", rosetta = ezql_rosetta())

#Send notification email if a project was flagged for removal ---------

source("r/email_if_removed.R")
email_if_removed(new_monthly, test = FALSE)

# Update Related Tables -------------------------------------------------------

source("r/update_official_name_locations.R")
tina::update_frank(update = TRUE)


