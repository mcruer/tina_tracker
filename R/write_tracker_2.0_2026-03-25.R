
#NOTES
#Still need a process for the error explained columns
#Still need to pull in the general meeting minutes
#IMPORTANT!!! Still need to make sure that blank meeting minutes don't trigger overwrite
#of previously uploaded data!!!

#Load Libraries & Helpers----
source("r/libraries.R")
source("r/helpers.R")
source("r/folder_and_file_locations.R")
source("r/generate_approval_status.R")

metadata_path <- "data/metadata_template_2026-03-26 Production.xlsx"
#output_path <- "output/"
#current_folder_path <- "B:/^TINA Package Tracker/"


#Load Data -----
pt <- pt()
monthly <- ezql_table("monthly")
monthly_all <- ezql_table("monthly_all")
frank <- frank()
events <- events()

boards_by_project_id <- pt %>%
  select(project_id, board_number)

board_names <- ezql_table("boards") %>%
  select(board_number, board_name)

pc <- ezql_table("commitments") %>%
  left_join(pt %>% select(project_id, board_number, project_name)) %>%
  nest(pc = -board_number)

monthly_worksheet_names <- metadata <- summarize_metadata(metadata_path) %>%
  pull_cell(all_info) %>%
  filter_in(tbl, "monthly") %>%
  filter_in_na(formula_location) %>%
  pull(col_name)

#Pull the Included Project IDs----

#This pulls any of the projects that were on the tracker last time, 
#(record_status == "In Progress") plus any new projects that weren't 
#previously on the tracker (is.na(record_status) & is.na(construction_status))

included_ids <- frank %>%
  left_join(monthly %>%
              select(project_id, record_status)) %>%
  filter(
    primary_capital_project,
    record_status == "In Progress"
    #| is.na(record_status) & is.na(construction_status)
  ) %>%
  select(project_id)



#Wrangle Comments -----

comments_out <- monthly_all %>%
  select(project_id, date_meeting, meeting_minutes) %>%
  group_by(project_id) %>%
  arrange(project_id, desc(date_meeting), .by_group = TRUE) %>%
  ungroup() %>%
  left_join(pt %>% select(project_id, board_number)) %>%
  relocate(board_number, .after = project_id)

meeting_minutes_heading_temp <- comments_out %>%
  distinct(board_number, date_meeting) %>%
  group_by(board_number) %>%
  arrange(board_number, desc(date_meeting)) %>%
  mutate(names = str_c("meeting_minutes_minus_", row_number())) %>%
  filter(row_number() <= 24) %>%
  ungroup()


empty_tibble <- tibble() %>%
  listful::build(str_c("meeting_minutes_minus_", 1:24),
                 add_na_column)

meeting_minutes_heading <- meeting_minutes_heading_temp %>%
  mutate(date_meeting = format(date_meeting, "%B %d, %Y") %>% as.character()) %>%
  pivot_wider(names_from = "names", values_from = "date_meeting") %>%
  bind_rows(empty_tibble) %>%
  nest(meeting_minutes_heading = -board_number)


comments_out <- comments_out %>%
  left_join(meeting_minutes_heading_temp) %>%
  select(project_id, names, meeting_minutes) %>%
  pivot_wider(names_from = "names", values_from = "meeting_minutes") %>%
  bind_rows(empty_tibble)


#Meeting Minutes General Out --------

meeting_minutes_general <- ezql_table("meeting_minutes_general")


meeting_minutes_general_out <- meeting_minutes_heading_temp %>%
  left_join(meeting_minutes_general) %>%
  quickm(names, str_replace, "meeting_minutes", "meeting_minutes_general") %>%
  select(board_number, names, meeting_minutes_general) %>%
  pivot_wider(names_from = "names", values_from = "meeting_minutes_general")

column_count <- ncol(meeting_minutes_general_out)

if (column_count<25) {
  meeting_minutes_general_out <- meeting_minutes_general_out %>%
    listful::build(str_c("meeting_minutes_general_minus_", 
                         column_count:24), add_na_column) 
}

meeting_minutes_general_out <- meeting_minutes_general_out %>%
  nest(meeting_minutes_general = - board_number)


#Build Monthly Table -------------------

funding_table <- pt %>%
  mutate(
    type = case_when(
      project_type %in% 
        c("Addition", "New Construction",
          "Purchase", "Retrofit") ~ "funding_construction",
      project_type == "Demolition" ~ "funding_demo",
      project_type == "Land Acquisition" ~ "funding_site",
      TRUE ~ "funding_other_projects")
  ) %>%
  select(parent_id, board_number, funding_total, type) %>%
  #NOTE!!! Board number is included here because otherwise when this is
  #left-joined in below, the parent IDs for joint projects match multiple
  #projects (and would get summed together instead of keeping them separate).
  group_by(parent_id, board_number, type) %>%
  quicks(funding_total, sum) %>%
  pivot_wider(names_from = "type", values_from = "funding_total") %>%
  ungroup()

#Create data to use if monthly dates are missing ----

#We use the dates in the project commitment if they are absent in the monthly
#tracker. This pulls in all the data to possibly be merged later.

monthly_names <- monthly %>% names()
pc_names <- pc %>% unnest(pc) %>% names

merge_to_monthly_if_na <- frank %>%
  select(project_id) %>%
  left_join(pc %>%
              unnest(pc)) %>%
  select(all_of (intersect(pc_names, monthly_names)))  %>%
  select(project_id, starts_with("date")) %>%
  nest(use_if_na = -project_id)


merge_target_names <- merge_to_monthly_if_na %>% 
  pull_cell(use_if_na) %>%
  names()


#Build monthly_out_interim ----

upload_alt <- upload <- df %>%
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
                  construction_status,
                  design_status),
                as.character) %>%
  unnest(upload) %>%
  left_join(last_meeting_dates, by = "board_number") %>% 
  mutate(date_meeting = coalesce(date_meeting, last_date_meeting)) %>%
  select(-last_date_meeting, -board_number)

approval_statuses <- generate_approval_status(events, included_ids)

# pc_process <- generate_approval_status(events, included_ids) %>%
#   left_join(upload_alt %>% select(-post_tender_complete, -pre_tender_complete)) %>%
#   select(project_id, 
#          pre_tender_complete,
#          pretender_package_type, 
#          pre_tender_documents_reviewed, 
#          post_tender_complete,
#          posttender_package_type,
#          post_tender_documents_reviewed, 
#   )

databased::database_it(approval_statuses)


monthly_out_interim <- included_ids %>%
  left_join (monthly) %>%
  select(-project_name, -ends_with("tender_complete")) %>%
  left_join(
    pt %>%
      select(
        project_id,
        parent_id,
        project_name,
        project_type,
        project_category,
        board_number
      )
  ) %>%
  left_join(funding_table) %>%
  left_join(comments_out) %>%
  mutate(record_status = "In Progress") %>%
  # left_join(to_merge_into_monthly) %>%
  # pipe_assign("missing_names", ~{setdiff(new_monthly_names, names(.x))}) %>%
  # listful::build(missing_names, add_na_column) %>% 
  left_join(pt %>% select(project_id, scope_cc_spaces_added, funding_cc)) %>%

  nest(original = all_of (merge_target_names)) %>%
  
  left_join(merge_to_monthly_if_na) %>%
  
  mutate(
    combined = map2(original, use_if_na, df_map, coalesce)
  ) %>%
  select(-original, -use_if_na) %>%
  unnest(combined) %>%
  # add_na_column("risk_type") %>%
  # add_na_column("risk_rating") %>%
  # add_na_column("risk_date_start") %>%
  # add_na_column("risk_cost_min") %>%
  # add_na_column("risk_cost_max") %>%
  # add_na_column("risk_delay_min") %>%
  # add_na_column("risk_delay_max") %>%
  # add_na_column("risk_mitigation") %>%
  mutate(meeting_minutes = NA_character_) %>%
  left_join(upload_alt %>%
              select(project_id, starts_with("error_explained"))
  ) %>%
  left_join(approval_statuses)

add_na_column <- function(data, col_name) {
  data %>% mutate("{col_name}" := NA)
}

existing_names <- monthly_out_interim %>%
  names()

missing_names <-  setdiff(monthly_worksheet_names, existing_names)
  
monthly_out <- monthly_out_interim %>%
  listful::build(missing_names, add_na_column) %>%
  nest(monthly = -board_number)

#Pre/Post Tender Data ------------------

# from_events <- events %>%
#   filter_in(event_type, "Tender") %>%
#   select(project_id, event_type, event_date) %>%
#   group_by(project_id, event_type) %>%
#   summarise(event_date = max(event_date), .groups = "drop") %>%
#   mutate(
#     budget_status = if_else(str_detect(event_type, "Within"), "(Within Budget)", "(Additional Funding)"),
#     event_type_clean = case_when(
#       str_detect(event_type, "Pre-Tender") ~ "pre_tender_complete",
#       str_detect(event_type, "Post-Tender") ~ "post_tender_complete"
#     )
#   ) %>%
#   filter(!is.na(event_type_clean)) %>%
#   # Keep only the most recent event per project/stage
#   # (needed because some projects have multiple pre/post tender check-ins)
#   group_by(project_id, event_type_clean) %>%
#   slice_max(event_date, n = 1) %>%
#   ungroup() %>%
#   mutate(event_date = str_c(event_date, " ", budget_status)) %>%
#   select(project_id, event_type_clean, event_date) %>%
#   pivot_wider(names_from = "event_type_clean", values_from = "event_date") %>%
#   # Bring in construction date to determine missing status
#   left_join(monthly %>% select(project_id, date_construction), by = "project_id") %>%
#   mutate(
#     # If post-tender exists but no pre-tender, pre-tender is MISSING
#     pre_tender_complete = case_when(
#       !is.na(pre_tender_complete) ~ pre_tender_complete,
#       !is.na(post_tender_complete) ~ "MISSING",
#       TRUE ~ "Not yet submitted/reviewed"
#     ),
#     # If construction date passed and no post-tender, it's MISSING; otherwise not yet submitted
#     post_tender_complete = case_when(
#       !is.na(post_tender_complete) ~ post_tender_complete,
#       date_construction < today() ~ "MISSING",
#       TRUE ~ "Not yet submitted/reviewed"
#     )
#   ) %>%
#   select(-date_construction) %>%
#   mutate(
#     pre_tender_documents_reviewed = NA_character_,
#     post_tender_documents_reviewed = NA_character_,
#     pretender_package_type = NA_character_, 
#     posttender_package_type = NA_character_
#   )
# 
# grandfathered <- monthly_out_interim %>% 
#   unnest(monthly) %>%
#   filter(date_construction < ymd("2025-07-01")) %>%
#   select(project_id) %>%
#   mutate(
#     pre_tender_complete = "Not Applicable",
#     post_tender_complete = "Not Applicable",
#     pre_tender_documents_reviewed = "Not Applicable",
#     post_tender_documents_reviewed = "Not Applicable",
#     pretender_package_type = "Not Applicable", 
#     posttender_package_type = "Not Applicable",
#          ) %>%
#   anti_join(from_events, by = "project_id")
# 
# remainder <- included_ids %>%
#   anti_join(grandfathered) %>%
#   anti_join(from_events) %>%
#   mutate(
#     pre_tender_complete = "Not yet submitted/reviewed",
#     post_tender_complete = "Not yet submitted/reviewed",
#     pre_tender_documents_reviewed = "Not yet submitted/reviewed",
#     post_tender_documents_reviewed = "Not yet submitted/reviewed",
#     pretender_package_type = "Not Yet Known", 
#     posttender_package_type = "Not Yet Known",
#   )
# 
# pc_process <- bind_rows(from_events, grandfathered) %>%
#   bind_rows(remainder) %>%
#   semi_join(included_ids, by = "project_id")





#Build Board Contacts Table -------------

board_contacts <- df_raw %>%
  #nested_mutate(all_harvested_data, temp_site_purchase_formula, as.character) %>%
  unnest (all_harvested_data) %>%
  select(board_number_name, board_contacts) %>%
  separate(board_number_name, 
           into = c("board_number", "board_name"), 
           sep = " - ") %>%
  to_number(board_number) %>%
  unnest(board_contacts) %>%
  nest(board_contacts =-board_number)


#Build Basic Project Info Table ------

basic_info_from_previous <- df_raw %>%
  #nested_mutate(all_harvested_data, temp_site_purchase_formula, as.character) %>%
  unnest(all_harvested_data) %>%
  nested_mutate(basic_project_info, c(date_opening_ceremony), as_date) %>%
  nested_mutate(basic_project_info, c(
    sign_installed,
    project_name_official
    ), 
    as.character) %>%
  unnest(basic_project_info) %>%
  select(project_id, sign_installed, date_opening_ceremony) %>%
  filter_out_na(project_id)

basic_project_info <- pt %>%
  select(
    project_id,
    board_number,
    project_name,
    project_name_official,
    location_address,
    location_city,
    location_postal_code,
    location_latitude,
    location_longitude,
    project_type,
    project_category
  ) %>%
  semi_join(included_ids) %>%
  left_join(basic_info_from_previous) %>%
  nest(basic_project_info = -board_number)

#Combine all data into a single tibble of boards to write ------

meeting_dates <- monthly %>%
  left_join(pt %>% select(project_id, board_number)) %>%
  group_by(board_number) %>%
  summarize(date_meeting = max(date_meeting))



test_data <- included_ids %>%
  left_join(pt %>% select(project_id, board_number)) %>%
  select(board_number) %>%
  distinct() %>%
  left_join (pc) %>%
  full_join(monthly_out) %>%
  full_join(basic_project_info) %>%
  left_join(meeting_minutes_heading) %>%
  left_join(board_contacts) %>%
  left_join(board_names) %>%
  #left_join(meeting_dates) %>%
  left_join(meeting_minutes_general_out) %>%
  mutate(
    meeting_minutes_general_current = "[PLEASE COMPLETE]",
    date_meeting = NA_Date_,
    ) %>%
  mutate(
     board_number_name = paste(board_number, board_name, sep = " - "),
     .before = 1
  ) %>%
  fix_null_listcols(
    monthly, 
    basic_project_info, 
    pc, 
    meeting_minutes_heading, 
    board_contacts
    )

test_data %>% filter(board_number == 44) %>%
  pull_cell(monthly)

#Initial Data Load Only ------

# test_data <- test_data %>%
#   mutate (
#     monthly = map(monthly,
#       ~.x %>%
#         mutate(
#           error_explained_23 = NA_character_,
#           error_explained_25 = NA_character_,
#           error_explained_30 = NA_character_,
#           error_explained_32 = NA_character_,
#           error_explained_34 = NA_character_,
#           error_explained_36 = NA_character_,
#         )
#       
#                     )
#   )

#Output ----


# openxlsx2::wb_load(metadata_path) %>%
#   openxlsx2::wb_save("output/test_output/example.xlsx")

selection <- boards %>%
  #filter(board_number == 44) %>%
  #filter_in(analyst, "sarosh") %>%
  pull(board_number)


test_data %>%
  filter(board_number %in% selection) %>% 
  
  mutate(output_path = paste0(output_path, 
                              board_number_name, ".xlsx"),
         metadata_path = metadata_path, 
         overwrite = TRUE
  ) %>%
  nest(df = -c(output_path, metadata_path, overwrite)) %>% 
  pwalk(xlr8_write_one, .progress = "Writing Files")


