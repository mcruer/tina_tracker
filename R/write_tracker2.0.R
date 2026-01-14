
#NOTES
#Still need a process for the error explained columns
#Still need to pull in the general meeting minutes
#IMPORTANT!!! Still need to make sure that blank meeting minutes don't trigger overwirte
#of previously uploaded data!!!

#Load Libraries & Helpers----
source("r/libraries.R")
source("r/helpers.R")

#Load Data -----
pt <- pt()
monthly <- ezql_table("monthly")
monthly_all <- ezql_table("monthly_all")
frank <- frank()

boards_by_project_id <- pt %>%
  select(project_id, board_number)

board_names <- ezql_table("boards") %>%
  select(board_number, board_name)

pc <- ezql_table("commitments") %>%
  left_join(pt %>% select(project_id, board_number, project_name)) %>%
  nest(pc = -board_number)




#Pull the Included Project IDs----

#This pulls any of the projects that were on the tracker last time, 
#(record_status == "In Progress") plus any new projects that weren't 
#previously on the tracker (is.na(record_status) & is.na(construction_status))

included_ids <- frank %>%
  left_join(monthly %>%
              select(project_id, record_status)) %>%
  filter(
    primary_capital_project,
    record_status == "In Progress" |
      is.na(record_status) & is.na(construction_status)
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


#Build Monthly Table -------------------


# new_monthly_names <- metadata %>%
#   pull_cell(all_info) %>%
#   filter_in(tbl, "monthly", na.rm=TRUE) %>%
#   pull (col_name)

# frank <- ezql_table("frankenstein")
# 
# cancelled_projects <- frank %>% 
#   filter_in(construction_status, "cancelled", na.rm = TRUE) %>%
#   select(project_id)
# 

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
  #NOTE!!! Board number is included here becauase otherwise when this is
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

monthly_out_interim <- included_ids %>%
  left_join (monthly) %>%
  select(-project_name) %>%
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
  add_na_column("risk_type") %>%
  add_na_column("risk_rating") %>%
  add_na_column("risk_date_start") %>%
  add_na_column("risk_cost_min") %>%
  add_na_column("risk_cost_max") %>%
  add_na_column("risk_delay_min") %>%
  add_na_column("risk_delay_max") %>%
  add_na_column("risk_mitigation") %>%
  mutate(meeting_minutes = NA_character_) %>%
  nest(monthly = -board_number)

#Pre/Post Tender Data ------------------

from_events <- events %>%
  filter_in(event_type, "Tender") %>%
  select(project_id, event_type, event_date) %>%
  group_by(project_id, event_type) %>%
  summarise(event_date = max(event_date)) %>%
  ungroup() %>%
  mutate(event_date = if_else(
    str_detect(event_type, "Within"), 
    str_c(event_date, " (Within Budget)"),
    str_c(event_date, " (Additional Funding)")),
    ) %>%
  to_character(event_date) %>%
  pivot_wider(names_from = "event_type", values_from = "event_date") %>% 
  quickm(matches("Budget"), replace_na, "MISSING") %>%
  clean_names() %>%
  rename(
    pre_tender_complete = pre_tender_budget,
    post_tender_complete = post_tender_budget,
  ) %>%
  mutate(
    pre_tender_documents_reviewed = NA_character_,
    post_tender_documents_reviewed = NA_character_,
    pretender_package_type = NA_character_, 
    posttender_package_type = NA_character_,
    
  ) %>%
  view()

grandfathered <- monthly_out_interim %>% 
  unnest(monthly) %>%
  filter(date_construction < ymd("2025-07-01")) %>%
  select(project_id) %>%
  mutate(
    pre_tender_complete = "Not Applicable",
    post_tender_complete = "Not Applicable",
    pre_tender_documents_reviewed = "Not Applicable",
    post_tender_documents_reviewed = "Not Applicable",
    pretender_package_type = "Not Applicable", 
    posttender_package_type = "Not Applicable",
         ) %>%
  anti_join(from_events, by = "project_id")

remainder <- included_ids %>%
  anti_join(grandfathered) %>%
  anti_join(from_events) %>%
  mutate(
    pre_tender_complete = "Not yet submitted/reviewed",
    post_tender_complete = "Not yet submitted/reviewed",
    pre_tender_documents_reviewed = "Not yet submitted/reviewed",
    post_tender_documents_reviewed = "Not yet submitted/reviewed",
    pretender_package_type = "Not Yet Known", 
    posttender_package_type = "Not Yet Known",
  )



pc_process <- bind_rows(from_events, grandfathered) %>%
  bind_rows(remainder) %>%
  semi_join(included_ids, by = "project_id") %>%
  
  #BAD - this doesn't do anything!
  mutate(pretender_package_type) %>%
  view()

monthly_out <- monthly_out_interim %>%
  unnest(monthly) %>%
  left_join(pc_process) %>%
  nest(monthly = -board_number) %>%
  view()

#Build Board Contacts Table -------------

board_contacts <- df_raw %>%
  nested_mutate(monthly, temp_site_purchase_formula, as.character) %>%
  unnest (monthly) %>%
  select(board_number_name, board_contacts) %>%
  separate(board_number_name, 
           into = c("board_number", "board_name"), 
           sep = " - ") %>%
  to_number(board_number) %>%
  unnest(board_contacts) %>%
  nest(board_contacts =-board_number)


#Build Basic Project Info Table ------

basic_info_from_previous <- df_raw %>%
  nested_mutate(monthly, temp_site_purchase_formula, as.character) %>%
  unnest(monthly) %>%
  nested_mutate(basic_project_info, date_opening_ceremony, as_date) %>%
  unnest(basic_project_info) %>%
  select(project_id, sign_installed, date_opening_ceremony, 
         basic_project_info_comments) %>%
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

make_na_row <- function(tbl) {
  tbl %>%
    slice(1) %>%
    mutate(across(everything(), ~ NA))
}

fix_null_listcols <- function(data, ...) {
  cols <- enquos(...)
  
  reduce(
    cols,
    .init = data,
    .f = function(df, colq) {
      col <- quo_name(colq)
      
      # find template
      template <- df[[col]] |>
        discard(is.null) |>
        pluck(1)
      
      na_template <- make_na_row(template)
      
      df %>%
        mutate({{ colq }} := map({{ colq }}, ~ if (is.null(.x))
          na_template
          else
            .x))
    }
  )
}

blank_general_meeting_minutes <- tibble() %>%
  listful::build(str_c("meeting_minutes_general_minus_", 1:24), add_na_column) %>%
  add_row()

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
  left_join(meeting_dates) %>%
  mutate(
    meeting_minutes_general_current = "[PLEASE COMPLETE]",
    meeting_minutes_general = list(blank_general_meeting_minutes)
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


#Initial Data Load Only ------

test_data <- test_data %>%
  mutate (
    monthly = map(monthly,
      ~.x %>%
        mutate(
          error_explained_23 = NA_character_,
          error_explained_25 = NA_character_,
          error_explained_30 = NA_character_,
          error_explained_32 = NA_character_,
          error_explained_34 = NA_character_,
          error_explained_36 = NA_character_,
        )
      
                    )
  )

#Output ----

output_path <- "output/test_output/" #"B:/^Project Monthly Updates/"
metadata_path <- "data/metadata_template_2026-01-09 Production.xlsx"
metadata <- summarize_metadata(metadata_path)

# openxlsx2::wb_load(metadata_path) %>%
#   openxlsx2::wb_save("output/test_output/example.xlsx")

selection <- boards %>%
  filter_in(analyst, "sarosh") %>%
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


