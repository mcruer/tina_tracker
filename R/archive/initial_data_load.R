source("r/libraries.R")
#devtools::load_all("C:/Users/McRuersG/OneDrive - Government of Ontario/Documents/R Projects/Packages/xlr8")

monthly <- ezql_table("monthly")

monthly_all <- load_data("monthly_all")


add_na_column <- function(df, column_name) {
  df %>%
    mutate(!!rlang::sym(column_name) := NA_character_)
}
# Number of years the project has to proceed past the completion date for 
#the project to be removed.
date_completion_cut_off <- years(2) 

metadata_path <- "data/metadata_template.xlsx"
metadata <- summarize_metadata(metadata_path)


pt <- ezql_table("projects")
boards <- pt %>%
  select(project_id, board_number)

board_names <- ezql_table("boards") %>%
  select(board_number, board_name)


#Load PC Data ------

pc <- ezql_table("commitments") %>%
  left_join(pt %>% select(project_id, board_number, project_name)) %>%
  nest(pc = -board_number)
  

# 
# pc %>%
#   filter_in_na(board_number) 
# 
# appendix_a_data_path <- "B:/^Project Commitment/Batch 3 - Other/Data Extraction/AppendixAIncoming.RData"
# 
# load(appendix_a_data_path)
# 
# 
# full_app_a<- full_appA %>% 
#   rename_with(str_remove, .cols = everything(), "_incoming$") %>% 
#   select(-file, project_name, parent_id, location_address, confirm_complete)
# 
# 
# 
# path <- "B:/^Project Commitment/Batch 3 - Other/Data Extraction/Metadata Tags"
# 
# form_metadata <- file_tibble(path) %>%
#   mutate(all_info = map(path, ~summarize_metadata(.x) %>% pull_cell(all_info))) %>%
#   mutate(form = map_chr(all_info, ~.x %>% pull_cell (form))) %>%
#   select(form, all_info) %>%
#   mutate(appendix = str_remove(form, ".*_Ap_")%>% 
#            str_remove("_v1.0"), 
#          .before = 1)
# 
# 
# commitments_path <- "C:/Users/McRuersG/Music/Batch 3"
# 
# commitments <- xlr8_read_folder(commitments_path, recursive = TRUE, extract = FALSE) %>%
#   mutate(appendix = str_extract(file, "/Ap . ") %>%
#            str_remove("/Ap ") %>% str_trim(), 
#          project_id = str_remove(file, " - .*$"),
#          .before = 1) %>%
#   filter_out_na(appendix) %>%
#   group_by(appendix, project_id) %>%
#   filter(n() == 1) %>%
#   ungroup()
# 
# 
# 
# 
# save(commitments, file = "data/commitments Data.rda")
# 
# data <- commitments %>%
#   left_join(form_metadata) %>%
#   mutate(data = map2(raw_df, all_info, xlr8_read, fix_dates_regex = NULL, .progress = "text"))
# 
# 
# appendix_a <-  data %>%
#   filter_in(appendix, "A") %>%
#   select(project_id, data) %>% 
#   slice(-c(70:82)) %>%
#   #filter_in(project_id, "^40-") %>%
#   unnest(data) %>% 
#   select(-ends_with("outgoing")) %>%
#   rename_with(~str_remove(.x, "_incoming")) %>%
#   select(project_id, starts_with("date")) %>%
#   left_join(pt %>% select(project_id, project_name)) %>%
#   left_join(boards) %>%
#   filter_out_na(board_number) %>%
#   nest(pc = -board_number)
# 

monthly_names <- monthly %>% names()

pc_names <- pc %>% unnest(pc) %>% names

merge_to_monthly_if_na <- pc %>%
  unnest(pc) %>%
  select(
  all_of (intersect(pc_names, monthly_names))
  ) %>%
  rename_with(~str_c(.x, "_pc"), .cols = -project_id)


to_merge_into_monthly <- pc %>%
  unnest(pc) %>%
  select(project_id, all_of(setdiff(pc_names, monthly_names))) %>%
  select(-board_number)

#Wrangle Comments -----

comments_out <- monthly_all %>%
  select(project_id, date_meeting, matches("comments")) %>%
  rowwise() %>%
  mutate(
    design_comments = str_c(" DESIGN: ", design_comments),
    site_comments = str_c(" SITE: ", site_comments),
    cost_comments = str_c(" COST: ", cost_comments),
    comments = str_c(" GENERAL: ", comments),
  ) %>%
  ungroup() %>%
  gplyr::quickm(matches("comments"), replace_na, "") %>%
  rowwise() %>%
  mutate(
    comments_out = str_c(design_comments, site_comments, cost_comments, comments) %>%
      str_trim()
  ) %>%
  ungroup() %>%
  select(project_id, date_meeting, comments_out) %>% 
  arrange(project_id, desc(date_meeting)) %>%
  left_join(pt %>% select(project_id, board_number)) %>%
  relocate(board_number, .after = project_id) %>%
  quickm(comments_out, replace_with_na, "^$")


meeting_minutes_heading_temp <- comments_out %>%
  distinct(board_number, date_meeting) %>%
  group_by(board_number) %>%
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
  select(project_id, names, comments_out) %>%
  pivot_wider(names_from = "names", values_from = "comments_out") %>%
  bind_rows(empty_tibble)



#Build Monthly Table -------------------

monthly <- ezql_table("monthly")

new_monthly_names <- metadata %>%
  pull_cell(all_info) %>%
  filter_in(tbl, "monthly", na.rm=TRUE) %>%
  pull (col_name)

frank <- ezql_table("frankenstein")

cancelled_projects <- frank %>% 
  filter_in(construction_status, "cancelled", na.rm = TRUE) %>%
  select(project_id)
  

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
  group_by(parent_id, board_number, type) %>%
  quicks(funding_total, sum) %>%
  pivot_wider(names_from = "type", values_from = "funding_total")


monthly_out <- monthly %>%
  anti_join(cancelled_projects) %>%
  filter(date_completion > today() %m-% date_completion_cut_off) %>% 
  pipe_assign("included_ids", ~.x %>% select(project_id)) %>%
  select(-project_name) %>%
  left_join(pt %>% select(project_id, 
                          parent_id,
                          project_name,
                          project_type,
                          project_category,
                          board_number
  )) %>%
  left_join(funding_table) %>%
  left_join(comments_out) %>%
  mutate(record_status = "In Progress") %>%
  left_join(to_merge_into_monthly) %>%
  pipe_assign("missing_names", ~{setdiff(new_monthly_names, names(.x))}) %>%
  listful::build(missing_names, add_na_column) %>% 
  select(-funding_cc) %>%
  left_join(pt %>% select(project_id, funding_cc)) %>%
  left_join(merge_to_monthly_if_na) %>%
  merge_if_na(date_site_purchase, date_site_purchase_pc, drop_col = TRUE) %>%
  merge_if_na(date_construction, date_construction_pc, drop_col = TRUE) %>%
  merge_if_na(date_substantial_completion, date_substantial_completion_pc, drop_col = TRUE) %>%
  merge_if_na(date_opening, date_opening_pc, drop_col = TRUE) %>%
  merge_if_na(date_completion, date_completion_pc, drop_col = TRUE) %>%
  nest(monthly = -board_number)



#Build Board Contacts Table -------------

board_contacts <- pt %>%
  
  distinct(board_number, board_name) %>%
  filter_out_na(board_name) %>%
  listful::build(c("name",
                   "role",
                   "title",
                   "email",
                   "phone"),
                 add_na_column) %>%
  nest(board_contacts =-board_number)



#Build Basic Project Info Table ------

basic_project_info <- pt %>%
  select(
    project_id,
    project_name,
    location_address,
    location_city,
    location_postal_code,
    location_latitude,
    location_longitude,
    project_type,
    project_category
  ) %>%
  semi_join(included_ids) %>%
  listful::build(
    c(
      "project_name_official",
      "sign_installed",
      "date_opening_ceremony",
      "basic_project_info_comments"
    ),
    add_na_column
  ) %>%
  left_join(boards) %>%
  nest(basic_project_info = -board_number)


#Combine all data into a single tibble of boards to write ------

test_data <- pc %>%
  full_join(monthly_out) %>%
  full_join(basic_project_info) %>%
  left_join(meeting_minutes_heading) %>%
  left_join(board_contacts) %>%
  left_join(board_names) %>%
  mutate(date_meeting = NA_Date_,
         board_number_name = paste(board_number, board_name, sep = " - "),
         .before = 1
         ) %>%
  peek

#test_data_old <- test_data
suplementary_data <- anti_join(test_data, test_data_old, by = "board_number_name") %>%
  bind_rows(test_data %>% slice(1)) %>%
  unnest_keep(pc, names_sep = "_") %>%
  nest(pc = starts_with("pc_")) %>%
  mutate(pc = map(pc, ~.x %>% rename_with(~str_remove(.x, "^pc_")))) %>%
  filter(board_number !=2) %>%
  unnest_keep(monthly, names_sep = "_") %>%
  filter_out(monthly_project_type, "Demolition|Land Acquisition") %>%
  nest(monthly = starts_with("monthly_")) %>%
  mutate(monthly = map(monthly, ~.x %>% rename_with(~str_remove(.x, "^monthly_"))))

suplementary_data %>%
  filter(board_number != 65) %>% 
  filter_out_na(board_number) %>%
  mutate(output_path = paste0("B:/^Project Monthly Updates/", 
                              board_number_name, ".xlsx"),
         metadata_path = metadata_path, 
         overwrite = FALSE
  ) %>%
  nest(df = -c(output_path, metadata_path, overwrite)) %>%
  
  pwalk(xlr8_write_one, .progress = "text")




test_data %>%
  #filter(board_number == 11) %>% 
  filter_out_na(board_number) %>%
  mutate(output_path = paste0("B:/^Project Monthly Updates/", 
                              board_number_name, ".xlsx"),
         metadata_path = metadata_path, 
         overwrite = TRUE
         ) %>%
  nest(df = -c(output_path, metadata_path, overwrite)) %>%

  pwalk(xlr8_write_one, .progress = "text")


ezql_table("boards")