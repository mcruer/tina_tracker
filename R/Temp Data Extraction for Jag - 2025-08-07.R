source("r/libraries.R")
folder_path <- "B:/^Project Monthly Updates"
metadata_path <- "data/metadata_template.xlsx"

metadata <- summarize_metadata(metadata_path)
pt <- pt()

all_info <- metadata %>% pull_cell(all_info)

df <- file_tibble(folder_path, file_type = "xlsx") %>%
  mutate(raw = map(path, read_excel_all, .progress = "Read in...")) %>%
  mutate(monthly = map(raw, xlr8_read, all_info = all_info, fix_dates_regex = "$^", .progress = "Extract..."))%>%
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
      select(-starts_with("temp_formula"))
  )) %>%
  mutate(board_number = str_remove(file, " - .*$") %>%
           as.numeric(), 
         .before = 1) %>%
  #Get rid of duplicate files
  mutate(modified_date = map(metadata, pull_cell, mtime)) %>%
  unnest(modified_date) %>%
  group_by(board_number) %>%
  filter(modified_date == max(modified_date)) %>%
  ungroup() %>%
  select(-modified_date)

# df %>%
#   select(metadata) %>%
#   unnest(metadata)
# mutate(project_id = map_chr(monthly, pull_cell, project_id), .before = 1) %>%
#   left_join(pt %>% select(project_id, board_number)) %>%
#   relocate(board_number) %>%
#   select(-project_id)
# select(upload) %>%
#   #pull_cell(upload, 21) %>%
#   #relocate(date_tender_issue)
#   unnest(upload) %>% names()
# get_dupes(project_id)
# filter_in_na(basic_info)
# pull_cell(upload) %>%
#   names() %>%
#   as_tibble() %>% view
# 
# unnest(upload)
# 
# 
# 
# 
# intersect(
#   df %>%
#     pull_cell(basic_project_info) %>%
#     names(),
#   
#   df %>%
#     pull_cell(monthly)%>%
#     names()
# )
# 
# map()
# unnest(monthly) %>%
#   select(-c(pc, board_number_name, missing_data_count, metadata, raw, file)) %>%
#   pull_cell(basic_project_info) %>%
#   pull_cell(location_latitude)
# 
# 
# df %>%
#   pull_cell(monthly) %>%
#   select(-ends_with("formula"))
# 
# library(readxl)
# cp_ids <- read_excel("data/3. 0 - Financial Data 2024-25 March Report June 30 2025.xlsx",
#                      sheet = "CP", skip = 2) %>%
#   select(2) %>%
#   set_names("project_id")
# 
# 
# cc_ids <- read_excel("data/3. 0 - Financial Data 2024-25 March Report June 30 2025.xlsx",
#                      sheet = "CCC", skip = 2) %>%
#   select(2) %>%
#   set_names("project_id")
# 
# 
# lp_ids <- read_excel("data/3. 0 - Financial Data 2024-25 March Report June 30 2025.xlsx",
#                      sheet = "LP", skip = 2) %>%
#   select(2) %>%
#   set_names("project_id")
# 
# 
# frank <- frank()
# 
# frank %>%
#   left_join(
#     pt %>%
#       select(project_id, ori_fy_drop, project_type, scope_otg_added, scope_cc_spaces_added)
#   ) %>%
#   select(
#     project_id,
#     ori_fy_drop,
#     project_type,
#     construction_status,
#     date_construction,
#     date_opening,
#     scope_otg_added,
#     scope_cc_spaces_added
#   ) %>% 
#   filter_in(project_id, "64-089")
# count(construction_status)

merge_if_na_vec <- function(target, use_if_na) {
  dplyr::if_else(is.na(target), use_if_na, target)
}

#This is just here to demonstrate how gplyr::df_map works and how it is
#used with merge_if_na_vec()

tibble(project_id = c(1, 2, 3),
       v1 = c("old", "old", "old")
) %>%
  nest(existing = v1) %>%
  mutate(v1 = c("old", "new", NA)) %>%
  nest(changes = v1) %>%
  mutate(out = map2(changes, existing, 
                    ~df_map(.x, .y, merge_if_na_vec))) %>%
  unnest(-project_id, names_sep = "_")

frank <- frank()

revised_frank <- df %>%
  select(board_number, upload) %>%
  mutate(upload = map(upload, ~.x %>% select(
    project_id, 
    date_construction, 
    date_opening,
  ))) %>%
  unnest_keep(upload) %>%
  left_join(frank %>% select(project_id, construction_status)) %>%
  mutate(construction_status = case_when(
    construction_status == "Cancelled" ~"Cancelled",
    today() > date_opening ~ "Complete",
    today() > date_construction ~ "Under Construction",
    today() <= date_construction ~ "Planning",
    TRUE ~ construction_status
  )) %>%
  select(-board_number) %>%
  group_by(project_id) %>%
  slice(1) %>%
  ungroup() %>%
  nest(monthly = -project_id) %>%
  full_join(
    frank %>%
      select(project_id, construction_status, date_opening, date_construction) %>%
      nest(frank = -project_id)
  ) %>%
  arrange(project_id) %>%
  unnest_keep(monthly) %>%
  nest(monthly = -c(project_id, frank)) %>%
  mutate(out = map2(monthly, frank, 
                    ~df_map(.x, .y, merge_if_na_vec),
                    .progress = "Working..."
  )
  
  ) %>%
  select(project_id, out) %>%
  unnest(out)

pc <- ezql_table("commitments_all") %>%
  rename_with(.fn = ~.x %>% str_c("_pc"), .cols = -project_id) %>%
  mutate(
    event_number = str_remove(event_id_pc, ".*\\|") %>%
      as.numeric(),
    .before = 1
         ) %>%
  group_by(project_id) %>%
  arrange(event_number, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(project_id, date_construction_pc, date_opening_pc)

standalone_child_care <- pt %>%
  mutate(is_standalone_child_care = (project_category == "Child Care") %>%
           na_to_F()) %>%
  select(project_id, is_standalone_child_care)

out <- revised_frank %>%
  left_join(pt) %>%
  select(
    project_id,
    ori_fy_drop,
    project_type,
    construction_status,
    date_construction,
    date_opening,
    scope_otg_added,
    scope_cc_spaces_added
  ) %>%
  left_join(standalone_child_care) %>%
  left_join(pc)
 
out %>% clip_it()
# 
# cp_ids %>%
#   left_join(out) %>%
#   clip_it()
# 
# cc_ids %>%
#   left_join(out) %>%
#   clip_it()
# 
# lp_ids %>%
#   left_join(out) %>%
#   clip_it()
# events <- events()
# #events %>%
# pt %>%
#   filter_in(project_id, "07-") %>%
#   filter_in(project_name, "port el") %>%
#   #pull(project_name)
#   relocate(funding_lp)



