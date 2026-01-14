
source(here2("R", "functions.R"))

monthly_all <- load_data("monthly_all")
monthly <- load_data("monthly")
board_allocations <- load_data("board_allocations")
contact_info <- load_data("contact_info")

director_email <-contact_info %>%
  filter_in(position, "director") %>%
  pull(email)

manager_emails <- contact_info %>%
  filter_in(position, "manager") %>%
  pull(email)

manager_names <- contact_info %>%
  filter_in(position, "manager") %>%
  pull(name) %>%
  str_remove(" .*") %>%
  str_replace("Christopher", "Chris")

director_name <- contact_info %>%
  filter_in(position, "director") %>%
  pull(name) %>%
  str_remove(" .*") %>%
  str_replace("Christopher", "Chris")

admin_email <- "geordie.mcruer@ontario.ca"

pt <- CProg2::pt()

monthly_progress <- monthly %>%
  select(-1:-3) %>% 
  relocate(date_harvest) %>%
  mutate(board_number = str_remove(project_id, "-.*")) %>% 
  group_by(board_number) %>%
  slice(1) %>%
  ungroup() %>%
  to_number(board_number) %>%
  select(board_number, date_meeting) %>%
  left_join(board_allocations) %>%
  mutate(time_since_meeting = today () - date_meeting,
         recently_updated = time_since_meeting < 31) %>%
  arrange(analyst) %>%
  group_by(analyst) %>%
  nest(df = -analyst) %>%
  left_join(contact_info %>% rename(analyst = name)) %>%
  left_join(contact_info %>% 
              select(-manager) %>% 
              rename (manager = name, manager_email = email) %>%
              select(manager, manager_email)) %>%
  mutate(recent_meetings = map_dbl(df, ~.x %>% 
                                     pull(recently_updated) %>% 
                                     sum()),
         board_count = map_dbl(df, nrow),
         analyst_first_name = str_remove(analyst, " .*"),
         body = glue(str_c("Hi {analyst_first_name}, \n\n",
                           "I just updated  all of ",
                           "our monthly trackers. Of the {board_count} boards ",
                           "you're responsible for, {recent_meetings} have been ",
                           "updated with meetings in the last 31 days. I've ",
                           "attached a file that contains some more detailed info. \n\n",
                           "As always, please let me know if you have identified ",
                           "any bugs within the monthly trackers or with the ",
                           "process. I'm also happy to chat about features you'd ",
                           "like to see added or improvements in process.\n\n",
                           "Have a great week!\nGeordie\n\n",
                           "Note: This message was automatically generated.")) %>%
           as.character(),
  ) %>%
  mutate(data.template.path = here2 ("data","Monthly Meeting - Analysts - Workbook Template.xlsx"),
         formatting.template.path = here2 ("data", "Monthly Meeting - Analysts - Formatting Template.xlsx"),
         output.file.name = str_c (here2(), "/", 
                                   analyst %>%
                                     str_remove(" .*"), 
                                   " - Monlthy Tracker Analyst Report.xlsx")) %>%
  ungroup %>%
  peek

monthly_progress %>%
  mutate(df = map(df, to_number, time_since_meeting)) %>%
  select(df, data.template.path, formatting.template.path, output.file.name) %>%
  pwalk(excelr8)
  
monthly_progress %>%
  rename(to = email,
         attachment_paths = output.file.name,
         ) %>%
  rowwise() %>%
  mutate(cc = list (c(manager_email, 
    admin_email))) %>%
  ungroup () %>%
  mutate(subject = "Analyst Report - Monthly Project Updates") %>%
  # mutate(
  #   cc = list(c("geordie.mcruer@ontario.ca", "geordie.mcruer@ontario.ca")),
  #   to = "geordie.mcruer@ontario.ca"
  # ) %>% 
  select(to, 
         cc, 
         subject, 
         body, 
         attachment_paths
         ) %>%
  pwalk(send)

out <- monthly_progress %>% 
  select(analyst, manager, df) %>%
  unnest(df) %>% 
  mutate(board_name_number = str_c(board_number, board_name, sep = " - ")) %>%
  select(analyst, manager, board_name_number, time_since_meeting) %>%
  to_character (time_since_meeting) %>%
  #quickm(time_since_meeting, str_c, " days") %>%
  pivot_longer(cols = c(board_name_number, time_since_meeting)) %>%
  group_by(analyst) %>%
  add_index() %>%
  quickm(index, ~.x %>% divide_by(2) %>% ceiling()) %>%
  quickm(index, ~str_c("board_", .x)) %>%
  pivot_wider(names_from = index) %>%
  select(-name) %>% 
  ungroup() %>%
  mutate(board_or_days = rep(c("School Board:", "Days Since Last Meeting:"), times = nrow(.)/2),
         .after = manager) %>%
  nest(df = -manager) %>%
  mutate(data.template.path = here2 ("data", "Manager Report - workbook_template.xlsx"),
         formatting.template.path = here2 ("data", "Manager Report Formatting Template.xlsx"),
         output.file.name = str_c (here2 (), "/output/", 
                                   manager %>%
                                     str_remove(" .*"), 
                                   " - Monlthy Tracker Manager Report.xlsx"))

out %>%
  select(-manager) %>%
  pwalk(excelr8)


Sys.sleep(15)

out %>%
  rename(name = manager) %>%
  left_join(contact_info) %>%
  rename(to = email) %>%
  mutate(cc = list(c(
    director_email, admin_email
  ))) %>%
  mutate(
    subject = "MANAGER REPORT: Monthly Board Meeting Tracker",
    body = str_c(
      "Hi ",
      str_remove(name, " .*"),
      ",\n\n",
      "Please see attached for an excel sheet that details when anlaysts ",
      "have last spoken with their boards and updated their monthly  ",
      "trackers. For any analysts that are behind with their meetings, ",
      "please raise the issue with them at your next opportunity. ",
      "This ensures that we have up-to-date data.\n\n",
      "Let me know if you have any questions or concerns.\n\n",
      "Thanks!\n",
      "-G\n\n",
      "Please note: This message was automatically generated."
    ),
    attachment_paths = output.file.name
  ) %>%
  # mutate(to = "geordie.mcruer@ontario.ca",
  #        cc = "geordie.mcruer@ontario.ca") %>%
  select(to, cc, subject, body, attachment_paths) %>%
  pwalk(send)


Sys.sleep(15)

changes <- monthly_all %>%
  arrange(desc(date_harvest)) %>%
  group_by(project_id) %>%
  slice(1:2) %>%
  distinct(
    pick(
      project_id,
      starts_with("date"),
      cost_total,
      construction_status,
      square_footage_new,
      square_footage_reno,
      -date_harvest,
      -date_meeting
    ),
    .keep_all = TRUE
  ) %>%
  filter(n () == 2) %>%
  select(-c(file, path, raw_df, metadata)) %>%
  arrange(project_id, date_meeting) %>%
  #arrange(date_meeting, .by_group = TRUE) %>%
  ungroup() %>%
  peek

excelr8(
  changes,
  here2 ("data", "Monthly Meeting - Critical Changes - Workbook Template.xlsx"),
  here2 ("data", "Monthly Meeting - Critical Changes - Formatting Template.xlsx"),
  output.file.name = here2 ("output", "Critical Changes.xlsx")
)

send(
  to = c(director_email,
         manager_emails),
  cc = admin_email,
  subject = "Monthly Updates: Critical Changes",
  attachment_paths = str_c (here2 ("output", "Critical Changes.xlsx")),
  body = str_c(
    "Hi ", director_name, ", ", manager_names[1], " & ", manager_names[2], ",\n\n",
    "See attached for a summary of all changes from the current data scraping ",
    "of the analysts' monthly updates sheets.\n\n",
    "The attached file shows changes ",
    "between the most recent meeting and the previous one. The sheet only ",
    "includes projects that have changes from one meeting to the next in ",
    "construction status, square footage, milestone dates or cost. \n\n",
    "The file ",
    "attached contains two lines for each project ID, representing the most ",
    "recent meeting and the one before that. Any changes on the critical ",
    "values listed above are highlighted in the second row.\n\n",
    "Let me know if you would like any changes to this report. I'd like to ",
    "make this process as helpful for everyone involvled as possible.\n\n",
    "Thanks!\n",
    "Geordie\n\n",
    "Please note: This email is automatically generated."
  )

)


pt_data_to_import <- pt %>% 
  #rename(board_number = board) %>%
  mutate(funding_other = funding_total - (funding_lp + funding_cp + funding_fdk +
                                            funding_cc + funding_eo + funding_chr)) %>%
  select(project_id, 
         board_number,
         project_type,
         project_category,
         date_ori,
         date_atp,
         funding_lp,
         funding_cp,
         funding_fdk,
         funding_cc,
         funding_eo,
         funding_chr,
         funding_other,
         funding_total
  ) %>%
  to_number(board_number) %>%
  left_join(load_data("board_allocations") %>% filter_out_na(board_number)) %>%
  select(-analyst)


monthly_all_out <- monthly_all %>%
  left_join(pt_data_to_import) %>%
  group_by(project_id) %>%
  mutate(is_most_recent = date_meeting == max(date_meeting)) %>%
  arrange(board_number, project_id) %>%
  relocate(is_most_recent) %>% 
  select(!where(is.list))%>%
  ungroup()

excelr8(monthly_all_out, 
        here2("data/Monthly Tracker - Output for Analysts - Workbook Template.xlsx"),
        here2("data/Monthly Tracker - Output for Analysts - Formatting  Template.xlsx"),
        output.file.name = str_c ("//csc.ad.gov.on.ca/dfs$/GrpData/CBSD/CAPITAL/",
                                  "STRATEGY/Operations/BOARDS/",
                                  "^Project Monthly Updates/",
                                  "1.0 Monthly Updates - All Boards/", 
                                  "^All Monthly Updates.xlsx"))
