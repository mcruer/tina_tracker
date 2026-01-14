
monthly<- load_data("monthly")

monthly %>% 
  arrange(desc(date_meeting)) %>%
  filter_in(project_id, "42")


cp_2023_projects <- load_data("cp_2023_approved_projects_key") %>%
  left_join(load_data ("cp_2023") %>% 
              select(-project_id)) %>%
  select(-where(is.list)) %>%
  filter_out(project_id, "20-125") %>%
  mutate(construction_status = "Planning") %>%
  rename(date_opening = opening_date,
         date_construction = construction_date,
         date_completion = completion_date,
         date_site_purchase = site_purchases_date
         
         ) %>%
  quickm(where(is.character) & matches("date"), 
         ~.x %>% as.numeric () %>% excel_numeric_to_date()) %>%
  select(project_id, project_name, construction_status, date_construction, 
         date_opening, date_completion,
         date_site_purchase)

monthly <- bind_rows(monthly, cp_2023_projects) %>% 
  arrange(project_id) %>% #names() %>% as_tibble() %>% clip_it()
  quickm(c(date_meeting, date_harvest), replace_na, today()) %>%
  unique()

database_it(monthly)

monthly_all <- load_data("monthly_all")

monthly_all <- bind_rows(monthly_all, monthly) %>%
  select(-any_of("date_modified")) %>%
  get_distinct_rows ()

database_it(monthly_all)


