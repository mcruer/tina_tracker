#This is pulled from the Project Tracker:
add_calculated_columns <- function(df, event_date) {
  
  current_event_date <- event_date
  
  related_events <- events %>%
    filter(project_id %in% df$project_id)
  
  columns_to_fill_down <-
    setdiff(
      pt_columns(),
      c(
        "date_most_recent_event",
        "project_id",
        "approval_note",
        "analyst_assessment"
      )
    )
  
  locations <- ezql_table("locations") %>%
    rename(location_city = city)
  
  df %>%
    #Add the event_date
    mutate(event_date = current_event_date) %>%
    bind_rows(related_events) %>%
    #Creating the event_number and event_id
    group_by(project_id) %>%
    arrange(event_number, .by_group = TRUE) %>%
    add_index(col_name = event_number) %>%
    mutate(event_id = str_c(project_id, "|", event_number)) %>%
    
    #Make sure that the funding and scope have zeros, not NA values
    quickm(matches("^funding|^scope"), replace_na, 0) %>%
    
    #Fill down columns 
    fill(columns_to_fill_down, .direction = "down") %>%
    ungroup() %>%
    #Select only the events from the current approval.
    filter(event_date == current_event_date) %>%
    
    #Create date_ori if it's the first event; otherwise keep it as is (should 
    #have been pulled down from the prvious event in the fill down step).
    mutate(
      date_ori = if_else(is.na (date_ori), event_date, date_ori),
      #Ensure Result is calculated if it's missing
      Result = if_else(is.na(Result), project_type, Result),
    ) %>%
    #Add the English description
    CProg2::add_description_e() %>%
    select(-project_description) %>%
    rename(project_description = description_e) %>%
    
    #Clean board names
    select(-board_name) %>%
    left_join(ezql_table("boards")) %>%
    
    #Match in the municiaplity, municipality code and subregion based on
    #the city.
    
    #First, I remove the columns we're pulling in from the locations dataset.
    select(
      -c(
        location_municipality_code, 
        location_municipality_legal, 
        location_subregion)
    ) %>%
    
    #In a perfect world, I'd go through the Events dataset and make sure that
    #all the cities were matched to what's found in the location list. 
    #However, without time to do that,  I can at least convert them to 
    #upper case. All the cities in the reference file in are in ALL CAPS. So I'm
    #converting what we have on file to ALL CAPS to increase the rate of a 
    #match.
    quickm(location_city, str_to_upper) %>%
    left_join(locations) %>%
    
    #Select all the names need for the events table
    select(all_of(names(events)))
}

#I still need to find something to do with the sign_installed column

basic_info <- df_raw %>%
  select(monthly) %>%
  nested_mutate(monthly, temp_site_purchase_formula, as.character) %>%
  unnest(monthly) %>%
  select(board_number_name, basic_project_info) %>%
  nested_mutate(basic_project_info, starts_with("date"), as.character) %>%
  nested_mutate(basic_project_info, starts_with("date"), 
                ~str_replace(.x, "Jan. 12 2025", "2025-01-12")) %>%
  nested_mutate(basic_project_info, starts_with("date"), as_date) %>%
  unnest(basic_project_info) %>%
  filter_out_na(project_id) %>%
  quickm(c(location_address, location_city, location_postal_code), str_to_upper) %>%
  quickm(location_postal_code, str_remove_all, " ")


location_names <- basic_info %>%
  select(project_name_official, starts_with("location")) %>%
  names()

events <- ezql_table("Events")
pt <- pt()
revised_location <- basic_info %>%
  group_by(project_id) %>%
  slice(1) %>%
  ungroup() %>%
  nest(new = all_of(location_names)) %>%
  left_join(pt %>%
              select(project_id, all_of(location_names)) %>%
              nest(original = -project_id)) %>%
  select(project_id, new, original) %>%
  mutate(
    check = map2(new, original, df_map, identical)
  ) %>%
  unnest(check) %>%
  filter(if_any(starts_with("location_"), ~ .x == FALSE)) %>%
  select(project_id, new) %>%
  unnest(new) %>%
  mutate(event_type = "Location or Official Name Update") %>%
  add_calculated_columns(today())

revised_location %>%
  ezql_edit("Events", rosetta = ezql_rosetta())

