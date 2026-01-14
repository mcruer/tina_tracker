
ezql_rosetta <- load_data("ezql_rosetta") %>%
  mutate(sql_type = if_else(r == "event_id", paste0(sql_type, " NOT NULL"), sql_type))

monthly_all %>%
  arrange(project_id, date_meeting) %>%
  group_by(project_id) %>%
  add_index() %>%
  mutate(Event_ID = str_c(project_id, "|", index), .after = index) %>% 
  select(-where(is.list), -index) %>%
  ezql_change_names(ezql_rosetta, "r", "sql") %>%
  ezql_new("monthly_all", rosetta = ezql_rosetta, 
           rosetta_names_col = "sql", 
           rosetta_sql_types_col = "sql_type", primary_key = "Event_ID")

ezql_make_temporal("monthly_all", "Event_ID")

#Add Harvest Date to SQL server table ----


ezql_query("ALTER TABLE capitalProjects.Monthly
ADD Harvest_Date DATE;")

monthly<- load_data("monthly")

monthly %>%
  select(-where(is.list)) %>%
  ezql_edit("monthly", rosetta = load_data("ezql_rosetta"))

#Upload ezql_rosetta to SQL Server:

ezql_rosetta <- load_data("ezql_rosetta")

ezql_rosetta_rosetta <- tibble(
  r = names(ezql_rosetta),
  sql = r,
  sql_type = c(
    "nvarchar(250)",
    "nvarchar(250)",
    "nvarchar(50)",
    "nvarchar(50)"
  ),
  r_type = "as.character"
)

ezql_new(df = ezql_rosetta, table = "ezql_rosetta", 
         rosetta = ezql_rosetta_rosetta, rosetta_names_col = "sql")




