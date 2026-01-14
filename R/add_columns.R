#Needed to correct a name that was added with a slight error.
#' ezql_query("
#'   EXEC sp_rename 
#'     @objname = N'capitalProjects.Monthly.Posttender_Checkin_Date', 
#'     @newname = N'Post_Tender_Checkin_Date', 
#'     @objtype = N'COLUMN';
#' ")



monthly_all <- ezql_table("monthly_all")

rosetta_new <- read_excel("data/New Columns for Rosetta.xlsx")

rosetta_new %>%
  ezql_edit("ezql_rosetta", rosetta = ezql_rosetta())

rosetta_new %>%
  select(sql, sql_type) %>%
  set_names(c("column_name", "data_type")) %>%
  pmap(ezql_column_sql_add, table = "monthly_all")

rosetta_new %>%
  select(sql, sql_type) %>%
  set_names(c("column_name", "data_type")) %>%
  pmap(ezql_column_sql_add, table = "monthly")



monthly_all %>%
  names() %in% (names(rosetta_new))
