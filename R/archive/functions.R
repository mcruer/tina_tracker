library(databased)
library(tidyverse)
library(reformR)
#library(templaforms)
library(gplyr)
library(janitor)
library(magrittr)
#library(excelr8)
library(rlang)
library(glue)
library(RDCOMClient)
library(email)
library(ezekiel)
library(CProg2)

path <- str_c ("//csc.ad.gov.on.ca/dfs$/GrpData/CBSD/CAPITAL/STRATEGY/", 
               "Operations/BOARDS/^Project Monthly Updates/", 
               "1.0 Monthly Updates - All Boards/")
path_archived_folder <- str_c ("//csc.ad.gov.on.ca/dfs$/GrpData/CBSD/CAPITAL/",
                               "STRATEGY/Operations/BOARDS/",
                               "^Project Monthly Updates/",
                               "1.0 Monthly Updates - All Boards/Archive/")
date_harvest <- today()
path_newly_archived_files <- str_c(path, "Archive/", date_harvest, "/")

calculate_construction_status <- function (df) {
  df %>%
    mutate(
      construction_status =
        case_when(
          date_completion <= date_meeting ~ "Complete",
          date_opening <= date_meeting ~ "Open",
          date_construction <= date_meeting ~ "Under Construction",
          date_construction > date_meeting ~ "Planning",
          TRUE ~ NA_character_
        )
    )
}

clean_monthly <- function (df) {
  
  strings_to_turn_na <- 
    str_c("^",
          c(
            "N/A",
            "Unknown ATM",
            "Same as above",
            "Not Applicable",
            "\\?"
          ), "$", collapse = "|")
  
  strings_to_remove <- c(
    "sq.ft",
    " ",
    ",",
    "sq ft",
    "approx", "'"
  ) %>% str_c(collapse = "|")
  
  
  df %>%
    quickm(date_meeting, ~.x %>% as.numeric() %>% excel_numeric_to_date()) %>%
    mutate(date_harvest = str_remove(file, "/.*") %>% as.Date()) %>%
    unnest(monthly) %>%
    clean_names() %>%
    quickm(project_id, str_replace, "^55-040-01-01$", "55-040-01") %>%
    quickm(where(is.character) & matches ("date|square_footage"), replace_with_na, strings_to_turn_na) %>% 
    quickm(where(is.character) & matches ("date|square_footage"), str_remove_all, strings_to_remove) %>% 
    quickm(where(is.character) & matches ("date|square_footage"), replace_with_na, "[:alpha:]") %>% 
    #filter_out_numeric(where(is.character) & matches("date|square_footage"), na.rm = TRUE) %>%
    quickm(where(is.character) & matches("date|square_footage"), as.numeric) %>%
    quickm(where(is.numeric) & contains("date"), excel_numeric_to_date) %>%
    quickm(c(cost_land, cost_building, cost_fte, cost_total, cost_variance), as.numeric) %>%
    #Code below is needed for recalculating construction status. If the file is
    #never opened, the cells with functions in them never get a chance to evaluate
    #to anything. As a result, we get a bunch of NAs here when we shouldn't.
    calculate_construction_status()
    
    
}

get_distinct_rows <- function (df) {
  df %>%
    relocate(file, path, raw_df, metadata, date_harvest) %>%
    arrange(date_harvest) %>%
    distinct(pick(project_id, date_meeting), .keep_all = TRUE) %>%
    #distinct(pick(-c(file, path, raw_df, metadata, date_harvest, cost_variance)), .keep_all = TRUE) %>%
    arrange(project_id)
}


