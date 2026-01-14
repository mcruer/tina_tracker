library(gplyr)
library(reformR)
library(templaforms)
library(tidyverse)
library(readxl)
library(magrittr)
library(listful)
library(databased)
library(janitor)


df <- monthly %>%
  filter_in(project_id, "^12-") %>%
  peek

workbook_template_path <- "data/workbook_template.xlsx"

generate_messages_df <- function (df,
                                  workbook_template_path,
                                  first_data_row = NULL) {
  
  codify <- function(df, text) {
    eval(parse(text = paste0("df %>% ", text)))
  }
  
  cell_template <- readxl::read_excel(workbook_template_path, "Cell Info") #
  
  messages_template <-
    readxl::read_excel(workbook_template_path, sheet = "Messages")
  
  if (nrow (gplyr::filter_in_na(messages_template, 
                                target, message, execute,
                                if_any_or_all = "if_any")) > 0){
    stop("The message tab of the worksheet template is missing values.")
  }
  
  messages_template <- messages_template %>%
    dplyr::left_join(cell_template %>% 
                       dplyr::select(column.name, sheet.number) %>%
                       rlang::set_names(c("target", "sheet")))
  
  if (nrow (gplyr::filter_in_na(messages_template, 
                                sheet,
                                if_any_or_all = "if_any")) > 0){
    stop("Target columns on the Messages sheet of the worksheet template do not match
         the columns in the Cell Info sheet.")
  }
  
  error_messages <- messages_template %>%
    dplyr::select (name, sheet, message, author)
  
  expressions <- messages_template %>%
    gplyr::filter_out_na(execute) %>% 
    dplyr::pull (execute)
  
  if (is.null(first_data_row)) {
    data_start_row <-
      cell_template %>%
      dplyr::pull(data.start.row) %>% 
      stats::median(na.rm = TRUE)
  }
  
  column_location <-
    cell_template %>%
    dplyr::select (column.name, column.number) %>%
    rlang::set_names(c("column_name", "column_number"))
  
  row_location <- df %>%
    gplyr::add_index(col_name = row_number) %>%
    gplyr::quickm(row_number, add, data_start_row - 1) %>%
    dplyr::select(project_id, row_number)
  
  messages <- df %>%
    listful::build(expressions, codify) %>%
    dplyr::select(project_id, dplyr::starts_with("error_")) %>%
    purrr::modify_if(is.logical, gplyr::na_to_F) %>%
    tidyr::pivot_longer(cols = -project_id) %>%
    #filter_in_na(value) %>%
    dplyr::filter (value) %>%
    dplyr::left_join(error_messages) %>%
    gplyr::quickm(name, str_remove, "error_") %>%
    tidyr::separate(name,
                    into = c("column_name", "index"),
                    sep = "_N") %>%
    dplyr::select(-index, -value) %>%
    #get_dupes(project.id, target.col) %>%
    dplyr::group_by(project_id, sheet, column_name, author) %>%
    gplyr::quicks(message, str_c, collapse = "; ") %>%
    dplyr::left_join(column_location) %>%
    dplyr::left_join(row_location) %>%
    dplyr::rename(col = column_number, row = row_number) %>%
    dplyr::ungroup() %>%
    dplyr::select (-column_name, -project_id)
  
  messages
  
}

generate_messages_df <- function (df,
                               workbook_template_path,
                               first_data_row = NULL) {
  
  codify <- function(df, text) {
    eval(parse(text = paste0("df %>% ", text)))
  }
  
  cell_template <- read_excel(workbook_template_path, "Cell Info") %>% 
    select(column.name, sheet.number) %>%
    set_names(c("target", "sheet"))
  
  messages_template <-
    read_excel(workbook_template_path, sheet = "Messages")
  
  if (nrow (filter_in_na(messages_template, 
                         target, message, execute,
                         if_any_or_all = "if_any")) > 0){
    stop("The message tab of the worksheet template is missing values.")
  }

  messages_template <- messages_template %>%
    left_join(cell_template)
  
  if (nrow (filter_in_na(messages_template, 
                         sheet,
                         if_any_or_all = "if_any")) > 0){
    stop("Target columns on the Messages sheet of the worksheet template do not match
         the columns in the Cell Info sheet.")
  }

  error_messages <- messages_template %>%
    select (name, sheet, message, author)
  
  expressions <- messages_template %>%
    filter_out_na(execute) %>% 
    pull (execute)

  if (is.null(first_data_row)) {
    data_start_row <-
    read_excel(workbook_template_path, sheet = "Cell Info") %>%
      pull(data.start.row) %>% 
      median(na.rm = TRUE)
  }
  
  column_location <-
    read_excel(workbook_template_path, sheet = "Cell Info") %>%
    select (column.name, column.number) %>%
    set_names(c("column_name", "column_number"))
  
  row_location <- df %>%
    add_index(col_name = row_number) %>%
    quickm(row_number, add, data_start_row - 1) %>%
    select(project_id, row_number)
  
  
  messages <- df %>%
    left_join(pt %>% select(project_id, date_ori, date_atp)) %>%
    build(expressions, codify) %>%
    select(project_id, starts_with("error_")) %>%
    modify_if(is.logical, na_to_F) %>%
    pivot_longer(cols = -project_id) %>%
    #filter_in_na(value) %>%
    filter (value) %>%
    left_join(error_messages) %>%
    quickm(name, str_remove, "error_") %>%
    separate(name,
             into = c("column_name", "index"),
             sep = "_N") %>%
    select(-index, -value) %>%
    #get_dupes(project.id, target.col) %>%
    group_by(project_id, sheet, column_name, author) %>%
    quicks(message, str_c, collapse = "; ") %>%
    left_join(column_location) %>%
    left_join(row_location) %>%
    rename(col = column_number, row = row_number) %>%
    ungroup() %>%
    select (-column_name, -project_id)
  
  messages
  
}


write_comment <- function (wb, author, sheet, message, col, row) {
  comment <- createComment(message, author = author, width= 1, height = 2)
  writeComment(wb = wb, sheet = sheet, col = col, row = row, comment =  comment)
  invisible(wb)
}

comments <- generate_messages_df (df, workbook_template_path)
df_out <- df %>%
  left_join(pt_data_to_import)
save(df_out, file = "data/tempfile.rda")
wb <- df %>%
  left_join(pt_data_to_import) %>%
  excelr8::excelr8(
    formatting.template.path = "data/Monthly Tracker Template.xlsx",
    data.template.path = "data/workbook_template.xlsx",
    #output.file.name = "output/temp.xlsx"
  ) %>%
  pbuild (comments, write_comment) %>%
  pbuild(conditional_formatting, add_conditional_formatting) %>%
  saveWorkbook("output/temp.xlsx",
             overwrite = TRUE)


add_conditional_formatting <- function (wb, sheet, col_start, col_end, row_start,
                                        row_end, rule, fontColour, bgFill){
  conditionalFormatting(wb,
                        sheet = sheet,
                        cols = col_start:col_end,
                        rows = row_start:row_end,
                        rule = rule,
                        style = createStyle(
                          fontColour = fontColour,
                          bgFill = bgFill
                        ))
  invisible (wb)
}

workbook_template_path <- "data/workbook_template.xlsx"
conditional_formatting <- read_excel(workbook_template_path, sheet = "Conditional Formatting", skip = 1)



