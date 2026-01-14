

generate_xls_formula <- function (text, data.start.row, data.length) {
  xls <- function (var.name) {
    col_num_to_let <- function(col_number) {
      if (col_number < 1) {
        stop("Column number must be greater than or equal to 1")
      }
      
      col_name <- ""
      
      while (col_number > 0) {
        remainder <- (col_number - 1) %% 26
        col_name <- paste0(LETTERS[remainder + 1], col_name)
        col_number <- (col_number - remainder - 1) %/% 26
      }
      
      return(col_name)
    }
    
    var.name <- ensym(var.name) %>% as.character
    
    data.template.row <- data.template %>%
      filter(column.name == var.name)
    
    
    column <- data.template.row %>%
      pull(column.number) %>%
      col_num_to_let
    
    paste0(column, data.start.row:(data.start.row + data.length - 1))
  }
  
  column.name <- data.template %>% pull (column.name)
  
  text %>%
    build(column.name,
          ~str_replace(., .x, str_c("{xls(", .x, ")}"))) %>% 
    glue
  
}



write_xls_from_datasheet <- function(data.template,
                                     df,
                                     wb) {
  
  col_num_to_let <- function(col_number) {
    if (col_number < 1) {
      stop("Column number must be greater than or equal to 1")
    }
    
    col_name <- ""
    
    while (col_number > 0) {
      remainder <- (col_number - 1) %% 26
      col_name <- paste0(LETTERS[remainder + 1], col_name)
      col_number <- (col_number - remainder - 1) %/% 26
    }
    
    return(col_name)
  }
  
  
  # write_formula_sum_continous <- function (wb,
  #                                          sheet.number = 1,
  #                                          column.number = 4,
  #                                          data.length = 6,
  #                                          data.start.row = 2,
  #                                          sum.col.start = 2,
  #                                          sum.col.end = 3) {
  #   temp.formula <- paste0 (
  #     "SUM(",
  #     col_num_to_let(sum.col.start),
  #     data.start.row:(data.start.row + data.length - 1),
  #     ":",
  #     col_num_to_let(sum.col.end),
  #     data.start.row:(data.start.row + data.length - 1),
  #     ")"
  #   )
  # 
  #   class(temp.formula) <- c("formula", class(temp.formula))
  # 
  # 
  #   writeData(
  #     wb,
  #     sheet = sheet.number,
  #     x = temp.formula,
  #     startCol = column.number,
  #     startRow = data.start.row
  #   )
  # 
  #   return(wb)
  # 
  # }
  
  
  
  write_data <-
    function (wb,
              df,
              column.name,
              sheet.number,
              column.number,
              data.start.row,
              data.length) {
      
      data <- df %>%
        select(all_of(column.name)) %>%
        head (min(data.length, nrow(df)))
      
      writeData(
        wb,
        sheet = sheet.number,
        x = data,
        startCol = column.number,
        startRow = data.start.row,
        colNames = FALSE
      )
      
      return(wb)
      
    }
  
  
  pwalk(data.template,
        function(column.name,
                 sheet.number,
                 column.number,
                 column.type,
                 data.length,
                 data.start.row,
                 sum.col.start,
                 sum.col.end,
                 validation.type,
                 validation.operator,
                 validation.value,
                 formula.text,
                 locked,
                 cell.colour,
                 font.colour) {
          
          if(is.na(data.length)) data.length <- nrow(df)
          if(is.na(locked)) locked <- TRUE
          if(is.na(cell.colour)) cell.colour <- NULL
          if(is.na(font.colour)) font.colour <- NULL
          
          # if (!is.na(column.type) &&
          #     column.type == "formula.sum.continious") {
          #   write_formula_sum_continous(
          #     wb = wb,
          #     data.start.row = data.start.row,
          #     data.length = data.length,
          #     sum.col.start = sum.col.start,
          #     sum.col.end = sum.col.end,
          #     sheet.number = sheet.number,
          #     column.number = column.number
          #   )
          # }
          
          cell.style <- createStyle(
            fgFill = cell.colour,
            fontColour = font.colour,
            locked = locked
          )
          
          addStyle(
            wb = wb,
            sheet = sheet.number,
            style = cell.style,
            rows = data.start.row:(data.start.row + data.length - 1),
            cols = column.number,
            stack = TRUE
          )
          
          
          
          if (!is.na(column.type) && column.type == "data") {
            write_data(
              wb = wb,
              df = df,
              column.name = column.name,
              sheet.number = sheet.number,
              column.number = column.number,
              data.start.row = data.start.row,
              data.length = data.length
            )
          }
          
          if (!is.na(validation.type)) {
            if (validation.type == "date") {
              dataValidation(
                wb,
                sheet = sheet.number,
                col = column.number,
                rows = data.start.row:(data.start.row + data.length),
                type = "date",
                operator = validation.operator,
                value = as.Date(validation.value)
              )
            }
            
            else{
              dataValidation(
                wb,
                sheet = sheet.number,
                col = column.number,
                rows = data.start.row:(data.start.row + data.length),
                type = validation.type,
                value = validation.value
              )
            }
            
          }
          
          if(!is.na(formula.text)) {
            x <- generate_xls_formula(formula.text, data.start.row, data.length)
            writeFormula(wb, 
                         sheet = sheet.number, 
                         x = x, 
                         startCol = column.number, 
                         startRow = data.start.row)
          }
          
        })
}

# Load the workbook template
worksheet.template.path <- "data/Monthly Tracker Template.xlsx"
data.template <- read_excel("data/Sheet Data.xlsx")
monthly <- monthly %>%
  mutate (meeting.date = ymd("2023-06-16"))

generate_monthly_templates <- function (df, data.template) {
    wb.template <- loadWorkbook(worksheet.template.path)
    wb.active <- copyWorkbook(wb.template)
    remove(wb.template)
    
    write_xls_from_datasheet(data.template, df, wb.active)
    
    board.number <- df %>%
      pull(board.number) %>%
      extract(1)
    board.name <- df %>%
      pull(board.name) %>%
      extract(1)
    file.name <-
      str_c("output/", board.number, " - ", board.name, " Monthly Update.xlsx")
    
    # whiteout <- createStyle(
    #   fontColour = "white"
    #   #bgFill = "white"
    # )
    # 
    # addStyle(wb.active, 1, style = whiteout, rows = 1, cols = 7)

    # writeFormula(wb.active, 
    #              sheet = 1,
    #              x = "TODAY()",
    #              startCol = 7, 
    #              startRow = 1
    #              )
    
    
    greyout <- createStyle(
      fontColour = "#000000",
      bgFill = "#CCCCCC"
    )
    
    conditionalFormatting(wb.active,
                          sheet = 1,
                          cols = 1:40,
                          rows = 3:50,
                          rule = "$B$2!=$G$1",
                          style = greyout)
    
  
    write_comment <- function (message, col, row) {
      comment <- createComment(message, "Geordie", width= 1, height = 2)
      writeComment(wb = wb.active, sheet = 1, col = col, row = row, comment =  comment)
    }
    
    comment.df <- df %>%
      select (project.id) %>%
      left_join(messages) %>%
      filter_out_na(message) %>%
      #view %>%
      select(-project.id)
      
    pwalk(comment.df, write_comment)

    saveWorkbook(wb.active,
                 file.name,
                 overwrite = TRUE)
    
    remove(wb.active)
    
}



walk(20,
     #monthly %>% pull(board.number) %>% unique,
     ~ suppressWarnings(suppressMessages(invisible(
       generate_monthly_templates(monthly %>%
                                    filter(board.number == .x),
                                  data.template)
     ))),
     .progress = "text")

