
target.comparison <- function(df, target, comparison, operator, index = 1) {
  
  new.name <- paste("error", target, index, sep = ".")
  
  target_sym <- sym(target)
  comparison_sym <- sym(comparison)
  
  operator_fun <- get(operator) # get the function represented by the character string
  
  df %>%
    mutate(!!new.name := operator_fun(!!target_sym, !!comparison_sym))
}


# 
# codify <- function(df, text) {
#   eval(parse(text = paste0("df %>% ", text)))
# }

error.messages <- read_excel("data/Error Definitions and Messages.xlsx") %>%
  select (name, message) %>%
  view

expressions <- read_excel("data/Error Definitions and Messages.xlsx") %>%
    filter_out_na(expression) %>%
    pull (execute) 

column.location <- read_excel("data/Sheet Data.xlsx") %>%
  pipe_assign("data.start.row", ~.x %>% pull(data.start.row) %>% median) %>%
  select (column.name, column.number) %>%
  view

row.location <- monthly %>%
  group_by(board.number) %>%
  add_index(col_name = row.number) %>%
  ungroup() %>%
  quickm(row.number, add, data.start.row - 1) %>%
  select(project.id, row.number) %>%
  view

messages <- monthly %>%
  build(expressions, ~codify(., .x)) %>%
  select(project.id, starts_with("error_")) %>%
  #rename_with(~str_remove(.x, "error_")) %>%
  modify_if(is.logical, na_to_T) %>%
  pivot_longer(cols = -project.id) %>%
  filter (!value) %>%
  left_join(error.messages) %>%
  quickm(name, str_remove, "error_") %>%
  separate(name, into = c("column.name", "index"), sep = "_") %>%
  select(-index, -value) %>%
  #get_dupes(project.id, target.col) %>%
  group_by(project.id, column.name) %>%
  quicks(message, str_c, collapse = "; ") %>%
  left_join(column.location) %>%
  left_join(row.location) %>%
  rename(col = column.number, row = row.number) %>%
  select (-column.name) %>%
  view


monthly %>% 
  # relocate (ori.date, .before = atp.date) %>%
  # mutate(
  #   error.1.ori.date = ori.date > atp.anticipated.date
  #   ),
  #   .before = 1
  # ) %>% relocate(qa.1.ori.date) %>% view
  mutate (
    #1 Check
    qa.construction.status = !is.na(construction.status),
    
    qa.atp.anticipated.date = !(is.na(atp.anticipated.date) &
                                  is.na(atp.date) &
                                  str_detect(construction.status, "Pre-Construction")),
    qa.date.present =
      !((
        is.na(construction.date) |
          is.na(substantial.completion.date) |
          is.na(opening.date) |
          is.na (completion.date)
      ) &
        str_detect (construction.status, "Construction|Complete ")),
    qa.date.present = if_else (is.na(qa.date.present), TRUE, qa.date.present),
    .before = 1) %>%
  rowwise() %>%
  mutate(
    qa.construction.earliest.date = construction.date == min (construction.date, 
                                                              substantial.completion.date,
                                                              opening.date,
                                                              completion.date,
                                                              na.rm = TRUE),
    qa.construction.earliest.date = if_else(is.na(qa.construction.earliest.date), TRUE, qa.construction.earliest.date),
    qa.completion.latest.date = completion.date == max (construction.date, 
                                                        substantial.completion.date, 
                                                        completion.date,
                                                        na.rm = TRUE),
    qa.completion.latest.date = if_else(is.na(qa.completion.latest.date), TRUE, qa.completion.latest.date),
    
    
    
    .after = qa.date.present
  ) %>%
  ungroup %>%
  mutate(
    qa.includes.project.cost = !(is.na(building.cost) & is.na(fte.cost) & 
                                   is.na(land.cost)) |
      str_detect(construction.status, "Cancel"),
    qa.includes.project.cost = if_else (is.na(qa.includes.project.cost), FALSE, qa.includes.project.cost),
    qa.fdk.remaining.less.than.funded = fdk.remaining <= fdk.funding | 
      is.na (fdk.remaining) | is.na(fdk.funding),
    qa.other.remaining.less.than.funded = (other.remaining <= other.funding) | 
      is.na(other.remaining) | is.na (other.funding),
    qa.savings.is.positive = project.savings >= 0 | is.na(project.savings),
    qa.remaining.perc.entered = str_detect(construction.status, "^Complete|Cancel|Purchase completed") | 
      spending.total.perc >.989 | 
      net.total.remaining <= 0,
    qa.remaining.perc.entered = if_else (is.na(qa.remaining.perc.entered), TRUE, qa.remaining.perc.entered),
    
    qa.savings.less.than.remaining = project.savings <= gross.total.remaining | is.na(project.savings),
    qa.anticipated.atp.is.in.future = atp.anticipated.date > ymd("2022-08-31") | is.na (atp.anticipated.date),
    qa.savings.tab.correct = total.savings == project.savings,
    
    
    .before = 1
  ) %>%
  relocate (starts_with("qa"))





