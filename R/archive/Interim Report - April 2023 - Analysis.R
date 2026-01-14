library(CProg) #loads the tidyverse, openxlsx, and various other packages
library(DataEDU)
library(magrittr)


#To Do
#Figure out how to deal with atp.anticipated.dates that are in the past.
#Look at NAs, especially in cost fileds/important dates

# Figure out this: 
#   Warning message:
#   There were 2 warnings in `mutate()`.
# The first warning was:
#   i In argument: `time.to.atp = time_length(today() %--% atp.anticipated.date, "years")`.
# Caused by warning:
#   !  2 failed to parse.

#loads the file used to make the table I want to replicate

#Use fiscal year, not school year
#Break it down to quarters
#Add assumpitons for projects without guidance from board
#Remove ASD Pilot, demo & land
#Recode accommodation pressure & child care to accommodation pressure
#Rooms and spaces for child care 
#Beaverton Thorah
#TDSB On hold projects - With Ministry
#57-017: Sept 1 2023
#21-107: Jan 1 2025
#65-047: Sept 1 2023
#If a project requires a site but doesn't have a date, create a flag
#Add a flag for projects that are included in child care list


#Make specific/additional tab for child care projects

#Do reconciliation of Project Counts



load("final.RData")

col.names <- mcp.new %>% names


interim.mcp <- tibble(file.names = str_c("Completed Templates/", list.files("Completed Templates/"))) %>%
  filter_out(file.names, "~") %>% 
  #filter_in(file.names, "57") %>%
  pull(1) %>%
  #extract(1) %>%
  map(
    ~ read_excel(
      .x,
      range = "A10:AN80",
      sheet = 3,
      col_names = FALSE,
      skip = 9
    ) %>%
      set_names(col.names) %>%
      filter_out_na(project.id) %>%
      reclass_c()
  ) %>%
  bind_rows() %>% 
  parse_guess_all () %>%
  quickm(project.id, ~str_replace(.x, "55-040", "55-040-01")) %>% 
  pipe_assign("interim.mcp.raw") %>%
  filter(opening.date > today()|is.na(opening.date)) %>% 
  filter_out(project.type, "ASD Pilot|Demolition|Land Acquisition") %>%
  filter_out(construction.status, "cancel") %>%
  filter_out(project.id, "29-030") %>%
  view

drop.stage.one <- interim.mcp.raw %>%
  anti_join(interim.mcp, by = "project.id") %>%
  view


left_join_select <- function (df.1, df.2, cols.string, include.project.id = TRUE, ...) {
  
  if (include.project.id == TRUE){
    cols.string <- c("project.id", cols.string)
  }
  
  df.1 %>%
    left_join(df.2 %>% 
                select (all_of(cols.string)),
              ...)
}

df <- interim.mcp %>%
  left_join_select(pt, "ori.date") %>%
  relocate(ori.date) %>%
  mutate (cc.perc = (cc.funding/total.funding)%>% replace_na(0),
          projected.request_cc = total.cost * cc.perc,
          projected.request_school = total.cost * (1-cc.perc),
          projected.request_total = total.cost,
          current.funding_cc = cc.funding,
          current.funding_school = total.funding *(1-cc.perc),
          current.funding_total = total.funding,
          time.to.atp = time_length(today() %--% atp.anticipated.date, "years"),
          total.project.time = time_length(ori.date %--% opening.date, "years"),
          .before = 1) %>%
  pivot_longer(cols = (starts_with("projected.") | starts_with("current."))) %>%
  separate(name, into = c("when", "funding.source"), sep = "_") %>%
  pivot_wider (names_from = "when",
               values_from = "value") %>%
  select (project.id, board.number,funding.source, current.funding, projected.request,
          construction.status, atp.anticipated.date, atp.date) %>%
  mutate (anticipated.atp.sy = fiscal_year(atp.anticipated.date)%>%
            str_replace("2020-21|2021-22", "2023-24"),
          anticipated.atp.sy = str_c("ATP ", anticipated.atp.sy, " FY")) %>%
  #count(anticipated.atp.sy) %>%
  rename(construction.status.2 = construction.status) %>%
  left_join_select(final %>% group_by(project.id) %>% slice(1), "construction.status") %>%
  mutate (project.timing = case_when(construction.status == "With Ministry" ~ construction.status,
                                     !is.na(anticipated.atp.sy) ~ anticipated.atp.sy,
                                     !is.na (atp.date) ~ "Under Construction",
                                     TRUE ~ "Error"),
          .before = 1) %>%
  #count (project.timing) %>%
  #quickm(n, divide_by, 3) %>%

  view



create_output_table <- function (df) {
  df %>%
  #final.residual %>%
     
    group_by(funding.source, project.timing) %>%
    summarise(
      n = n(),
      current.funding = sum(current.funding, na.rm = TRUE),
      projected.request = sum(projected.request, na.rm = TRUE),
    ) %>%  
    arrange (funding.source == "total",
             #funding.source == "Other",
             funding.source == "cc",
             .by_group = FALSE) %>% 
    ungroup() %>%  
    mutate(difference = projected.request - current.funding, .after= current.funding) %>% 
    pivot_wider(names_from = funding.source, values_from = c(current.funding, difference, projected.request)) %>%
    summary_row(where(is.numeric)) %>% 
    arrange(
      project.timing != "Check",
      project.timing != "Under Construction",
      project.timing != "With Ministry") %>% 
    mutate (project.timing = case_when(project.timing == "total" ~ "Total",
                                            TRUE ~ project.timing
    )) %>%
    # mutate(difference_cp = difference_Other + difference_cp,
    #        projected.request_cp = projected.request_Other + projected.request_cp) %>%
    # select(-atp.within.year, -difference_Other, -projected.request_Other) %>%
    gussy_up()
  
}

final.residual <- final %>%
  anti_join(df, by="project.id") %>% 
  mutate(funding.source = if_else(str_detect(funding.source, "total|cc$"), funding.source, "school")) %>%
  group_by(project.id, board.number, funding.source, construction.status) %>%
  summarise(
    current.funding = sum(current.funding, na.rm = TRUE),
    projected.request = sum(projected.request, na.rm = TRUE),
  ) %>%
  left_join_select(mcp.new, c("atp.anticipated.date", "atp.date")) %>% 
  mutate (project.timing = case_when(construction.status == "With Ministry" ~ construction.status,
                                     !is.na(atp.anticipated.date) ~ str_c ("ATP ", fiscal_year(atp.anticipated.date), " FY"),
                                     !is.na (atp.date) ~ "Under Construction",
                                     TRUE ~ "Error"),
          project.timing = str_replace(project.timing, "ATP 2020-21 FY", "ATP 2022-23 FY")
  ) %>%
  anti_join(drop.stage.one, by = "project.id") %>%
  view

final.residual %>%
  ungroup() %>%
  summarise(sum(current.funding) - sum (projected.request))


min.projected.increase <- final %>% 
  mutate(funding.source = if_else(str_detect(funding.source, "total|cc$"), funding.source, "school")) %>%
  group_by(project.id, board.number, funding.source, construction.status) %>%
  summarise(
    current.funding = sum(current.funding, na.rm = TRUE),
    projected.request = sum(projected.request, na.rm = TRUE),
  ) %>%
  mutate (min.increase.perc = if_else (current.funding == 0, 1, projected.request/current.funding)) %>%
  ungroup %>%
  select (project.id, funding.source, min.increase.perc) %>%
  view

interim.out <- bind_rows(
  df, 
  final.residual
  ) %>% 
  mutate (board.increase.perc = if_else (current.funding == 0, 1, projected.request/current.funding)) %>%
  left_join(min.projected.increase) %>%
  mutate (increase.perc = case_when (is.na(board.increase.perc) && is.na(min.increase.perc) ~ 1,
                                     is.na(min.increase.perc) ~ board.increase.perc,
                                     board.increase.perc %>% near(1) ~ min.increase.perc,
                                     TRUE ~ board.increase.perc),
          projected.request = increase.perc * current.funding,
          ) %>%

  view

increase.perc.out <- interim.out %>%
  filter_in(funding.source, "total") %>%
  select (project.id, increase.perc) %>%
  view

interim.mcp %>%
  left_join(increase.perc.out) %>%
  mutate (adjusted.total.cost.estimate = total.funding * increase.perc,
          .after = total.cost) %>%
  relocate(increase.perc, .before = adjusted.total.cost.estimate) %>%
  left_join_select(child.care.in.planning, "child.care.in.planning") %>%
  quickm(child.care.in.planning, replace_na, FALSE) %>%
  relocate (child.care.in.planning, .after =  construction.status) %>%
  view %>%
  {gussy_up_excel(df = ., title = "Interim Project Data Collection - May 2023")} %>%
  save_excel("Interim Data Collection - Project Level.xlsx")


cc.choping.block.key <- interim.mcp %>%
  left_join(increase.perc.out) %>%
  mutate (adjusted.total.cost.estimate = total.funding * increase.perc,
          .after = total.cost) %>%
  relocate(increase.perc, .before = adjusted.total.cost.estimate) %>%
  left_join_select(child.care.in.planning, "child.care.in.planning") %>%
  quickm(child.care.in.planning, replace_na, FALSE) %>%
  relocate (child.care.in.planning, .after =  construction.status) %>%
  filter(child.care.in.planning) %>%
  select(project.id)
  view


interim.out %>% na_review %>% view

interim.out %>% 
  create_output_table() %>%
  gussy_up() %>%
  view %>%
  clip_it(col_names = FALSE)
  

pt %>% 
  filter_pid("05.1-012-01") %>%
  relocate (ends_with("funding")) %>% view

df %>% 
  #filter_out(project.id, "12-260") %>%
  create_output_table() %>%
  view %>%
  clip_it(col_names = FALSE)


temp.3 <- interim.out %>%
  left_join(DataEDU::boards %>% select (1:2)) %>%
  # mutate(board.type = case_when(english & public ~ "English - Public",
  #                               english & !public ~ "English - Catholic",
  #                               !english & public ~ "French - Public",
  #                               TRUE ~ "French - Catholic"),
  #        .before = 1) %>%
  listify_df(board.name) %>%
  pipe_assign("temp", ~names(.x))%>%
  pipe_assign("temp.2") %>%
  map2(temp, ~.x %>% create_output_table %>% mutate (region = .y, .before = 1)) %>%
  bind_rows()

temp.3 %>%
  rename(board.name = region) %>%
  left_join(DataEDU::boards %>% select (1:2)) %>%
  arrange (board.number) %>%
  mutate(board.name.2 = lag(board.name),
         board.name = if_else (is.na(board.name.2)|board.name.2 != board.name, board.name, ""),
         
         .before = 1) %>%
  select (-board.name.2) %>%
  relocate(board.number, .before = 1) %>%
  mutate(board.number = if_else(str_detect(board.name, "^$"), "", as.character (board.number))) %>%
  mutate (space = "", .after = 1) %>%
  view %>%
  select (-(1:3)) %>%
  clip_it(col_names = FALSE)
view


interim.mcp %>% gussy_up() %>% view
  filter_in(construction.status, "cancel") %>% count (construction.status)
  filter(opening.date < today()) %>% 
  view


final


cc.interim <- interim.mcp %>%
  filter(project.catigory == "Child Care" | cc.funding == total.funding) %>% 
  view

cc.mcp <- mcp.new %>%
  filter(project.catigory == "Child Care" | cc.funding == total.funding) %>%
  anti_join(cc.interim, by = "project.id") %>% 
  view 

bind_rows(cc.interim %>% reclass_c(), cc.mcp %>% reclass_c()) %>%
  parse_guess_all() %>%
  arrange(board.number) %>%
  view %>%
  {gussy_up_excel(df = ., title = "Interim Project Data Collection - May 2023")} %>%
  save_excel("Child Care Projects - Revised.xlsx")


atp %>%
  mutate (inc.perc = total.sb.min/(cumulative.funding-total.sb.min),
          event.year= year(event.date),
          .before = 1) %>%
  filter(event.type == "ATP") %>%
  filter(inc.perc < Inf) %>%
  filter(event.year >= 2020) %>%
  #group_by(event.year) %>%
  summarise(n = n(), 
            inc.perc = mean(inc.perc)) %>%
  #quicks (inc.perc, mean) %>%
  view

interim.mcp %>%
  filter(total.cost == 0) %>%
  left_join_select(CAs, c("board.number", "ca.name"), include.project.id = FALSE) %>%
  relocate (ca.name) %>%
  arrange(ca.name) %>%
  view #%>%
  {gussy_up_excel(title = "Projects with Zero Cost", df = .)} %>%
  save_excel("Projects without cost info.xlsx") 
CAs %>% names


final.residual %>%
  ungroup() %>%
  select(project.id, construction.status) %>%
  unique %>%
  left_join(pt) %>% 
  filter_out(construction.status, "with") %>%
  filter (total.min.funding > 0) %>%
  pipe_assign("pids", ~.x %>% pull(project.id) %>% str_c(collapse = "|")) %>%
  view

interim.mcp %>%  #select (ends_with(".funding")|ends_with(".cost")) %>% na_review() %>% view
  #filter_out_numeric(fte.cost, na.rm = TRUE) %>% view
  reclass_n(ends_with(".cost")) %>%
  mutate_a(~.x%>%replace_na(0), ends_with(".funding")|ends_with(".cost")) %>%
  filter(total.cost > total.funding) %>%
  mutate(increase.perc = total.cost/total.funding, .after = total.cost) %>%
  summarise(
    increase.perc = sum (total.cost)/sum(total.funding)
  ) %>% 
  gussy_up() %>%
  view

child.care.in.planning <- read_excel("Copy of Child Care Projects - Revised lc v2.xlsx",
                                     skip = 2) %>% 
  bleach_names() %>%
  filter_in(include.in.analysis, "y") %>%
  mutate (child.care.in.planning = TRUE) %>%
  view

child.care.in.planning %>%
  group_by(board.number, board.name) %>%
  parse_guess_all() %>% 
  summarise(
    n = n(),
    cc.funding = sum(cc.funding),
    other.funding = sum(other.funding),
    total.funding = cc.funding + other.funding,
  ) %>%
  ungroup %>% 
  gussy_up() %>%
  view %>%
  clip_it()



cc.choping.block.with.atps<- cc.choping.block.key %>%
  left_join(atp) %>%
  group_by(project.id) %>%
  filter (n() > 1) %>% 
  filter(max(event.type == "ATP") == 1) %>% 
  mutate (change.cc.perc = sum(cc)/first(cc), .after = project.id) %>%
  relocate (cc, pod, .after = total.sb.min) %>%
  select (project.id:pod) %>%
  #gussy_up() %>%
  #view
  summarise(
    original.funding = first(cc),
    revised.cc.funding = sum(cc),
    change.cc.perc = revised.cc.funding/original.funding,
  ) %>%
  view

cc.choping.block.exposure <- cc.choping.block.key %>%
  left_join(cc.choping.block.with.atps) %>%
  left_join_select(pt, cols.string = c("cc.funding")) %>%
  quickm(change.cc.perc, ~.x %>% replace_na (1)) %>%
  #filter(change.cc.perc < 1.25) %>%
  mutate(
    original.cc.funding = if_else(is.na(original.funding), cc.funding, original.funding), 
    exposure = (1.25-change.cc.perc)*original.cc.funding,
    exposure = if_else(exposure < 0, 0, exposure)) %>% 
  select (-original.funding) %>%
  relocate (original.cc.funding, .after = project.id) %>% 
  view 
cc.choping.block.exposure %>%
  gussy_up() %>%
  clip_it()


cc.choping.block.exposure %>%
  quicks(exposure, sum)



















