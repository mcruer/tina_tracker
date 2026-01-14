library(CProg)
library(DataEDU)

list.files("Completed Templates/")

load("data/col.names.RData")

file.names <- tibble(file.names = str_c("output/", list.files("output/"))) %>%
  filter_out(file.names, "~")

monthly <- file.names %>%
  mutate (raw = map (file.names, ~suppressMessages (read_excel(.x,
                              range = "A1:AN80",
                              sheet = "Monthly Project Updates",
                              col_names = FALSE,
                              col_types = "text")),
                        .progress = "text"))%>%
  mutate(meeting.date = map_chr(raw, ~.x %>% slice (2) %>% pull (2)) %>%
           as.numeric %>%
           excel_numeric_to_date(),
         data = map (raw, ~.x %>%
                       slice(-1:-7) %>%
                       set_names(col.names) %>%
                       filter_out_na(project.id))) %>%
  select (-raw) %>%
  unnest(data) %>%
  parse_guess_all() %>%
  quickm(project.id, ~str_replace(.x, "55-040", "55-040-01")) %>% 
  mutate_a(.f = excel_numeric_to_date, ends_with("date") & where(is.numeric)) %>%
  mutate (scrape.date = today()) %>%
  view


file.names %>%
  mutate(
    original.path = str_remove(file.names, "(?<=\\/)[^\\/]*$"),
    original.file.name = str_extract(file.names, "(?<=\\/)[^\\/]*$"),
    new.location = str_c(original.path, "archive/", as.character(today()), " - ", original.file.name)) %>%
  select (file.names, new.location) %>%
  set_names(c("from", "to")) %>%
  view %>%
  pwalk(file.copy)

save (monthly, file = "monthly.RData")
load(file = "monthly.RData")
