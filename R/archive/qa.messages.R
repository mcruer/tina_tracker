messages <- tribble (
  ~names, ~message,
  "qa.construction.status", "Construction status (Col 7) cannot be left blank. Please complete.",
  "qa.atp.anticipated.date", "Please provide an Anticipated ATP Date (Col 10) for this Project",
  "qa.date.present", "Please provide all milestone dates (Cols 11 to 14) for this project.",
  "qa.construction.earliest.date", "The construction date (Col 11) must be earlier than all other milestone dates (Cols 12 to 14). Please correct.",
  "qa.completion.latest.date", "The project completion date (Col 14) should come after all other milestone dates (Cols 11 to 13). Please correct.",
  "qa.includes.project.cost", "Please ensure that the project cost section (Columns 23 -25) has been completed",
  "qa.fdk.remaining.less.than.funded", "The remaining allocation for FDK (Col 30) cannot be greater than the total FDK Funding (Col 17). Please correct.",
  "qa.other.remaining.less.than.funded", "The remaining allocation for Other Funding (Col 34) cannot be greater than the total Other Funding (Col 21). Please correct.",
  "qa.savings.is.positive", "Project Savings (Col 36) cannot be negative. Please correct.",
  "qa.remaining.perc.entered", "The Remaining Allocation to be Incurred section (Cols 38 to 51) must be completed for this project.",
  "qa.savings.less.than.remaining", "Total Remaining Allocations after Savings (Col 37) cannot be negative. Please correct.",
  "qa.anticipated.atp.is.in.future", str_c("The Anticipated ATP Date (Col 10) should be in the future. If you received an ATP ",
                                           "approval for this project since September 2022, please enter the date it was received."),
  "qa.savings.tab.correct", "Project Savings (Col 36 on the MCP tab) must be equal to Total Project Savings (Col 12 on the Savings tab)."
)
# 
# if_na <- function (col, replacement) {
#   if_else(is.na(col), replacement, col)
# }
# m_if_na <- function (df, col, replacement) {
#   df.out<- df %>%
#     mutate({{col}} := if_else(is.na({{col}}),
#                               replacement,
#                               {{col}}))
# }

combined.2 %>%
  pivot_longer(
    cols = starts_with("qa."),
    names_to = "names",
    values_to = "value"
  ) %>%
  relocate(names, value) %>%
  filter (value == FALSE) %>%
  left_join(messages) %>%
  relocate(message, .after = 1) %>% 
  mutate(
    message = if_else (names == "qa.ori.date.not.earliest",
                       str_c(
                         "The original funding date (",
                         ori.date,
                         ") must precede all other dates (Cols 11 to 14)."),
                       message),
    message = str_c(project.id, ": ", message)
  ) %>% 
  bind_rows(MCP %>%
              select(matches("funding$|remaining|project.id")) %>%
              select(-matches("^other|^gross|total")) %>%
              reclass_n(cc.remaining, eo.remaining) %>%
              pivot_longer(cols = -project.id) %>%
              separate(name,
                       into = c("bucket", "initial.or.remaining"),
                       sep = "\\.") %>%
              pivot_wider(names_from = "initial.or.remaining",
                          values_from = "value") %>%
              mutate_a(~replace_na(.x, 0), funding, remaining) %>%
              mutate(difference = funding - remaining) %>%
              filter (round(difference) < 0) %>%
              mutate(message = str_c(project.id, ": The funding for ", str_to_upper(bucket), " must be greater or equal to than the remaining allocation")) %>%
              select(project.id, message) %>%
              left_join(MCP)) %>%
  select(-names, -value) %>%
  left_join(ca_list, by = "board.number") %>%
  relocate (ca.name) %>%
  arrange (ca.name, board.number) %>% 
  pipe_assign("out") %>%
  peek () %>%
  {gussy_up_excel(df = ., title = "Outstanding MCP Errors")} %>% 
  save_excel("Outstanding MCP Erros.xlsx")
  
save(MCP, file="MCP.RData")

out %>%
  #filter_in(message, "future") %>%
  #select (message, atp.date, atp.anticipated.date) %>%
  peek
  view
