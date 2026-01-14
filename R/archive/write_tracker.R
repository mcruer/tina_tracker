
source("R/functions.R")

monthly <- monthly()

pt <- CProg2::pt()

pt_data_to_import <- pt %>% 
  mutate(funding_other = funding_total - (funding_lp + funding_cp + funding_fdk +
                                            funding_cc + funding_eo + funding_chr)) %>%
  select(project_id, 
         board_number,
         project_type,
         project_category,
         date_ori,
         date_atp,
         funding_lp,
         funding_cp,
         funding_fdk,
         funding_cc,
         funding_eo,
         funding_chr,
         funding_other,
         funding_total
  ) %>%
  to_number(board_number) %>%
  left_join(load_data("board_allocations") %>% 
              filter_out_na(board_number)
            ) %>%
  select(-analyst)

monthly %>%
  filter_out(construction_status, "Cancelled|Complete") %>%
  left_join(pt_data_to_import) %>%
  to_number(board_number) %>%
  mutate(board_number_name = str_c(board_number, board_name, sep = " - ")) %>%
  nest(df = -board_number_name) %>%
  mutate (
    formatting.template.path = here2 ("data", "Monthly Tracker Template.xlsx"),
    data.template.path = here2 ("data", "workbook_template.xlsx"),
    output.file.name = str_c(path, board_number_name, " - Monthly Tracker.xlsx"),
    unique_identifier = exprs(project_id)
  ) %>%
  select(-board_number_name) %>%
  pwalk(excelr8,
  .progress = "text")
