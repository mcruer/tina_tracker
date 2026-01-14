

combined.2 <- MCP %>%
  left_join(pt %>% select(project.id, ori.date)) %>%
  relocate (ori.date, .before = atp.date) %>%
  mutate(
    qa.ori.date.not.earliest = if_all(ends_with("date"), ~ori.date<=.x | is.na(.x)
    ),
    .before = 1
  ) %>%
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


