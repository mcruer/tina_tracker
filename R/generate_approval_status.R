
#Check for Duplicate Events on the same day:

# events %>%
#   semi_join(included_ids) %>%
#   filter_in(event_type, "^ATP$|pre-tender|post-tender") %>%
#   group_by(project_id) %>%
#   filter(event_type != "ATP" | event_date == max(event_date[event_type == "ATP"])) %>%
#   ungroup() %>%
#   mutate(event_type = str_remove(event_type, " \\(Within\\)")) %>%
#   
#   # Check for duplicates
#   group_by(project_id, event_type) %>%
#   filter(n() > 1) %>%
#   ungroup() %>%
#   left_join(frank %>% select(project_id, construction_status)) %>%
#   relocate(date_ori, construction_status) %>%
#   view()



generate_approval_status <- function (events, included_ids){
  
  classify_approval <- function(has_date, has_other, has_atp, in_construction, date_val, within_budget) {
    case_when(
      has_date & within_budget  ~ paste0(as.character(date_val), " (Within Budget)"),
      has_date & !within_budget ~ paste0(as.character(date_val), " (Additional Funding)"),
      !has_date & has_atp       ~ "Not Necessary",
      !has_date & !has_atp & in_construction  ~ "MISSING",
      !has_date & !has_atp & !in_construction ~ "Not Yet Approved",
      .default = "Not Yet Approved"
    )
  }
  
  approval_status <- included_ids %>%
    left_join(
    events %>%
    filter_in(event_type, "^ATP$|pre-tender|post-tender") %>%
    group_by(project_id) %>%
    filter(event_type != "ATP" | event_date == max(event_date[event_type == "ATP"], na.rm = TRUE)) %>%
    ungroup()) %>%
    mutate(
      within_budget = str_detect(event_type, "Within"),
      event_type    = str_remove(event_type, " \\(Within\\)")
    ) %>%
    group_by(project_id, event_type) %>%
    slice_max(event_date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(project_id, event_type, event_date, within_budget) %>%
    pivot_wider(
      names_from  = event_type,
      values_from = c(event_date, within_budget)
    ) %>%
    rename(
      date_atp                  = `event_date_ATP`,
      date_pretender_approval   = `event_date_Pre-Tender Budget`,
      date_posttender_approval  = `event_date_Post-Tender Budget`,
      within_budget_atp         = `within_budget_ATP`,
      within_budget_pretender   = `within_budget_Pre-Tender Budget`,
      within_budget_posttender  = `within_budget_Post-Tender Budget`
    ) %>%
    left_join(monthly %>% select(project_id, date_construction)) %>%
    mutate(
      in_construction = !is.na(date_construction) & date_construction < today(),
      pretender_status = classify_approval(
        has_date        = !is.na(date_pretender_approval),
        has_other       = !is.na(date_posttender_approval),
        has_atp         = !is.na(date_atp),
        in_construction = in_construction,
        date_val        = date_pretender_approval,
        within_budget   = coalesce(within_budget_pretender, FALSE)
      ),
      posttender_status = classify_approval(
        has_date        = !is.na(date_posttender_approval),
        has_other       = !is.na(date_pretender_approval),
        has_atp         = !is.na(date_atp),
        in_construction = in_construction,
        date_val        = date_posttender_approval,
        within_budget   = coalesce(within_budget_posttender, FALSE)
      )
    ) %>%
    select(project_id, pre_tender_complete = pretender_status, post_tender_complete = posttender_status)
  
  return(approval_status)
  
  }
  
