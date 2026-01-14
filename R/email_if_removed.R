
make_removed_project_html <- function(
    project_id,
    record_status,
    project_name,
    board_number,
    board_name,
    analyst
){
  
  # Outlook-friendly emoji (HTML entities)
  emoji_wave     <- "&#x1F44B;"    # 👋
  emoji_warning  <- "&#x26A0;"     # ⚠️
  
  # Replace smart quotes/dashes with HTML entities
  clean_text <- function(x) {
    x |>
      stringr::str_replace_all("’", "&rsquo;") |>
      stringr::str_replace_all("‘", "&lsquo;") |>
      stringr::str_replace_all("“", "&ldquo;") |>
      stringr::str_replace_all("”", "&rdquo;") |>
      stringr::str_replace_all("–", "&ndash;") |>
      stringr::str_replace_all("—", "&mdash;")
  }
  
  # Build the table as a tibble
  df <- tibble::tibble(
    `Project ID`   = project_id,
    Board          = board_number,
    `Board Name`   = board_name,
    `Project Name` = project_name,
    `Record Status` = record_status
  )
  
  # Construct the HTML table
  table_html <- df %>%
    knitr::kable(
      format = "html",
      table.attr = 'border="1" style="border-collapse:collapse;width:100%;margin-bottom:16px;border-color:#c0c0c0;"'
    ) %>%
    stringr::str_replace_all('style="text-align:[^"]+"', "") %>%
    stringr::str_replace_all(
      "(?<=<th|<td)",
      ' style="padding:6px;text-align:left;border:1px solid #c0c0c0;"'
    )
  
  # Assemble the email body
  html <- glue::glue('
    <html>
      <body style="font-family: Calibri, sans-serif; font-size: 11pt; color:#222;">

        <p>Hi {analyst} {emoji_wave},</p>

        <p>
          The project below has been marked for <b>removal from the Monthly Tracker</b>.
        </p>

        <h3 style="margin-bottom:6px;color:#0a6fa4;">Project Details</h3>

        {table_html}

        <p style="margin-top:12px;">
          {emoji_warning}
          If this update was made in error, or if the project should remain active in the Monthly Tracker,
          please contact the <b>TINA Data Team</b> as soon as possible so we can correct the status.
        </p>

        <p style="margin-top:12px;">
          Thanks,<br>
          <b style="color:#0a6fa4;">TINA</b><br>
          <span style="color:#555;">Your CProg Data System</span><br>
          <i style="color:#777;">Total Information Needed ASAP</i>
        </p>

      </body>
    </html>
  ') |> clean_text()
  
  html
}

send_removed_project_email <- function(
    project_id,
    record_status,
    project_name,
    board_number,
    board_name,
    analyst,
    to,
    cc = NULL,
    test = FALSE
){
  
  html <- make_removed_project_html(
    project_id,
    record_status,
    project_name,
    board_number,
    board_name,
    analyst
  )
  
  subject <- glue::glue("{project_id}: {project_name} has been removed from the Monthly Tracker") %>%
    as.character()

  if (test) {
    email::send(
      to = "geordie.mcruer@ontario.ca",
      from = "CapitalProgramData@ontario.ca",
      cc = c("geordie.mcruer@ontario.ca","CapitalProgramData@ontario.ca"),
      subject = subject,
      html_body = html
    )
  } else {
    email::send(
      to = to,
      from = "CapitalProgramData@ontario.ca",
      cc = cc,
      subject = subject,
      html_body = html
    )
  }
}

email_if_removed <- function(new_monthly, test){
  boards <- ezql_table("boards")
  contacts <- ezql_table("contacts")
  pt <- ezql_table("projects")
  monthly <- monthly()
  
  newly_removed <- new_monthly %>%
    left_join(monthly %>% 
                select(
                  project_id,
                  previous_record_status = record_status
                )) %>%
    filter_in(record_status, "Remove from tracker", na.rm = TRUE) %>%
    filter_out(previous_record_status, "Remove from tracker") %>%
    select(project_id, record_status)

  manager_and_director_emails <- contacts %>%
    filter_in(role, "manager|director") %>%
    pull(email)
  
  newly_removed %>%
    left_join(pt) %>%
    left_join(boards %>% select(board_number, analyst)) %>%
    left_join(contacts %>% rename(analyst = name)) %>%
    select(project_id, record_status, project_name, board_number, board_name, analyst, to = email) %>%
    mutate(cc = list(manager_and_director_emails)) %>%
    pwalk(send_removed_project_email, test = test)
}




