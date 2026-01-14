library(magrittr)
here2 <- function (...){
  root <- stringr::str_c("C:/Users/McRuersG/OneDrive - Government of Ontario/Documents/",
                "R Projects/Analysis/Monthly Trackers/")
  args_vec <- list(...) %>%
    purrr::map(databased::confirm_slash) %>%
    unlist %>%
    stringr::str_c(collapse = "")
  
  stringr::str_c(root, args_vec) %>%
    stringr::str_remove("/$")

}

source(here2 ("R", "harvest_data.R"))
source(here2 ("R", "write_tracker.R"))
source(here2 ("R", "generate_reports.R"))

