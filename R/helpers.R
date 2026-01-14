path_rda <- function(){
  getOption("databased.path") %>%
    confirm_slash() %>%
    stringr::str_c("rda/")
}

create_path_data <- function (dataset_name) {
  stringr::str_c(path_rda(), dataset_name, ".rda")
}

confirm_slash <- function(path) {
  if (!stringr::str_detect(path, "/$")) {
    path <- stringr::str_c(path, "/")
  }
  path
}


load_data <- function(data_name, extension = ".rda") {
  file_path <- create_path_data (data_name)
  
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  loaded_names <- load(file_path)
  if (!data_name %in% loaded_names) {
    stop("Data object '", data_name, "' not found in the loaded file.")
  }
  
  return(get(data_name))
}

add_na_column <- function(df, column_name) {
  df %>%
    mutate(!!rlang::sym(column_name) := NA_character_)
}

