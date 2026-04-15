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


make_na_row <- function(tbl) {
  tbl %>%
    slice(1) %>%
    mutate(across(everything(), ~ NA))
}

fix_null_listcols <- function(data, ...) {
  cols <- enquos(...)
  
  reduce(
    cols,
    .init = data,
    .f = function(df, colq) {
      col <- quo_name(colq)
      
      # find template
      template <- df[[col]] |>
        discard(is.null) |>
        pluck(1)
      
      na_template <- make_na_row(template)
      
      df %>%
        mutate({{ colq }} := map({{ colq }}, ~ if (is.null(.x))
          na_template
          else
            .x))
    }
  )
}
