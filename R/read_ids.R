#' @useDynLib EOLembrainToolbox
NULL

#' Read single column txt files.
#'
#' @param path Path or file name to the txt file
#' @param sheet Sheet to read. Either a string (the name of a sheet), or
#'   an integer (the position of the sheet). Defaults to the first sheet.
#' @param col_names Either \code{TRUE} to use the first row as column names,
#'   \code{FALSE} to number columns sequentially from \code{X1} to \code{Xn},
#'   or a character vector giving a name for each column.
#' @param col_types Either \code{NULL} to guess from the spreadsheet or a
#'   character vector containing "blank", "numeric", "date" or "text".
#' @param na Missing value. By default readxl converts blank cells to missing
#'   data. Set this value if you have used a sentinel value for missing values.
#' @param skip Number of rows to skip before reading any data.
#' @export
#' @examples
#' # Specific sheet either by position or by name
#' read_excel(datasets, 2)
#' read_excel(datasets, "mtcars")


read_ids <- function(path) {
  
  path <- check_dir_file(path)
  
  switch(path_format(path),
         xls =  read_ids_txt(path, sheet, col_names, col_types, na, skip),
         xlsx = read_ids_path(path, sheet, col_names, col_types, na, skip)
  )
  
}

read_ids_txt <- function(path) {
  ids <- readLines(path, skipNul = TRUE)
  ids <- unname(unlist(ids))
  ids
}

read_ids_path <- function(path) {
  file_path <- list.files(path = path, 
                          pattern = "\\.txt$",
                          recursive = TRUE,
                          include.dirs = FALSE,
                          full.names = TRUE)  # list of file path
  
  ids <- plyr::llply(file_path, readLines,skipNul = TRUE)
  ids <- unname(unlist(ids))
  ids
}


# Helper functions -------------------------------------------------------------

path_format <- function(path){
  ext <- tolower(tools::file_ext(path))
  if (ext == "" & is_dir(path)) ext <- "dir_path"
  
  switch(ext,
         txt = "txt",
         dir_path = "dir_path",
         stop("Unknown format .", ext, call. = FALSE)
  )
}









