#' Read IDs from single txt file path, or txt files in directory
#'
#' @param path txt file path, or directory contains txt files
#'
#' @return character vector.
#'

#' @export
read_ids <- function(path) {
  
  path <- check_dir_file(path)
  
  switch(path_format(path),
         txt =  read_ids_txt(path),
         dir_path = read_ids_path(path)
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



