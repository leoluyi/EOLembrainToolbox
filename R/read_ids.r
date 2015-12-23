#' Read IDs from single txt file path, or txt files in directory
#'
#' @param path txt file path, or directory contains txt files.
#' @param pattern Character string containing a regular expression 
#'        to be matched in the given character vector. Only work for dir path.
#' @param show_path Print file path or not. Only work for dir path.
#'
#' @return character vector.
#'

#' @export
read_ids <- function(path, pattern = ".*", show_path = FALSE) {
  path <- check_dir_file(path)
  
  switch(path_format(path),
         txt =  read_ids_txt(path),
         dir_path = read_ids_path(path, pattern, show_path))
}

read_ids_txt <- function(path) {
  file_path <- normalizePath(path,
                             winslash = "/",
                             mustWork = TRUE)
  
  ids <- readLines(path, skipNul = TRUE)
  ids <- unname(unlist(ids))
  ids
}

read_ids_path <- function(path, pattern, show_path) {
  file_path <- list.files(
    path = path,
    pattern = "\\.txt$",
    recursive = TRUE,
    include.dirs = FALSE,
    full.names = TRUE
  )  # list of file path
  
  path_dir <- dirname(file_path)
  path_file <- grep(pattern, basename(file_path), value = TRUE)
  
  file_path <- normalizePath(file.path(path_dir, path_file),
                             winslash = "/",
                             mustWork = TRUE)
  
  if (show_path) {
    cat("read files from:", file_path, "\n", sep = "\n")
  }
  
  ids <- plyr::llply(file_path, readLines, skipNul = TRUE)
  ids <- unlist(ids, use.names = FALSE)
  invisible(ids)
}


# Helper functions -------------------------------------------------------------

path_format <- function (path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "" && is_dir(path))
    ext <- "dir_path"
  
  switch (ext,
          txt = "txt",
          dir_path = "dir_path",
          stop ("Unknown format .", ext, call. = FALSE))
}
