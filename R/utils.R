check_file <- function(path) {
  if (!file.exists(path)) {
    stop("'", path, "' does not exist",
         if (!is_absolute_path(path))
           paste0(" in current working directory ('", getwd(), "')"),
         ".",
         call. = FALSE)
  }
  
  normalizePath(path, "/", mustWork = FALSE)
}



check_dir <- function(path) {
  if (!devtools:::dir.exists(path)) {
    stop("'", path, "' does not exist",
         if (!is_absolute_path(path))
           paste0(" in current working directory ('", getwd(), "')"),
         ".",
         call. = FALSE)
  }
  
  normalizePath(path, "/", mustWork = FALSE)
}


is_dir <- function(x) {
  path <- check_dir_file(x)
  file.info(path)$isdir
}


check_dir_file <- function(path) {
  
  path_temp <- normalizePath(path, "/", mustWork = FALSE)
  path <- gsub("/+$", "", path_temp)
  
  if (!file.exists(path) & !devtools:::dir.exists(path)) {
    stop("'", path, "' does not exist",
         if (!is_absolute_path(path))
           paste0(" in current working directory ('", getwd(), "')"),
         ".",
         call. = FALSE)
  }
  
  normalizePath(path, "/", mustWork = FALSE)
}


is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:|\\\\|~)", path)
}

isFALSE <- function(x) identical(x, FALSE)



#  ------------------------------------------------------------------------

# path <- check_file(path)
# ext <- tolower(tools::file_ext(path))
