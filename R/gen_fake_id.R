gen_fake_id <- function(ids, path, key=c("p", "j"), .survey_id=NULL, .outurl=NULL, .etc1=NA) {
  
  if (missing(key)) {
    warning("the fake-id key is not specified; by default, the key is 'p'")
    method = "p"
  }
  
  key <- match.arg(key)
  
  if(is.null(ids)) return()
  
  ## check if file extention is ".xlsx"
  ext <- tolower(tools::file_ext(path))
  if(!identical(ext, "xlsx")) stop("`path` must be .xlsx file", call. = FALSE)
  
  old_pid <- NULL
  
  
  ## get path base string
  file_match_str <- tools::file_path_sans_ext(basename(path))
  file_match <- list.files("./", paste0("^", file_match_str, ".*\\.xlsx$"))
  
  ## check old file exists
  if(length(file_match)!=0) {
    old_file_name <- file_match[[1]]
    original_file_exists <- file.exists(old_file_name)
  } else original_file_exists <- FALSE
  
  
  if(original_file_exists) {
    old_data <- readxl::read_excel(old_file_name, 
                                   col_types = c("numeric",rep("text", 3)))
    cat("從", normalizePath(old_file_name), "匯入", nrow(old_data), "筆舊pid\n\n")
    
    old_pid <- stringr::str_extract(old_data$outurl,
                                    "[^=]+$")  # str after last "="
    
    if(is.null(.outurl)) {
      .outurl <- stringr::str_extract(old_data$outurl,
                                      "^(.*[=])")[[1]]  # str before last "="
    }
    if(is.null(.survey_id)) .survey_id <- old_data$survey_id[[1]]
    
    new_panel_id <- setdiff(ids, old_data$panel_id) # exclude existed panel_id
    
    if(length(new_panel_id)==0) {
      cat("# `gen_fake_id`: 沒有新panel_id需要上傳pid\n\n")
      return()
    }
  } else {
    new_panel_id <- ids
  }
  
  if(is.null(.survey_id)) stop("`.survey_id` must not be NULL", call. = FALSE)
  if(is.null(.outurl)) stop("`.outurl` must not be NULL", call. = FALSE)
  
  n_new_panel_id <- length(new_panel_id)  
  n_duplicated_id <- n_new_panel_id
  new_pid <- NULL  # reserve space
  
  if(n_duplicated_id != 0) {
    repeat {
      new_pid <- c(setdiff(new_pid, old_pid),
                   paste0(toupper(key), stringr::str_pad(sample(500000, n_duplicated_id, replace = FALSE), 
                                                         11, pad = "0")))
      n_duplicated_id <- length(intersect(new_pid, old_pid))
      if(n_duplicated_id == 0) break
    }
    
    df <- data.frame(survey_id = as.numeric(.survey_id), 
                     panel_id = as.character(new_panel_id), 
                     outurl = paste0(.outurl, new_pid),
                     etc1 = .etc1, stringsAsFactors=F)
    if(original_file_exists) {
      df <- as.data.frame(dplyr::bind_rows(old_data, df), stringsAsFactors=F)
    }
  } else {
    df <- old_data
  }
  
  ## write to excel file
  time_stamp <- strftime(Sys.time(), format = "%Y-%m-%d-%H%M%S")  # time stamp for file name
  new_file_name <- paste0(file_match_str, "_",time_stamp, ".xlsx")
  openxlsx::write.xlsx(df, new_file_name, sheetName="sheet1")
  
  ## create log file
  dir.create("./pid_log", showWarnings = FALSE)
  df_log <- data.frame(survey_id = as.numeric(.survey_id), 
                       panel_id = as.character(new_panel_id), 
                       pid = new_pid)
  write.table(df_log,
              file = file.path("./pid_log", paste0("log_",
                                                   file_match_str, "_",
                                                   time_stamp, ".log")),
              quote = FALSE, row.names = FALSE, col.names = TRUE
  )
  
  
  cat("-> pid已匯出至 ", new_file_name, "\n",
      "共匯出", 
      if(original_file_exists) 
        length(old_data$panel_id), "筆舊pid，",
      n_new_panel_id, "筆新pid，",
      "檔案中共包含", length(df$panel_id), "筆id\n",
      "(請用excel'另存'成.xls檔 =>「外部調查連結匯入」=> 上傳pid)\n\n")
  

  if(file.exists(new_file_name) & original_file_exists) {
    ## copy file
    file.copy(from = old_file_name, 
              to = paste0("./pid_log/", basename(old_file_name), ".temp"))
    
    ## remove old file
    file.remove(old_file_name)
    log_xlsx_match <- list.files("./pid_log", 
                                 paste0("\\.backup$"), 
                                 full.names = TRUE)

    file.remove(log_xlsx_match)
    cat(log_xlsx_match, sep = "\n", "removed.\n")
  }
  
  ## rename
  file.rename(from = list.files("./pid_log", "\\.temp$", full.names = TRUE),
              to = gsub("\\.temp$", ".backup",
                        list.files("./pid_log", "\\.temp$", full.names = TRUE))
  )
}

