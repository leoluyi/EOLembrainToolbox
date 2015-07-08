gen_fake_id <- function(ids, path, key=c("p", "j"), .survey_id=NULL, .outurl=NULL, .etc1=NA) {
  
  key <- match.arg(key)
  
  if(is.null(ids)) return()
  
  ## check if file extention is ".xlsx"
  ext <- tolower(tools::file_ext(path))
  if(!identical(ext, "xlsx")) stop("`path` must be .xlsx file", call. = FALSE)
  
  old_pid <- NULL
  
  
  ## get path base string
  file_match_str <- tools::file_path_sans_ext(basename(path))
  old_file_name <- list.files("./", paste0("^", file_match_str, ".*\\.xlsx$"))
  
  ## check old file exists
  if(!length(old_file_name)==0) {
    original_file_exists <- file.exists(old_file_name[[1]])
  } else original_file_exists <- FALSE
  
  
  if(original_file_exists) {
    old_data <- readxl::read_excel(old_file_name, 
                                   col_types = c("numeric",rep("text", 3)))
    cat("從", old_file_name, "匯入", nrow(old_data), "筆舊pid\n\n")
    
    old_pid <- stringr::str_extract(old_data$outurl,
                                    "[^=]+$")  # str after last "="
    
    if(is.null(.outurl)) {
      .outurl <- stringr::str_extract(old_data$outurl,
                                      "^(.*[=])")[[1]]  # str before last "="
    }
    if(is.null(.survey_id)) .survey_id <- old_data$survey_id[[1]]
    
    ids <- setdiff(ids, old_data$panel_id) # exclude existed panel_id
  } 
  
  if(is.null(.survey_id)) stop("`.survey_id` must not be NULL", call. = FALSE)
  if(is.null(.outurl)) stop("`.outurl` must not be NULL", call. = FALSE)
  
  n <- length(ids)
  new_pid <- NULL
  n_duplicated_id <- n
  
  if(n_duplicated_id != 0) {
    repeat {
      new_pid <- c(setdiff(new_pid, old_pid),
                   paste0(toupper(key), stringr::str_pad(sample(500000, n_duplicated_id, replace = FALSE), 
                                                11, pad = "0")))
      n_duplicated_id <- length(intersect(new_pid, old_pid))
      if(n_duplicated_id == 0) break
    }
    
    df <- data.frame(survey_id = as.numeric(.survey_id), 
                     panel_id = as.character(ids), 
                     outurl = paste0(.outurl, new_pid),
                     etc1 = .etc1, stringsAsFactors=F)
    if(original_file_exists) {
      df <- as.data.frame(dplyr::bind_rows(old_data, df), stringsAsFactors=F)
    }
  } 
  else df <- old_data
  
  time_stamp <- strftime(Sys.time(), format = "%Y-%M-%d-%H%M%S")  # time stamp for file name
  new_file_name <- paste0(file_match_str, "_",time_stamp, ".xlsx")
  
  ## write to excel file
  openxlsx::write.xlsx(df, new_file_name, sheetName="sheet1")
  
  ## create log file
  dir.create("./pid_log", showWarnings = FALSE)
  df_log <- data.frame(survey_id = as.numeric(.survey_id), 
                       panel_id = as.character(ids), 
                       pid = new_pid)
  write.table(df_log,
              file = file.path("./pid_log", paste0("log_",
                                                   file_match_str, "_",
                                                   time_stamp, ".log")),
              quote = FALSE, row.names = FALSE, col.names = TRUE
  )
  
  
  cat("pid已匯出至 ", new_file_name, "\n",
      "共匯出", length(df$panel_id)-n, "筆舊pid，", n, "筆新pid，",
      "檔案中共包含", length(df$panel_id), "筆id\n",
      "(請用excel'另存'成.xls檔 =>「外部調查連結匯入」=> 上傳pid)\n\n")
  
  ## remove old file
  if(file.exists(new_file_name) & original_file_exists)
    unlink(old_file_name) 
}

