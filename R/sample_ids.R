sample_N <- function(.data, .n, .id_var="Panel_id",
                     sent_id = NULL,
                     finished_id = NULL,
                     include_sent = FALSE,
                     show = TRUE) {
  
  if(.n == 0) {
    if(show == TRUE) cat(paste0("(沒有抽樣)", "\n"))
    return()
  }
  
  if(length(finished_id) == 0 & include_sent == TRUE)
    stop("When `include_sent` set to TRUE, length of `finished_id` must not be 0.",
         call. = FALSE)
  
  if(include_sent == FALSE) {
    exclude_id <- c(sent_id, finished_id)
  } else exclude_id <- finished_id
  
  ## exclude id
  filter_criteria <- lazyeval::interp(~ ! id_var %in% exclude_id,
                                      id_var = as.name(.id_var))
  .data <- .data %>% filter_(filter_criteria)
  
  if(nrow(.data) == 0) {
    if(show == TRUE) cat(paste0("(此條件已無會員)", "\n"))
    return()
  }
  
  include_sent_info <- NULL   # 包含已發送訊息
  if(include_sent == TRUE) include_sent_info <- "(含已發送)"
  
  less_info <- NULL   # 缺額訊息
  if(.n > nrow(.data)) {
    .n <- nrow(.data)
    less_info <- "(缺額)"
  }
  
  sample_id <- .data %>% # exclude id
    dplyr::sample_n(size = .n, replace = FALSE) %>%
    dplyr::select_(.id_var) %>% unlist %>% unname
  
  if(show == TRUE) {
    cat(paste0(scales::comma(length(sample_id)),
               "\t個ID被抽出",
               include_sent_info,
               less_info,
               "\n"))
  }
  
  sample_id
}


n_sent_id <- function (sent_id_path, date_from=NULL, date_to=NULL) {
  
  sent_id_path <- suppressWarnings(normalizePath(sent_id_path))
  
  if(!is.null(date_from) & !is.null(date_to)) {
    time_interval <- seq(as.Date(date_from), as.Date(date_to), "day") %>%
      as.character %>%
      paste0(".*\\.txt", collapse="|")
    
    list.files(path = sent_id_path,
               pattern = time_interval,
               full.names=TRUE) %>%
      plyr::llply(readr::read_table,
                  col_names = F,
                  col_types = "c") %>%
      unlist %>% unname %>%  unique %>% length %>%
      cat(date_from, "至", date_to, "期間共發送了\n", ., "個ID")
  } else {
    list.files(path = sent_id_path,
               recursive = TRUE,
               include.dirs = FALSE,
               full.names=TRUE) %>%  # 路徑
      plyr::llply(readr::read_table,
                  col_names = F,
                  col_types = "c") %>%
      unlist %>% unname %>% length %>% unique %>%
      cat(sent_id_path, "\n資料夾中共發送了\n", ., "個ID\n")
  }
}


write_table_date <- function(ids, file_name) {
  
  new_file_name <- gsub(".txt$", paste0("_", Sys.Date(), ".txt", collapse=""), file_name)
  write.table(ids, file = new_file_name, quote=F, row.names=F, col.names=F)
  
  cat("-> mail list 已匯出至", new_file_name, "\n\n")
}


read_ids <- function(dir_path) {
  
  dir_path <- check_dir(dir_path)
  
  file_path <- list.files(path = dir_path, 
                          pattern = "\\.txt$",
                          recursive = TRUE,
                          include.dirs = FALSE,
                          full.names = TRUE)  # list of file path
  
  ids <- plyr::llply(file_path, readLines,skipNul = TRUE)
  ids <- unname(unlist(ids))
  ids
}

