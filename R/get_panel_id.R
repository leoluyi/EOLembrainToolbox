read_panel_id <- function(
  path = "https://github.com/leoluyi/EOLembrainToolbox/raw/master/panel_data/panel_2015-06-08.csv")
{
  
  # check extension: csv
  if(tolower(tools::file_ext(path)) != "csv") {
    stop("The extention of panel id file must be .csv", call. = FALSE)
  }
  
  ## read original panel file
  panel <- readr::read_csv(path,
                           col_names = TRUE,
                           col_types = paste0(rep("c", 8), collapse = ""),
                           na = "")
  
  panel[,c(2, 4:length(panel))] <-
    lapply(panel[c(2, 4:length(panel))], as.factor) %>%
    as_data_frame
  
  panel <- panel %>%
    mutate(age = as.numeric(age)) %>%
    mutate(aream = as.numeric(aream)) %>%
    mutate(aream_name = factor(aream_name))
  
  #   area_table <- panel %>%
  #     select(aream, aream_name) %>%
  #     distinct %>%
  #     arrange(aream)
  
  ## Changing the order of levels of a factor
  
  levels(panel$aream_name) <-
    list(
      "基隆市" = "基隆市",
      "臺北市" = "臺北市",
      "新北市" = "新北市",
      "桃園市" = "桃園縣",
      "新竹縣" = "新竹縣",
      "新竹市" = "新竹市",
      "苗栗縣" = "苗栗縣",
      "臺中市" = "臺中市(原臺中市)",
      "臺中市" = "臺中市(原臺中縣)",
      "彰化縣" = "彰化縣",
      "南投縣" = "南投縣",
      "雲林縣" = "雲林縣",
      "嘉義縣" = "嘉義縣",
      "嘉義市" = "嘉義市",
      "臺南市" = "臺南市(原臺南市)",
      "臺南市" = "臺南市(原臺南縣)",
      "高雄市" = "高雄市(原高雄縣)",
      "高雄市" = "高雄市(原高雄市)",
      "屏東縣" = "屏東縣",
      "宜蘭縣" = "宜蘭縣",
      "花蓮縣" = "花蓮縣",
      "臺東縣" = "臺東縣",
      "澎湖縣" = "澎湖縣",
      "金門縣" = "金門縣",
      "連江縣" = "連江縣",
      "南海諸島" = "南海諸島",
      "釣魚臺列嶼" = "釣魚臺列嶼")
  
  panel <- panel %>%
    mutate(aream = as.numeric(aream_name))
  
  panel
}




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
  
  sent_id_path <- normalizePath(sent_id_path)
  
  if(!is.null(date_from) & !is.null(date_to)) {
    time_interval <- seq(as.Date(date_from), as.Date(date_to), "day") %>%
      as.character %>%
      paste0(collapse="|")
    
    list.files(path = sent_id_path,
               pattern = time_interval, full.names=TRUE) %>%
      plyr::llply(readr::read_table,
                  col_names = F,
                  col_types = "c") %>%
      unlist %>% unname %>% length %>% unique %>%
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
  
  new_name <- gsub(".txt$", paste0("_", Sys.Date(), ".txt", collapse=""), file_name)
  write.table(ids, file = new_name, quote=F, row.names=F, col.names=F)
  
  cat("mail list 已匯出至\n", normalizePath(new_name), "\n")
}


read_ids <- function(dir_path) {
  
  dir_path <- check_dir(dir_path)
  
  file_path <- list.files(path = dir_path, 
             recursive = TRUE,
             include.dirs = FALSE,
             full.names = TRUE)  # list of file path
  
  ids <- plyr::llply(file_path, readLines,skipNul = TRUE)
  
  ids <- unname(unlist(ids))
}




