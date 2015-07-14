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
