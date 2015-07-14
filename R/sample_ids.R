sample_N <- function(.data, .n, .id_var="Panel_id",
                     sent_id = NULL,
                     finished_id = NULL,
                     include_sent = FALSE,
                     show = TRUE) {
  
  if(.n == 0) {
    if(show == TRUE) cat(paste0("(�S�����)", "\n"))
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
    if(show == TRUE) cat(paste0("(������w�L�|��)", "\n"))
    return()
  }
  
  include_sent_info <- NULL   # �]�t�w�o�e�T��
  if(include_sent == TRUE) include_sent_info <- "(�t�w�o�e)"
  
  less_info <- NULL   # ���B�T��
  if(.n > nrow(.data)) {
    .n <- nrow(.data)
    less_info <- "(���B)"
  }
  
  sample_id <- .data %>% # exclude id
    dplyr::sample_n(size = .n, replace = FALSE) %>%
    dplyr::select_(.id_var) %>% unlist %>% unname
  
  if(show == TRUE) {
    cat(paste0(scales::comma(length(sample_id)),
               "\t��ID�Q��X",
               include_sent_info,
               less_info,
               "\n"))
  }
  
  sample_id
}
