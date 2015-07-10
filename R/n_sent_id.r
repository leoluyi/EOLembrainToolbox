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
      cat(date_from, "��", date_to, "�����@�o�e�F\n", ., "��ID")
  } else {
    list.files(path = sent_id_path,
               recursive = TRUE,
               include.dirs = FALSE,
               full.names=TRUE) %>%  # ���|
      plyr::llply(readr::read_table,
                  col_names = F,
                  col_types = "c") %>%
      unlist %>% unname %>% unique %>% length %>% 
      cat(sent_id_path, "\n��Ƨ����@�o�e�F\n", ., "��ID\n")
  }
}
