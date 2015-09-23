#' Count sent IDs from txt files
#'
#' @param sent_id_path Directory contains .txt files of sent IDs, 
#'                     of which file name contains date.
#' @param date_from Date begin.
#' @param date_to Date end.
#'
#'
#' @examples
#' n_sent_id("./exclude_id/", date_from = "2015-09-01", date_to = "2015-09-30")

#' @export
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
      cat(date_from, "\u81f3", date_to, "\u671f\u9593\u5171\u767c\u9001\u4e86\n", ., "\u500bID")
  } else {
    list.files(path = sent_id_path,
               recursive = TRUE,
               include.dirs = FALSE,
               full.names=TRUE) %>%  # 路徑
      plyr::llply(readr::read_table,
                  col_names = F,
                  col_types = "c") %>%
      unlist %>% unname %>% unique %>% length %>% 
      cat(sent_id_path, "\n\u8cc7\u6599\u593e\u4e2d\u5171\u767c\u9001\u4e86\n", ., "\u500bID\n")
  }
}
