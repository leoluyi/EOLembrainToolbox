#' write_mail_list
#'
#' @param ids 
#' @param file_name 
#'
#' @return txt file
#'
#' @examples
#' \dontrun{
#' id_output %>% write_mail_list("xxxx_sent_id.txt")
#' }
#' @export 
write_mail_list <- function(ids, file_name) {
  
  new_file_name <- gsub(".txt$", paste0("_", Sys.Date(), ".txt", collapse=""), file_name)
  write.table(ids, file = new_file_name, quote=F, row.names=F, col.names=F)
  
  cat("-> mail list \u5df2\u532f\u51fa\u81f3", new_file_name, "\n\n")
  
  invisible()
}
