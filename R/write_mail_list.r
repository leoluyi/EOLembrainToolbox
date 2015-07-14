write_mail_list <- function(ids, file_name) {
  
  new_file_name <- gsub(".txt$", paste0("_", Sys.Date(), ".txt", collapse=""), file_name)
  write.table(ids, file = new_file_name, quote=F, row.names=F, col.names=F)
  
  cat("-> mail list ¤w¶×¥X¦Ü", new_file_name, "\n\n")
}
