read_pid <- function(n, path = "Pid上傳_.xls") {
  
  path <- check_file(path)
  ext <- tolower(tools::file_ext(path))
  
  aa <- paste0("P", formatC(sample(300000, n, replace = FALSE), 
                      width = 11, format = "d", flag = "0"))
  
  print("請用「外部資料匯入」=>「外部調查連結匯入」上傳Pid")
}

write_pid
