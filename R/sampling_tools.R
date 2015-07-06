## -- update package ----
if(packageDescription("openxlsx")$Version < "3.0.0") {
  devtools::install_github("awalker89/openxlsx")
}


as.strataCrossN <- function(.strataPctList, .namesList, N){
  
  ## error handler
  if (length(.strataPctList) != length(.namesList)) 
    stop("`strataPctList`和`namesList`長度不一致")
  if (is.list(.strataPctList) != TRUE) stop(".strataPctList must have class \"list\"")
  if (is.list(.namesList) != TRUE) stop(".strataPctList must have class \"list\"")
  
  for (i in seq_along(strataPctList)){
    if (i ==1) {strataCrossArr <- strataPctList[[1]]}
    else {strataCrossArr <- outer(strataCrossArr, strataPctList[[i]])}
  }
  strataCrossArr <- as.array(strataCrossArr)
  dimnames(strataCrossArr) <- .namesList
  
  ## 抽樣陣列
  N <- as.integer(N)
  strataCrossN <- round(strataCrossArr*N, 0); # sum(strataCrossN) # 總數
  ## 平面化
  strataCrossN_flat = as.data.frame.table(strataCrossN)
  ## 或用{plyr}做成平面化
  # library(plyr)
  # adply(strataCrossN, c(1,2,3))
  return(strataCrossN_flat)
}



margin_prop <- function(.strataCrossN_flat,
                        export = FALSE,
                        outputfile="R實抽樣本temp.csv")
{
  
  ## 實抽樣本expanded
  strataCrossN.expanded <- .strataCrossN_flat[
    rep(row.names(.strataCrossN_flat), .strataCrossN_flat$Freq), 
    1:length(.strataCrossN_flat)]
  
  strataCrossN.expanded <- strataCrossN.expanded[-length(strataCrossN.expanded)]
  
  Ndim <- length(strataCrossN.expanded)
  總抽樣本 <- margin.table(table(strataCrossN.expanded)) # Sum total
  
  ## 建立list
  mar_tab <- function(x) {
    prop.table(table(x, deparse.level = 0))
  }
  
  tab <- function(x) table(x, deparse.level = 0)
  
  probList <- c(總樣本數 = 總抽樣本, 
                    lapply(strataCrossN.expanded, FUN = mar_tab))
  
  freqList <- lapply(strataCrossN.expanded, FUN = tab)
  print(freqList)
  cat(paste0(rep("-", 40), collapse = ""), "\n")
  
  ## 實抽樣本寫入txt
  
  #cat(.strataCrossN_flat$Freq, file = "實抽樣本vector.txt", sep = "\n")
  if(export) {
    write.csv(.strataCrossN_flat, file = outputfile, row.names=FALSE)
  }
  
  probList <- c(probList[1], lapply(probList[-1], format, digits=3, nsmall=3))
  
  probList
}



renew_sampleN <- function (.strataCrossN_flat, outputfile="R實抽樣本temp.csv") {
  
  ## 實抽樣本從txt重讀
  # 實抽樣本 <- as.numeric(readLines("實抽樣vector本.txt"))
  實抽樣本 <- read.csv(file = outputfile, header=TRUE)$Freq
  .strataCrossN_flat$Freq <- 實抽樣本  # 更新
  ## 實抽樣本寫入txt
  #cat(.strataCrossN_flat$Freq, file = "實抽樣本vector.txt", sep = "\n")
  write.csv(.strataCrossN_flat, file = outputfile, row.names=FALSE)
  return(.strataCrossN_flat)
}


# ## 讀取raw data
# read.data.xlsx3 <- function (.fileName, .sheetIndex=1, .colIndex=NULL, 
#                              .colClasses="character", encoding="unknown")
# {
#   library(openxlsx)
#   .data <- openxlsx::read.xlsx(
#     xlsxFile = .fileName,
#     sheet = .sheetIndex,
#     colNames = T
#   )
#   return(.data)
# }


# ## 讀取raw data
# read.data.xlsx2 <- function (.fileName, .sheetIndex=1, .colIndex=NULL, 
#                              .colClasses="character", encoding="unknown")
# {
#   jgc <- function(){
#     .jcall("java/lang/System", method = "gc")
#   }  
#   
#   jgc()
#   
#   options(java.parameters = "-Xmx4g") # read.xlsx增加記憶體
#   
#   .data <- xlsx::read.xlsx2(
#     file = .fileName,
#     sheetIndex = .sheetIndex,
#     header = T,
#     colClasses = .colClasses,
#     colIndex = .colIndex,  # 只擷取前十欄
#     stringsAsFactors = FALSE
#   )
#   return(.data)
# }


# ## 讀取raw data
# read.data.xlsx <- function (.fileName, .sheetIndex=1, .colIndex=NULL,
#                             .colClasses="character", encoding="unknown")
# {
#   jgc <- function(){
#     .jcall("java/lang/System", method = "gc")
#   }
#   
#   jgc()
#   
#   .data <- xlsx::read.xlsx(
#     file = .fileName,
#     sheetIndex = .sheetIndex,
#     header = T,
#     colClasses = .colClasses,
#     colIndex = .colIndex,  # 只擷取前十欄
#     encoding = encoding,
#     stringsAsFactors = FALSE
#   )
#   return(.data)
# }


## sample compare
sampleC <- function (.data,
                     .strataCrossN_flat,
                     .vars,
                     .namesList) {
  
  # 防呆
  if (length(.vars) != length(.namesList))
    stop("length(.vars) must equal to length(.namesList)")
  if (is.data.frame(.data) != TRUE) 
    stop(".data must have class \"data.frame\"")
  
  ID_var_name <- names(.data)[grep("ID$", names(.data), ignore.case = TRUE)][[1]]
  if(length(ID_var_name)!=0)
    if(anyDuplicated(.data[[ID_var_name]])) {
      stop(c("ID有重複: ", "\n",
             paste0(unique(
               .data[[ID_var_name]][duplicated(.data[[ID_var_name]])]), 
               collapse = "\n")))
    }
  
  
  var_names <- names(.namesList)
  
  # 排除不在list的樣本
  for(i in seq_along(.namesList)){
    filter_criteria <- lazyeval::interp(~ which_column %in% .namesList[[i]], 
                                        which_column = as.name(.vars[i]))
    
    .data <- .data %>% filter_(filter_criteria)
  }
  
  
  # .data check levels
  for(i in seq_along(.namesList)){
    .data[[.vars[i]]] <- .data[[.vars[i]]] %>%
      factor(levels=.namesList[[i]])
  }
  
  # .strataCrossN_flat check levels
  for(i in seq_along(.namesList)){
    .strataCrossN_flat[[var_names[i]]] <- .strataCrossN_flat[[var_names[i]]] %>%
      factor(levels=.namesList[[i]])
  }
  
  
  # .strataCrossN_flat 排序
  .strataCrossN_flat <- .strataCrossN_flat[c(var_names, "Freq")]
  .strataCrossN_flat <- .strataCrossN_flat %>%
    arrange_(.dots = rev(names(.strataCrossN_flat)[-length(.strataCrossN_flat)]))
  
  # 樣本分佈
  sampleCross_flat <- table(
    .data[.vars]) %>% # (Q1R..為excel檔表頭變數名)
    as.data.frame.table()
  
  # sampleCross_flat 排序
  sampleCross_flat <- sampleCross_flat %>% arrange_(.dots = rev(.vars))
  
  # 檢查多寡
  ncolStrata = ncol(.strataCrossN_flat)
  ncolSample = ncol(sampleCross_flat)
  
  n1 <- sampleCross_flat[["Freq"]]
  n2 <- .strataCrossN_flat[["Freq"]]
  
  # 防呆
  if (length(n1) != length(n2)) stop("樣本組合與分層數不相等")
  
  strataCompare <-
    cbind(.strataCrossN_flat[!(names(.strataCrossN_flat) %in% c("Freq"))], 
          應抽樣本數 = n2,
          Completed_N = n1,
          樣本多寡 = n1 - n2
    )
  return(strataCompare)
}


read.strata <- function(strata_file, .namesList)
{
  .strataCrossN_flat <- read.csv(strata_file, header = TRUE, 
                                 na.strings = "", stringsAsFactors=F)
  
  var_names <- names(.namesList)
  
  for(i in seq_along(.namesList)){
    .strataCrossN_flat[[var_names[i]]] <- .strataCrossN_flat[[var_names[i]]] %>%
      factor(levels=.namesList[[i]])
  } 
  
  .strataCrossN_flat
}


strata.sample.ID <- function (.data, .strataCrossN_flat, .namesList, 
                              .vars, ID_var_name="Panel.ID", 
                              method=c("srswor","srswr","poisson","systematic"), 
                              pik=NULL)
{
  ## Possible error:
  # Error in data.frame(..., check.names = FALSE) : 
  #   arguments imply differing number of rows: 0, 1
  #
  # The problem lies in your trying to set the sample size from
  # some groups equal to zero. Instead, subset your original data before sampling.
  # 
  # data.reordered <- subset(data.reordered, !(Q1R==1 && Q2R==2 && Q3R==4) &&
  #                            !(Q1R==2 && Q2R==2 && Q3R==4))
  # 實抽樣本 <- 實抽樣本[實抽樣本!=0]
  
  ID_var_name <- ID_var_name[[1]]
  
  if(anyDuplicated(.data[[ID_var_name]])) {
    stop(c("抽取之ID有重複: ", "\n",
           paste0(.data[[ID_var_name]][duplicated(.data[[ID_var_name]])], 
                  collapse = "\n")))
  }
  
  var_names <- names(.namesList)
  
  # .strataCrossN_flat check levels
  for(i in seq_along(.namesList)){
    .strataCrossN_flat[[var_names[i]]] <- .strataCrossN_flat[[var_names[i]]] %>%
      factor(levels=.namesList[[i]])
  }
  
  # .strataCrossN_flat 排序
  .strataCrossN_flat <- .strataCrossN_flat[,c(var_names, "Freq")]
  .strataCrossN_flat <- .strataCrossN_flat %>%
    arrange_(.dots = rev(names(.strataCrossN_flat)[-length(.strataCrossN_flat)]))
  
  # .strataCrossN_flat 移除Freq == 0
  .strataCrossN_flat <- .strataCrossN_flat %>%
    filter(!(Freq == 0))
  
  N_vector <- .strataCrossN_flat$Freq
  
  # .data排除重複id
  .data <- .data %>% distinct_(ID_var_name)
  
  # .data排除不在list的樣本
  for(i in seq_along(.namesList)){
    filter_criteria <- lazyeval::interp(~ which_column %in% .namesList[[i]], 
                                        which_column = as.name(.vars[i]))
    
    .data <- .data %>% filter_(filter_criteria)
  }
  
  # .data check levels
  for(i in seq_along(.namesList)){
    .data[[.vars[i]]] <- .data[[.vars[i]]] %>%
      factor(levels=.namesList[[i]])
  }
  
  # .data 排序 (strata抽樣樣本要先排序)
  data.reordered <- arrange_(.data, .dots = rev(.vars)) ## 以後面變數為第一排序層級
  
  # 抽出之ID
  ID_Output <- data.reordered %>%
    sampling::strata(stratanames=.vars, size=N_vector, 
           method, .data[[pik]]) %>%
    sampling::getdata(data.reordered,.) %>%
    `[[`(ID_var_name) %>% unlist %>% unname
  
  cat("共", length(ID_Output), "個ID被抽出\n")
  
  return(ID_Output)
}


write.output.xlsx3 <- function(ID_sample, .data, ID_var_name){
  
  # ID_var_name: ID所在變數名
  # ID_sample: 要抽取的ID
  
  # 防呆
  if(anyDuplicated(ID_sample)) {
    stop(c("抽取之ID有重複: ", "\n",
           paste0(ID_sample[duplicated(ID_sample)], 
                  collapse = "\n")))
  }
  
  # .data排除重複id
  .data <- .data %>% distinct_(ID_var_name)
  
  tempData <- .data[.data[[ID_var_name]] %in% ID_sample,]
  tempData_backup <- .data[!(.data[[ID_var_name]] %in% ID_sample),]
  
  outputfileName <- paste(tools::file_path_sans_ext(fileName),
                          "抽樣後raw_data.xlsx",sep = "_")
  
  # 轉為data.frame
  is_tbl <- inherits(.data, "tbl")
  if(!is.data.frame(.data)) {
    .data <- dplyr::as_data_frame(.data)
  } else if (is_tbl) {
    .data <- dplyr::tbl_df(.data)
  }
  
  wb <- openxlsx::createWorkbook(
    creator = paste0("EOLembrain", Sys.getenv("USERNAME"), sep="_"))
  openxlsx::addWorksheet(wb, paste("N=", length(unique(ID_sample)),sep=""))
  openxlsx::addWorksheet(wb, "backup")
  openxlsx::writeData(wb, paste("N=", length(unique(ID_sample)),sep=""), x = tempData,
                      rowNames = FALSE, keepNA=FALSE)
  
  openxlsx::writeData(wb, "backup", x = tempData_backup,
                      rowNames = FALSE, keepNA=FALSE)
  openxlsx::saveWorkbook(wb, file = outputfileName, overwrite = TRUE)
}


# write.output.xlsx2 <- function(ID_sample, .data, ID_var_name){
#   
#   # ID_var_name: ID所在變數名
#   # ID_sample: 要抽取的ID
#   
#   options(java.parameters = "-Xmx4g") # read.xlsx增加記憶體
#   
#   library(tools)
#   library(xlsx)
#   
#   jgc <- function(){
#     .jcall("java/lang/System", method = "gc")
#   }  
#   
#   jgc()
#   
#   # 防呆
#   if(anyDuplicated(ID_sample)) {
#     stop(c("抽取之ID有重複: ", "\n",
#            paste0(ID_sample[duplicated(ID_sample)], 
#                   collapse = "\n")))
#   }
#   
#   # .data排除重複id
#   .data <- .data %>% distinct_(ID_var_name)
#   
#   tempData <- .data[.data[[ID_var_name]] %in% ID_sample,]
#   tempData_backup <- .data[!(.data[[ID_var_name]] %in% ID_sample),]
#   
#   outputfileName <- paste(file_path_sans_ext(fileName),
#                           "抽樣後raw_data.xlsx",sep = "_")
#   write.xlsx2(x = tempData, file = outputfileName,
#               sheetName = paste("N=", length(unique(ID_sample)),sep=""), 
#               row.names = FALSE, showNA=FALSE)
#   write.xlsx2(x = tempData_backup, file = outputfileName, append=TRUE,
#               sheetName = "backup", 
#               row.names = FALSE, showNA=FALSE)
# }


# write.output.xlsx1 <- function(ID_sample, .data, ID_var_name){
#   
#   # ID_var_name: ID所在變數名
#   # ID_sample: 要抽取的ID
#   
#   library(tools)
#   library(xlsx)
#   
#   jgc <- function(){
#     .jcall("java/lang/System", method = "gc")
#   }  
#   
#   jgc()
#   
#   # 防呆
#   if(anyDuplicated(ID_sample)) {
#     stop(c("抽取之ID有重複: ", "\n",
#            paste0(ID_sample[duplicated(ID_sample)], 
#                   collapse = "\n")))
#   }
#   
#   # .data排除重複id
#   .data <- .data %>% distinct_(ID_var_name)
#   
#   tempData <- .data[.data[[ID_var_name]] %in% ID_sample,]
#   tempData_backup <- .data[!(.data[[ID_var_name]] %in% ID_sample),]
#   
#   outputfileName <- paste(file_path_sans_ext(fileName),
#                           "抽樣後raw_data.xlsx",sep = "_")
#   write.xlsx(x = tempData, file = outputfileName,
#              sheetName = paste("N=", length(unique(ID_sample)),sep=""), 
#              row.names = FALSE, showNA=FALSE)
#   write.xlsx(x = tempData_backup, file = outputfileName, append=TRUE,
#              sheetName = "backup", row.names = FALSE, showNA=FALSE)
# }

write.output.csv <- function(ID_sample, .data, ID_var_name){
  
  # ID_var_name: ID所在變數名
  # ID_sample: 要抽取的ID
  
  # 防呆
  if(anyDuplicated(ID_sample)) {
    stop(c("抽取之ID有重複: ", "\n",
           paste0(ID_sample[duplicated(ID_sample)], 
                  collapse = "\n")))
  }
  
  # .data排除重複id
  .data <- .data %>% distinct_(ID_var_name)
  
  tempData <- .data[.data[[ID_var_name]] %in% ID_sample,]
  tempData_backup <- .data[!(.data[[ID_var_name]] %in% ID_sample),]
  
  outputfileName <- paste(tools::file_path_sans_ext(fileName),
                          "抽樣後raw_data.csv",sep = "_")
  write.csv(x = tempData, file = outputfileName,
            quote = T, na = "", row.names=F)
  write.csv(x = tempData_backup, 
            file = paste0(tools::file_path_sans_ext(fileName), "_backup.csv"),
            quote = T, na = "", row.names=F)
}
