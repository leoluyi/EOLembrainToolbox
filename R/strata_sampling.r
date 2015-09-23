# ## -- update package ----
# if(packageDescription("openxlsx")$Version < "3.0.0") {
#   devtools::install_github("awalker89/openxlsx")
# }


#' Create strataCrossN
#'
#' @param .strataPctList See example.
#' @param .namesList See example.
#' @param N Total sample size.
#'
#' @import dplyr
#' 
#' @examples
#' N = 900
#'
#' strataPctList <- list(
#'   sex = c(0.5, 0.5),
#'   age = c(1/3, 1/3, 1/3),
#'   area = c(0.5, 0.2, 0.3)
#' )
#'
#' namesList <- list(sex=1:2, age=2:4, area=1:3)
#' vars <- c("Q1R", "Q2R", "Q3R")   # names(namesList)
#' 
#' strataCrossN_flat <- as.strataCrossN(strataPctList, namesList, N)
#' 
#' @export
to_strataCrossN <- function (.strataPctList, .namesList, N) {
  
  ## error handler
  if (length(.strataPctList) != length(.namesList))
    stop("`strataPctList` and `namesList`\u9577\u5ea6\u4e0d\u4e00\u81f4", call.=FALSE)
  if(!identical(setdiff(names(.strataPctList), names(.namesList)), character(0)))
    stop("names of `strataPctList` and `namesList` must be identical", call.=FALSE)
  if (is.list(.strataPctList) != TRUE)
    stop(".strataPctList must have class \"list\"", call.=FALSE)
  if (is.list(.namesList) != TRUE)
    stop(".namesList must have class \"list\"", call.=FALSE)
  
  for (i in seq_along(.strataPctList)) {
    if (i ==1) {strataCrossArr <- .strataPctList[[1]]}
    else {strataCrossArr <- outer(strataCrossArr, .strataPctList[[i]])}
  }
  
  strataCrossArr <- as.array(strataCrossArr)
  .namesList <- .namesList[names(.strataPctList)]
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


#' Check marginal proportion
#' @param .strataCrossN_flat 
#'
#' @param export 
#' @param outputfile 
#' 
#' @import openxlsx sampling
#' 
#' @export
margin_prop <- function(.strataCrossN_flat,
                        export = FALSE,
                        outputfile="R\u5be6\u62bd\u6a23\u672ctemp.csv")
{
  
  ## 實抽樣本expanded
  strataCrossN.expanded <- .strataCrossN_flat[
    rep(row.names(.strataCrossN_flat), .strataCrossN_flat$Freq),
    1:length(.strataCrossN_flat)]
  
  strataCrossN.expanded <- strataCrossN.expanded[-length(strataCrossN.expanded)]
  
  Ndim <- length(strataCrossN.expanded)
  n_total <- margin.table(table(strataCrossN.expanded)) # Sum total
  
  ## 建立list
  mar_tab <- function(x) {
    prop.table(table(x, deparse.level = 0))
  }
  
  tab <- function(x) table(x, deparse.level = 0)
  
  probList <- c(n_total = n_total,
                lapply(strataCrossN.expanded, FUN = mar_tab))
  
  freqList <- lapply(strataCrossN.expanded, FUN = tab)
  print(freqList)
  cat(paste0(rep("-", 40), collapse = ""), "\n")
  
  ## write txt
  
  #cat(.strataCrossN_flat$Freq, file = "實抽樣本vector.txt", sep = "\n")
  if(export) {
    write.csv(.strataCrossN_flat, file = outputfile, row.names=FALSE)
  }
  
  probList <- c(probList[1], lapply(probList[-1], format, digits=3, nsmall=3))
  
  probList
}



renew_sampleN <- function (.strataCrossN_flat, outputfile="R\u5be6\u62bd\u6a23\u672ctemp.csv") {
  
  ## 實抽樣本從txt重讀
  # 實抽樣本 <- as.numeric(readLines("實抽樣本vector.txt"))
  sample_size <- read.csv(file = outputfile, header=TRUE)$Freq
  .strataCrossN_flat$Freq <- sample_size  # 更新
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


#' sample C
#' @param .data 
#'
#' @param .strataCrossN_flat 
#' @param .vars 
#' @param .namesList 
#' @import dplyr lazyeval
#' @export
sampleC <- function (.data,
                     .strataCrossN_flat,
                     .vars,
                     .namesList) {
  
  ## 防呆
  if (!identical(setdiff(names(.vars), names(.namesList)), character(0)))
    stop("names of `.vars` and `.namesList` must be identical", call.=FALSE)
  if(!identical(setdiff(names(.strataPctList), names(.namesList)), character(0)))
    stop("names of `strataPctList` and `namesList` must be identical", call.=FALSE)
  if(!identical(setdiff(names(.strataCrossN_flat)[names(.strataCrossN_flat)!="Freq"],
                        names(.namesList)), character(0)))
    stop("names of `.strataCrossN_flat` and `namesList` must be identical", call.=FALSE)
  if (is.data.frame(.data) != TRUE)
    stop(".data must have class \"data.frame\"", call.=FALSE)
  
  ID_var_name <- names(.data)[grep("ID$", names(.data), ignore.case = TRUE)][[1]]
  if (length(ID_var_name)!=0)
    if (anyDuplicated(.data[[ID_var_name]])) {
      stop(c("ID\u6709\u91cd\u8907: ", "\n",
             paste0(unique(
               .data[[ID_var_name]][duplicated(.data[[ID_var_name]])]),
               collapse = "\n")),
           call.=FALSE)
    }
  
  if (anyDuplicated(vars)) stop("duplicated '.vars'", call.=FALSE)
  
  
  var_names <- names(.namesList)
  
  # .data排除不在list的樣本
  for(i in seq_along(.namesList)){
    filter_criteria <- lazyeval::interp(~ as.character(which_column) %in% as.character(.namesList[[i]]),
                                        which_column = as.name(.vars[names(.namesList)[i]]))
    
    .data <- .data %>% filter_(filter_criteria)
  }
  
  
  # .data check levels
  for(i in seq_along(.namesList)){
    .data[[.vars[names(.namesList)[i]]]] <- .data[[.vars[names(.namesList)[i]]]] %>%
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
    .data[.vars[var_names]]) %>% # (Q1R..為excel檔表頭變數名)
    as.data.frame.table()
  
  # sampleCross_flat 排序
  sampleCross_flat <- sampleCross_flat %>% arrange_(.dots = rev(.vars[var_names]))
  
  # 檢查多寡
  ncolStrata = ncol(.strataCrossN_flat)
  ncolSample = ncol(sampleCross_flat)
  
  n1 <- sampleCross_flat[["Freq"]]
  n2 <- .strataCrossN_flat[["Freq"]]
  
  # 防呆
  if (length(n1) != length(n2)) stop("\u6a23\u672c\u7d44\u5408\u8207\u5206\u5c64\u6578\u4e0d\u76f8\u7b49", call.=FALSE)
  
  strataCompare <-
    cbind(.strataCrossN_flat[!(names(.strataCrossN_flat) %in% c("Freq"))],
          sample_size = n2,
          Completed_N = n1,
          diff = n1 - n2
    )
  
  names(strataCompare) <- ifelse(names(strataCompare) %in% var_names,
                                 paste(.vars[var_names], var_names , sep = "_"),
                                 names(strataCompare)
  )
  
  ## sort variables
  strataCompare <- strataCompare[ifelse(names(strataCompare) %in% sort(paste(.vars[var_names], var_names , sep = "_")),
                                        sort(paste(.vars[var_names], var_names , sep = "_")),
                                        names(strataCompare))]
  
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



#' strata sample ID
#'
#' @param .data 
#' @param .strataCrossN_flat 
#' @param .namesList 
#' @param .vars 
#' @param ID_var_name 
#' @param method 
#' @param pik 
#'
#' @export
strata_ids <- function (.data, 
                        .strataCrossN_flat,
                        .namesList,
                        .vars, 
                        ID_var_name="Panel.ID",
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
  # .strataCrossN_flat <- .strataCrossN_flat[.strataCrossN_flat$Freq!=0,]
  
  ## 防呆
  if (!identical(setdiff(names(.vars), names(.namesList)), character(0)))
    stop("names of `.vars` and `.namesList` must be identical", call.=FALSE)
  if(!identical(setdiff(names(.strataCrossN_flat)[names(.strataCrossN_flat)!="Freq"],
                        names(.namesList)), character(0)))
    stop("names of `.strataCrossN_flat` and `namesList` must be identical", call.=FALSE)
  
  
  ID_var_name <- ID_var_name[[1]]
  
  if(anyDuplicated(.data[[ID_var_name]])) {
    stop(c("\u62bd\u53d6\u4e4bID\u6709\u91cd\u8907: ", "\n",
           paste0(.data[[ID_var_name]][duplicated(.data[[ID_var_name]])],
                  collapse = "\n")),
         call.=FALSE)
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
  
  # .strataCrossN_flat 移 除Freq == 0
  .strataCrossN_flat <- .strataCrossN_flat %>% filter(Freq != 0)
  
  N_vector <- .strataCrossN_flat$Freq
  
  # .data排除重複id
  .data <- .data %>% distinct_(ID_var_name)
  
  # .data排除不在list的樣本
  for(i in seq_along(.namesList)){
    filter_criteria <- lazyeval::interp(~ as.character(which_column) %in% as.character(.namesList[[i]]),
                                        which_column = as.name(.vars[names(.namesList)[i]]))
    
    .data <- .data %>% filter_(filter_criteria)
  }
  
  # .data check levels
  for(i in seq_along(.namesList)){
    .data[[.vars[names(.namesList)[i]]]] <- .data[[.vars[names(.namesList)[i]]]] %>%
      factor(levels=.namesList[[i]])
  }
  
  # .strataCrossN_flat check levels
  for(i in seq_along(.namesList)){
    .strataCrossN_flat[[var_names[i]]] <- .strataCrossN_flat[[var_names[i]]] %>%
      factor(levels=.namesList[[i]])
  }
  
  # .data 排序 (strata抽樣樣本要先排序)
  data.reordered <- arrange_(.data, .dots = rev(.vars[var_names])) ## 以後面變數為第一排序層級
  
  
  # 抽出之ID
  if(!is.null(method)) method <- match.arg(method)
  
  ID_Output <- data.reordered %>%
    sampling::strata(stratanames = .vars[var_names], 
                     size = N_vector,
                     method = method, 
                     .data[[pik]]
    ) %>%
    sampling::getdata(data.reordered, .) %>%
    `[[`(ID_var_name) %>% unlist %>% unname
  
  cat("\u5171", length(ID_Output), "\u500bID\u88ab\u62bd\u51fa\n")
  
  return(ID_Output)
}



#' write output xlsx
#'
#' @param ID_sample 
#' @param .data 
#' @param ID_var_name 
#'
#' @export
write_output_xlsx <- function(ID_sample, .data, ID_var_name){
  
  # ID_var_name: ID所在變數名
  # ID_sample: 要抽取的ID
  
  # 防呆
  if(anyDuplicated(ID_sample)) {
    stop(c("\u62bd\u53d6\u4e4bID\u6709\u91cd\u8907: ", "\n",
           paste0(ID_sample[duplicated(ID_sample)],
                  collapse = "\n")))
  }
  
  # .data排除重複id
  .data <- .data %>% distinct_(ID_var_name)
  
  tempData <- .data[.data[[ID_var_name]] %in% ID_sample,]
  tempData_backup <- .data[!(.data[[ID_var_name]] %in% ID_sample),]
  
  outputfileName <- paste(tools::file_path_sans_ext(fileName),
                          "\u62bd\u6a23\u5f8craw_data.xlsx",sep = "_")
  
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



#' write_output_csv
#' @param ID_sample 
#'
#' @param .data 
#' @param ID_var_name 
#'
#' @export
write_output_csv <- function(ID_sample, .data, ID_var_name){
  
  # ID_var_name: ID所在變數名
  # ID_sample: 要抽取的ID
  
  # 防呆
  if(anyDuplicated(ID_sample)) {
    stop(c("\u62bd\u53d6\u4e4bID\u6709\u91cd\u8907: ", "\n",
           paste0(ID_sample[duplicated(ID_sample)],
                  collapse = "\n")),
         call.=FALSE)
  }
  
  # .data排除重複id
  .data <- .data %>% distinct_(ID_var_name)
  
  tempData <- .data[.data[[ID_var_name]] %in% ID_sample,]
  tempData_backup <- .data[!(.data[[ID_var_name]] %in% ID_sample),]
  
  outputfileName <- paste(tools::file_path_sans_ext(fileName),
                          "\u62bd\u6a23\u5f8craw_data.csv",sep = "_")
  write.csv(x = tempData, file = outputfileName,
            quote = T, na = "", row.names=F)
  write.csv(x = tempData_backup,
            file = paste0(tools::file_path_sans_ext(fileName), "_backup.csv"),
            quote = T, na = "", row.names=F)
}
