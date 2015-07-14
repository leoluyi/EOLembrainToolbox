## -- update package ----
if(packageDescription("openxlsx")$Version < "3.0.0") {
  devtools::install_github("awalker89/openxlsx")
}


as.strataCrossN <- function(.strataPctList, .namesList, N){
  
  ## error handler
  if (length(.strataPctList) != length(.namesList)) 
    stop("`strataPctList`�M`namesList`���פ��@�P")
  if (is.list(.strataPctList) != TRUE) stop(".strataPctList must have class \"list\"")
  if (is.list(.namesList) != TRUE) stop(".strataPctList must have class \"list\"")
  
  for (i in seq_along(strataPctList)){
    if (i ==1) {strataCrossArr <- strataPctList[[1]]}
    else {strataCrossArr <- outer(strataCrossArr, strataPctList[[i]])}
  }
  strataCrossArr <- as.array(strataCrossArr)
  dimnames(strataCrossArr) <- .namesList
  
  ## ��˰}�C
  N <- as.integer(N)
  strataCrossN <- round(strataCrossArr*N, 0); # sum(strataCrossN) # �`��
  ## ������
  strataCrossN_flat = as.data.frame.table(strataCrossN)
  ## �Υ�{plyr}����������
  # library(plyr)
  # adply(strataCrossN, c(1,2,3))
  return(strataCrossN_flat)
}



margin_prop <- function(.strataCrossN_flat,
                        export = FALSE,
                        outputfile="R���˥�temp.csv")
{
  
  ## ���˥�expanded
  strataCrossN.expanded <- .strataCrossN_flat[
    rep(row.names(.strataCrossN_flat), .strataCrossN_flat$Freq), 
    1:length(.strataCrossN_flat)]
  
  strataCrossN.expanded <- strataCrossN.expanded[-length(strataCrossN.expanded)]
  
  Ndim <- length(strataCrossN.expanded)
  �`��˥� <- margin.table(table(strataCrossN.expanded)) # Sum total
  
  ## �إ�list
  mar_tab <- function(x) {
    prop.table(table(x, deparse.level = 0))
  }
  
  tab <- function(x) table(x, deparse.level = 0)
  
  probList <- c(�`�˥��� = �`��˥�, 
                    lapply(strataCrossN.expanded, FUN = mar_tab))
  
  freqList <- lapply(strataCrossN.expanded, FUN = tab)
  print(freqList)
  cat(paste0(rep("-", 40), collapse = ""), "\n")
  
  ## ���˥��g�Jtxt
  
  #cat(.strataCrossN_flat$Freq, file = "���˥�vector.txt", sep = "\n")
  if(export) {
    write.csv(.strataCrossN_flat, file = outputfile, row.names=FALSE)
  }
  
  probList <- c(probList[1], lapply(probList[-1], format, digits=3, nsmall=3))
  
  probList
}



renew_sampleN <- function (.strataCrossN_flat, outputfile="R���˥�temp.csv") {
  
  ## ���˥��qtxt��Ū
  # ���˥� <- as.numeric(readLines("����vector��.txt"))
  ���˥� <- read.csv(file = outputfile, header=TRUE)$Freq
  .strataCrossN_flat$Freq <- ���˥�  # ��s
  ## ���˥��g�Jtxt
  #cat(.strataCrossN_flat$Freq, file = "���˥�vector.txt", sep = "\n")
  write.csv(.strataCrossN_flat, file = outputfile, row.names=FALSE)
  return(.strataCrossN_flat)
}


# ## Ū��raw data
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


# ## Ū��raw data
# read.data.xlsx2 <- function (.fileName, .sheetIndex=1, .colIndex=NULL, 
#                              .colClasses="character", encoding="unknown")
# {
#   jgc <- function(){
#     .jcall("java/lang/System", method = "gc")
#   }  
#   
#   jgc()
#   
#   options(java.parameters = "-Xmx4g") # read.xlsx�W�[�O����
#   
#   .data <- xlsx::read.xlsx2(
#     file = .fileName,
#     sheetIndex = .sheetIndex,
#     header = T,
#     colClasses = .colClasses,
#     colIndex = .colIndex,  # �u�^���e�Q��
#     stringsAsFactors = FALSE
#   )
#   return(.data)
# }


# ## Ū��raw data
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
#     colIndex = .colIndex,  # �u�^���e�Q��
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
  
  # ���b
  if (length(.vars) != length(.namesList))
    stop("length(.vars) must equal to length(.namesList)")
  if (is.data.frame(.data) != TRUE) 
    stop(".data must have class \"data.frame\"")
  
  ID_var_name <- names(.data)[grep("ID$", names(.data), ignore.case = TRUE)][[1]]
  if(length(ID_var_name)!=0)
    if(anyDuplicated(.data[[ID_var_name]])) {
      stop(c("ID������: ", "\n",
             paste0(unique(
               .data[[ID_var_name]][duplicated(.data[[ID_var_name]])]), 
               collapse = "\n")))
    }
  
  
  var_names <- names(.namesList)
  
  # �ư����blist���˥�
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
  
  
  # .strataCrossN_flat �Ƨ�
  .strataCrossN_flat <- .strataCrossN_flat[c(var_names, "Freq")]
  .strataCrossN_flat <- .strataCrossN_flat %>%
    arrange_(.dots = rev(names(.strataCrossN_flat)[-length(.strataCrossN_flat)]))
  
  # �˥����G
  sampleCross_flat <- table(
    .data[.vars]) %>% # (Q1R..��excel�ɪ��Y�ܼƦW)
    as.data.frame.table()
  
  # sampleCross_flat �Ƨ�
  sampleCross_flat <- sampleCross_flat %>% arrange_(.dots = rev(.vars))
  
  # �ˬd�h��
  ncolStrata = ncol(.strataCrossN_flat)
  ncolSample = ncol(sampleCross_flat)
  
  n1 <- sampleCross_flat[["Freq"]]
  n2 <- .strataCrossN_flat[["Freq"]]
  
  # ���b
  if (length(n1) != length(n2)) stop("�˥��զX�P���h�Ƥ��۵�")
  
  strataCompare <-
    cbind(.strataCrossN_flat[!(names(.strataCrossN_flat) %in% c("Freq"))], 
          ����˥��� = n2,
          Completed_N = n1,
          �˥��h�� = n1 - n2
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
  # ���˥� <- ���˥�[���˥�!=0]
  
  ID_var_name <- ID_var_name[[1]]
  
  if(anyDuplicated(.data[[ID_var_name]])) {
    stop(c("�����ID������: ", "\n",
           paste0(.data[[ID_var_name]][duplicated(.data[[ID_var_name]])], 
                  collapse = "\n")))
  }
  
  var_names <- names(.namesList)
  
  # .strataCrossN_flat check levels
  for(i in seq_along(.namesList)){
    .strataCrossN_flat[[var_names[i]]] <- .strataCrossN_flat[[var_names[i]]] %>%
      factor(levels=.namesList[[i]])
  }
  
  # .strataCrossN_flat �Ƨ�
  .strataCrossN_flat <- .strataCrossN_flat[,c(var_names, "Freq")]
  .strataCrossN_flat <- .strataCrossN_flat %>%
    arrange_(.dots = rev(names(.strataCrossN_flat)[-length(.strataCrossN_flat)]))
  
  # .strataCrossN_flat ����Freq == 0
  .strataCrossN_flat <- .strataCrossN_flat %>%
    filter(!(Freq == 0))
  
  N_vector <- .strataCrossN_flat$Freq
  
  # .data�ư�����id
  .data <- .data %>% distinct_(ID_var_name)
  
  # .data�ư����blist���˥�
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
  
  # .data �Ƨ� (strata��˼˥��n���Ƨ�)
  data.reordered <- arrange_(.data, .dots = rev(.vars)) ## �H�᭱�ܼƬ��Ĥ@�ƧǼh��
  
  # ��X��ID
  ID_Output <- data.reordered %>%
    sampling::strata(stratanames=.vars, size=N_vector, 
           method, .data[[pik]]) %>%
    sampling::getdata(data.reordered,.) %>%
    `[[`(ID_var_name) %>% unlist %>% unname
  
  cat("�@", length(ID_Output), "��ID�Q��X\n")
  
  return(ID_Output)
}


write.output.xlsx3 <- function(ID_sample, .data, ID_var_name){
  
  # ID_var_name: ID�Ҧb�ܼƦW
  # ID_sample: �n�����ID
  
  # ���b
  if(anyDuplicated(ID_sample)) {
    stop(c("�����ID������: ", "\n",
           paste0(ID_sample[duplicated(ID_sample)], 
                  collapse = "\n")))
  }
  
  # .data�ư�����id
  .data <- .data %>% distinct_(ID_var_name)
  
  tempData <- .data[.data[[ID_var_name]] %in% ID_sample,]
  tempData_backup <- .data[!(.data[[ID_var_name]] %in% ID_sample),]
  
  outputfileName <- paste(tools::file_path_sans_ext(fileName),
                          "��˫�raw_data.xlsx",sep = "_")
  
  # �ରdata.frame
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
#   # ID_var_name: ID�Ҧb�ܼƦW
#   # ID_sample: �n�����ID
#   
#   options(java.parameters = "-Xmx4g") # read.xlsx�W�[�O����
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
#   # ���b
#   if(anyDuplicated(ID_sample)) {
#     stop(c("�����ID������: ", "\n",
#            paste0(ID_sample[duplicated(ID_sample)], 
#                   collapse = "\n")))
#   }
#   
#   # .data�ư�����id
#   .data <- .data %>% distinct_(ID_var_name)
#   
#   tempData <- .data[.data[[ID_var_name]] %in% ID_sample,]
#   tempData_backup <- .data[!(.data[[ID_var_name]] %in% ID_sample),]
#   
#   outputfileName <- paste(file_path_sans_ext(fileName),
#                           "��˫�raw_data.xlsx",sep = "_")
#   write.xlsx2(x = tempData, file = outputfileName,
#               sheetName = paste("N=", length(unique(ID_sample)),sep=""), 
#               row.names = FALSE, showNA=FALSE)
#   write.xlsx2(x = tempData_backup, file = outputfileName, append=TRUE,
#               sheetName = "backup", 
#               row.names = FALSE, showNA=FALSE)
# }


# write.output.xlsx1 <- function(ID_sample, .data, ID_var_name){
#   
#   # ID_var_name: ID�Ҧb�ܼƦW
#   # ID_sample: �n�����ID
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
#   # ���b
#   if(anyDuplicated(ID_sample)) {
#     stop(c("�����ID������: ", "\n",
#            paste0(ID_sample[duplicated(ID_sample)], 
#                   collapse = "\n")))
#   }
#   
#   # .data�ư�����id
#   .data <- .data %>% distinct_(ID_var_name)
#   
#   tempData <- .data[.data[[ID_var_name]] %in% ID_sample,]
#   tempData_backup <- .data[!(.data[[ID_var_name]] %in% ID_sample),]
#   
#   outputfileName <- paste(file_path_sans_ext(fileName),
#                           "��˫�raw_data.xlsx",sep = "_")
#   write.xlsx(x = tempData, file = outputfileName,
#              sheetName = paste("N=", length(unique(ID_sample)),sep=""), 
#              row.names = FALSE, showNA=FALSE)
#   write.xlsx(x = tempData_backup, file = outputfileName, append=TRUE,
#              sheetName = "backup", row.names = FALSE, showNA=FALSE)
# }

write.output.csv <- function(ID_sample, .data, ID_var_name){
  
  # ID_var_name: ID�Ҧb�ܼƦW
  # ID_sample: �n�����ID
  
  # ���b
  if(anyDuplicated(ID_sample)) {
    stop(c("�����ID������: ", "\n",
           paste0(ID_sample[duplicated(ID_sample)], 
                  collapse = "\n")))
  }
  
  # .data�ư�����id
  .data <- .data %>% distinct_(ID_var_name)
  
  tempData <- .data[.data[[ID_var_name]] %in% ID_sample,]
  tempData_backup <- .data[!(.data[[ID_var_name]] %in% ID_sample),]
  
  outputfileName <- paste(tools::file_path_sans_ext(fileName),
                          "��˫�raw_data.csv",sep = "_")
  write.csv(x = tempData, file = outputfileName,
            quote = T, na = "", row.names=F)
  write.csv(x = tempData_backup, 
            file = paste0(tools::file_path_sans_ext(fileName), "_backup.csv"),
            quote = T, na = "", row.names=F)
}
