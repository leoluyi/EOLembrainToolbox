as.numeric_RC <- function (.data) {
  
  # 有C、R或S的變數轉成數字
  
  is_tbl <- inherits(.data, "tbl")
  
  if(!is.data.frame(.data)) {
    .data <- dplyr::as_data_frame(.data)
  } else if (is_tbl) {
    .data <- dplyr::tbl_df(.data)
  }
  
  #   seq1 <- grep("C|R|S", names(.data), ignore.case = FALSE)
  #   seq2 <- grep("Q", names(.data), ignore.case = FALSE)
  #   which_criteria <- intersect(seq1, seq2)
  
  which_criteria <- grep("^Q.+(C|R|S)", names(.data), ignore.case = FALSE)
  
  .data[, which_criteria] <- 
    sapply(.data[, which_criteria], sjmisc::to_value)
  
  if(!is.data.frame(.data)) {
    .data <- dplyr::as_data_frame(.data)
  } else if (is_tbl) {
    .data <- dplyr::tbl_df(.data)
  }
  .data
}


as.character_OT <- function (.data) {
  
  # 有T, O的變數轉成character
  
  is_tbl <- inherits(.data, "tbl")
  
  if(!is.data.frame(.data)) {
    .data <- dplyr::as_data_frame(.data)
  } else if (is_tbl) {
    .data <- dplyr::tbl_df(.data)
  }
  
  which_criteria <- grep("^Q[[:alnum:]]+[OT]|ID$", 
                         names(.data), 
                         ignore.case = FALSE)
  
  # get temp labels
  temp_var_label <- get_var_labels(.data[,which_criteria])
  
  # to character and trim whitespace
  .data[,which_criteria] <- 
    sapply(.data[,which_criteria], 
           function(x) stringr::str_trim(as.character(x)))
  
  # restore variable lables
  .data[,which_criteria] <- sjmisc::set_var_labels(.data[,which_criteria],
                                                   temp_var_label)
  
  if(!is.data.frame(.data)) {
    .data <- dplyr::as_data_frame(.data)
  } else if (is_tbl) {
    .data <- dplyr::tbl_df(.data)
  }
  .data
}



to_factor_RS <- function (.data) {
  
  # 有R或S的變數轉成factor
  
  is_tbl <- inherits(.data, "tbl")
  
  if(!is.data.frame(.data)) {
    .data <- dplyr::as_data_frame(.data)
  } else if (is_tbl) {
    .data <- dplyr::tbl_df(.data)
  }
  
  which_criteria <- grep("^Q[[:alnum:]]+[RS]", 
                         names(.data), 
                         ignore.case = FALSE)
  
  # get temp labels
  temp_var_label <- sjmisc::get_var_labels(.data[,which_criteria])
  
  #   for (i in which_criteria) {
  #     temp_val_label <- sjmisc::get_val_labels(.data[[i]])
  #     .data[[i]] <- sjmisc::to_label(.data[[i]]) %>%
  #       sjmisc::set_val_labels()
  #   }
  
    .data[, which_criteria] <- 
      sapply(.data[, which_criteria], 
             function(x) {
               temp_val_label <- sjmisc::get_val_labels(x)
               x <- sjmisc::to_label(x)
               x <- sjmisc::set_val_labels(x, temp_val_label)
             })
  
  # restore variable lables
  .data[,which_criteria] <- sjmisc::set_var_labels(.data[,which_criteria],
                                                   temp_var_label)
  
  if (!is.data.frame(.data)) {
    .data <- dplyr::as_data_frame(.data)
  } else if (is_tbl) {
    .data <- dplyr::tbl_df(.data)
  }
  .data
}


combine_pipe_table <- function (.data, .var_start, .var_end, .MR_num, .step) {
  
  #   .var_start    # 表格起始變數名
  #   .var_end      # 表格末端變數名
  #   .MR_num         # 複選題選項數
  #   .step          # piping相同題目數
  
  # 防呆
  if(!(.var_start %in% colnames(.data))) stop(c("找不到變數: ",.var_start))
  if(!(.var_end %in% colnames(.data))) stop(c("找不到變數: ",.var_end))
  
  startCol <- match(.var_start, colnames(.data)) # 開始欄
  endCol <- match(.var_end, colnames(.data)) # 結束欄
  
  # 防呆
  if (startCol >= endCol) stop("題目排列順序有誤")
  
  varCountPerMR <- .MR_num * .step
  totalVarCount <- endCol - startCol + 1
  
  # 防呆
  if (totalVarCount %% varCountPerMR != 0) {
    stop("題目排列或輸入參數有誤")
  } else Qcount <- totalVarCount / varCountPerMR # 原始表格未重複題目數
  
  for (i in seq(1, totalVarCount-varCountPerMR+1, by=varCountPerMR)) {
    NowCol <- startCol + i -1
    
    for (k in 0:(.MR_num -1)) {
      cat(i %/% varCountPerMR + 1, ":",
          colnames(.data)[NowCol+k],'<<',colnames(.data)[NowCol+k+.MR_num],"\n")
      
      .data[[NowCol+k]] <- 
        # 如果不是NA就填回前面變數
        ifelse(!is.na(.data[[NowCol+k+.MR_num]]), 
               .data[[NowCol+k+.MR_num]], .data[[NowCol+k]])
    }
  }
  
  .data
}


fix_pipe_table <- function (.data, .var_start, .var_end, .MR_num) {

  #   .var_start    # 表格起始變數名
  #   .var_end      # 表格末端變數名
  #   .MR_num         # 複選題選項數
  #   .step          # piping相同題目數
  .step = 1
  
  # 防呆
  if (!(.var_start %in% colnames(.data))) stop(c("找不到變數: ",.var_start))
  if (!(.var_end %in% colnames(.data))) stop(c("找不到變數: ",.var_end))
  
  startCol <- match(.var_start, colnames(.data)) # 開始欄
  endCol <- match(.var_end, colnames(.data)) # 結束欄
  
  # 防呆
  if (startCol >= endCol) stop("題目排列順序有誤")
  
  varCountPerMR <- .MR_num * .step
  totalVarCount <- endCol - startCol + 1
  
  # 防呆
  if (totalVarCount %% varCountPerMR != 0) {
    stop("題目排列或輸入參數有誤")
  } else Qcount <- totalVarCount / varCountPerMR # 原始表格未重複題目數
  
  
  for (i in seq(1, totalVarCount-varCountPerMR+1, by=.MR_num)) {
    NowCol <- startCol + i - 1
    
    cat("(複選)",
        names(.data)[NowCol:(NowCol+.MR_num-1)], "\n")
    
    rows_which_all_zero <- which(rowSums(.data[NowCol:(NowCol+.MR_num-1)]) == 0)
    .data[rows_which_all_zero, NowCol:(NowCol+.MR_num-1)] <- NA
  }
  
  .data
}


