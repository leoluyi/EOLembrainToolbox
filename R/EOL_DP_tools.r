#' ISAS data manipulation tools
#'
#' @param .data ISAS data.
#' @import dplyr

#' @export
as_numeric_RC <- function (.data) {
  
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


#' @export
as_character_OT <- function (.data) {
  
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
  temp_var_label <- sjmisc::get_var_labels(.data[,which_criteria])
  
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


#' @export
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



#' Combine same choices from ISAS "table + piping + multiple choice"
#'
#' @param .data 
#' @param .var_start 
#' @param .var_end 
#' @param .MR_num 
#' @param .step 
#'
#' @export
#'
combine_pipe_table <- function (.data, .var_start, .var_end, .MR_num, .step) {
  
  #   .var_start    # 表格起始變數名
  #   .var_end      # 表格末端變數名
  #   .MR_num         # 複選題選項數
  #   .step          # piping相同題目數
  
  # 防呆
  if(!(.var_start %in% colnames(.data))) stop(c("\u627e\u4e0d\u5230\u8b8a\u6578: ",.var_start))
  if(!(.var_end %in% colnames(.data))) stop(c("\u627e\u4e0d\u5230\u8b8a\u6578: ",.var_end))
  
  startCol <- match(.var_start, colnames(.data)) # 開始欄
  endCol <- match(.var_end, colnames(.data)) # 結束欄
  
  # 防呆
  if (startCol >= endCol) stop("\u984c\u76ee\u6392\u5217\u9806\u5e8f\u6709\u8aa4")
  
  varCountPerMR <- .MR_num * .step
  totalVarCount <- endCol - startCol + 1
  
  # 防呆
  if (totalVarCount %% varCountPerMR != 0) {
    stop("\u984c\u76ee\u6392\u5217\u6216\u8f38\u5165\u53c3\u6578\u6709\u8aa4")
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


# fix_pipe_table ----------------------------------------------------------
#' 
#' Fix ISAS table + piping + multiple choice 
#' @param .data Data frame to fix.
#' @param .var_start Variable name of table start.
#' @param .var_end Variable name of table end.
#' @param .MR_num Number of multiple choices in each question.
#'
#' @export
fix_pipe_table <- function (.data, .var_start, .var_end, .MR_num) {
  
  #   .var_start    # 表格起始變數名
  #   .var_end      # 表格末端變數名
  #   .MR_num        # 複選題選項數
  .step = 1  # piping相同題目數
  
  # 防呆
  if (!(.var_start %in% colnames(.data))) stop(c("\u627e\u4e0d\u5230\u8b8a\u6578: ",.var_start))
  if (!(.var_end %in% colnames(.data))) stop(c("\u627e\u4e0d\u5230\u8b8a\u6578: ",.var_end))
  
  startCol <- match(.var_start, colnames(.data)) # 開始欄
  endCol <- match(.var_end, colnames(.data)) # 結束欄
  
  # 防呆
  if (startCol >= endCol) stop("\u984c\u76ee\u6392\u5217\u9806\u5e8f\u6709\u8aa4")
  
  varCountPerMR <- .MR_num * .step
  totalVarCount <- endCol - startCol + 1
  
  # 防呆
  if (totalVarCount %% varCountPerMR != 0) {
    stop("\u984c\u76ee\u6392\u5217\u6216\u8f38\u5165\u53c3\u6578\u6709\u8aa4")
  } else Qcount <- totalVarCount / varCountPerMR # 原始表格未重複題目數
  
  
  for (i in seq(1, totalVarCount-varCountPerMR+1, by=.MR_num)) {
    NowCol <- startCol + i - 1
    
    cat("(\u8907\u9078)",
        names(.data)[NowCol:(NowCol+.MR_num-1)], "\n")
    
    rows_which_all_zero <- which(rowSums(.data[NowCol:(NowCol+.MR_num-1)]) == 0)
    .data[rows_which_all_zero, NowCol:(NowCol+.MR_num-1)] <- NA
  }
  
  .data
}
