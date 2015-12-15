#' ISAS data manipulation tools
#'
#' @param data ISAS data.
#' @import dplyr

#' @export
as_numeric_RC <- function (data) {
  # 有C、R或S的變數轉成數字
  
  is_tbl <- inherits(data, "tbl")
  
  if (!is.data.frame(data)) {
    data <- dplyr::as_data_frame(data)
  } else if (is_tbl) {
    data <- dplyr::tbl_df(data)
  }
  
  #   seq1 <- grep("C|R|S", names(data), ignore.case = FALSE)
  #   seq2 <- grep("Q", names(data), ignore.case = FALSE)
  #   which_criteria <- intersect(seq1, seq2)
  
  which_criteria <-
    grep("^Q.+(C|R|S)", names(data), ignore.case = FALSE)
  
  data[, which_criteria] <-
    sapply(data[, which_criteria], function(x)
      as.integer(sjmisc::to_value(x)))
  
  if (!is.data.frame(data)) {
    data <- dplyr::as_data_frame(data)
  } else if (is_tbl) {
    data <- dplyr::tbl_df(data)
  }
  data
}


#' @export
as_character_OT <- function (data) {
  # 有T, O的變數轉成character
  
  is_tbl <- inherits(data, "tbl")
  
  if (!is.data.frame(data)) {
    data <- dplyr::as_data_frame(data)
  } else if (is_tbl) {
    data <- dplyr::tbl_df(data)
  }
  
  which_criteria <- grep("^Q[[:alnum:]]+[OT]|ID$",
                         names(data),
                         ignore.case = FALSE)
  
  # get temp labels
  temp_var_label <- sjmisc::get_label(data[, which_criteria])
  
  # to character and trim whitespace
  data[, which_criteria] <-
    sapply(data[, which_criteria],
           function(x)
             stringr::str_trim(as.character(x)))
  
  # restore variable lables
  data[, which_criteria] <-
    sjmisc::set_label(data[, which_criteria],
                      temp_var_label)
  
  if (!is.data.frame(data)) {
    data <- dplyr::as_data_frame(data)
  } else if (is_tbl) {
    data <- dplyr::tbl_df(data)
  }
  data
}


#' @export
to_factor_RS <- function (data) {
  # 有R或S的變數轉成factor
  
  is_tbl <- inherits(data, "tbl")
  
  if (!is.data.frame(data)) {
    data <- dplyr::as_data_frame(data)
  } else if (is_tbl) {
    data <- dplyr::tbl_df(data)
  }
  
  which_criteria <- grep("^Q[[:alnum:]]+[RS]",
                         names(data),
                         ignore.case = FALSE)
  
  # get temp labels
  temp_var_label <- sjmisc::get_label(data[, which_criteria])
  
  #   for (i in which_criteria) {
  #     temp_val_label <- sjmisc::get_val_labels(data[[i]])
  #     data[[i]] <- sjmisc::to_label(data[[i]]) %>%
  #       sjmisc::set_val_labels()
  #   }
  
  data[, which_criteria] <-
    sapply(data[, which_criteria],
           function(x) {
             temp_val_label <- sjmisc::get_val_labels(x)
             x <- sjmisc::to_label(x)
             x <- sjmisc::set_val_labels(x, temp_val_label)
           })
  
  # restore variable lables
  data[, which_criteria] <-
    sjmisc::set_label(data[, which_criteria],
                      temp_var_label)
  
  if (!is.data.frame(data)) {
    data <- dplyr::as_data_frame(data)
  } else if (is_tbl) {
    data <- dplyr::tbl_df(data)
  }
  data
}




#' Combine same choices from ISAS "table + piping + multiple choice"
#'
#' @param df Data.
#' @param var_start Dariable name of table start.
#' @param var_end Variable name of table end.
#' @param mr_C_max Number of items in a multiple response question, excludes repeated.
#' @param steps Number of duplicated questions in a questionset.
#' @param rep_place Where the repeated items locate.
#'            "q" for question; "c" for choices
#'
#' @return
#' data.frame result
#'
#' @export
#'
combine_pipe_table <- function (df,
                                var_start,
                                var_end,
                                mr_C_max,
                                steps,
                                rep_place = c("q", "c")) {
  #   var_start    # 表格起始變數名
  #   var_end      # 表格末端變數名
  #   mr_C_max         # 複選題選項數
  #   steps         # piping同題目數
  rep_place <- match.arg(rep_place)
  
  # 防呆
  if (!(var_start %in% colnames(df)))
    stop(c("\u627e\u4e0d\u5230\u8b8a\u6578: ", var_start, call. = F))
  if (!(var_end %in% colnames(df)))
    stop(c("\u627e\u4e0d\u5230\u8b8a\u6578: ", var_end, call. = F))
  
  switch(
    rep_place,
    q = combine_pipe_table_q(df, var_start, var_end, mr_C_max, steps),
    c = combine_pipe_table_c(df, var_start, var_end, mr_C_max, steps)
  )
  
}

combine_pipe_table_q <- function (df,
                                  var_start,
                                  var_end,
                                  mr_C_max,
                                  steps) {
  
  # 防呆
  startCol <- match(var_start, colnames(df)) # 開始欄
  endCol <- match(var_end, colnames(df)) # 結束欄
  if (startCol >= endCol)
    stop("\u984c\u76ee\u6392\u5217\u9806\u5e8f\u6709\u8aa4", call. = F)
  
  varCountPerMR <- mr_C_max * steps
  totalVarCount <- endCol - startCol + 1
  if (totalVarCount %% varCountPerMR != 0) {
    stop(
      "\u984c\u76ee\u6392\u5217\u6216\u8f38\u5165\u53c3\u6578\u6709\u8aa4",
      call. = F
    )
  } else
    Qcount <- totalVarCount / varCountPerMR # 原始表格未重複題目數
  
  
  col_to_rm <- NULL # reserve space
  for (i in seq(1, totalVarCount - varCountPerMR + 1, by = varCountPerMR)) {
    NowCol <- startCol + i - 1
    
    for (j in 0:(mr_C_max - 1)) {
      cat(i %/% varCountPerMR + 1,
          ":",
          colnames(df)[NowCol + j],
          '<<')
      for (k in 1:(steps-1)) {
        cat(colnames(df)[NowCol + j + k*mr_C_max], " ")
        # if not NA, overwrite previous var
        df[[NowCol + j]] <-
          ifelse(!is.na(df[[NowCol + j + k*mr_C_max]]),
                 df[[NowCol + j + k*mr_C_max]], df[[NowCol + j]])
        col_to_rm <- c(col_to_rm, colnames(df[,NowCol + j + k*mr_C_max]))
      }
      cat("\n")
    }
  }
  
  # remove redundant column
  df <- df[, !names(df) %in% col_to_rm]
  cat("deleted variables: \n", paste0(col_to_rm, collapse = "\n"), "\n")
  
  df
}

combine_pipe_table_c <- function (df,
                                  var_start,
                                  var_end,
                                  mr_C_max,
                                  steps) {

  # 防呆
  startCol <- match(var_start, colnames(df)) # 開始欄
  endCol <- match(var_end, colnames(df)) # 結束欄
  if (startCol >= endCol)
    stop("\u984c\u76ee\u6392\u5217\u9806\u5e8f\u6709\u8aa4", call. = F)
  
  varCountPerMR <- mr_C_max
  totalVarCount <- endCol - startCol + 1
  
  if (totalVarCount %% varCountPerMR != 0) {
    stop(
      "\u984c\u76ee\u6392\u5217\u6216\u8f38\u5165\u53c3\u6578\u6709\u8aa4",
      call. = F
    )
  } else
    Qcount <- totalVarCount / varCountPerMR # 原始題目數
  
  for (i in seq(1, totalVarCount - varCountPerMR + 1, by = varCountPerMR)) {
    NowCol <- startCol + i - 1
    
    for (k in seq(0, (mr_C_max - 1), by = steps)) {
      cat(i %/% varCountPerMR + 1,
          ":",
          colnames(df)[NowCol + k],
          '<<',
          colnames(df)[NowCol + k + steps],
          "\n")
      
      df[[NowCol + k]] <-
        # if not NA, overwrite previous var
        ifelse(!is.na(df[[NowCol + k + steps]]),
               df[[NowCol + k + steps]], df[[NowCol + k]])
    }
  }
  
  df
}


# fix_pipe_table ----------------------------------------------------------
#'
#' Fix ISAS table + piping + multiple choice
#' @param df Data frame to fix.
#' @param var_start Variable name of table start.
#' @param var_end Variable name of table end.
#' @param mr_C_max Number of items in a multiple response question.
#'
#' @export
fix_pipe_table <- function (df, var_start, var_end, mr_C_max) {
  #   var_start    # 表格起始變數名
  #   var_end      # 表格末端變數名
  #   mr_C_max        # 複選題選項數
  steps = 1  # piping同題目數
  
  # 防呆
  if (!(var_start %in% colnames(df)))
    stop(c("\u627e\u4e0d\u5230\u8b8a\u6578: ", var_start, call. = F))
  if (!(var_end %in% colnames(df)))
    stop(c("\u627e\u4e0d\u5230\u8b8a\u6578: ", var_end, call. = F))
  
  startCol <- match(var_start, colnames(df)) # 開始欄
  endCol <- match(var_end, colnames(df)) # 結束欄
  
  # 防呆
  if (startCol >= endCol)
    stop("\u984c\u76ee\u6392\u5217\u9806\u5e8f\u6709\u8aa4")
  
  varCountPerMR <- mr_C_max * steps
  totalVarCount <- endCol - startCol + 1
  
  # 防呆
  if (totalVarCount %% varCountPerMR != 0) {
    stop(
      "\u984c\u76ee\u6392\u5217\u6216\u8f38\u5165\u53c3\u6578\u6709\u8aa4",
      call. = F
    )
  } else
    Qcount <- totalVarCount / varCountPerMR # 原始表格未重複題目數
  
  
  for (i in seq(1, totalVarCount - varCountPerMR + 1, by = mr_C_max)) {
    NowCol <- startCol + i - 1
    
    cat("(\u8907\u9078)",
        names(df)[NowCol:(NowCol + mr_C_max - 1)], "\n")
    
    rows_which_all_zero <-
      which(rowSums(df[NowCol:(NowCol + mr_C_max - 1)], na.rm = TRUE) == 0)
    df[rows_which_all_zero, NowCol:(NowCol + mr_C_max - 1)] <- NA
  }
  
  df
}
