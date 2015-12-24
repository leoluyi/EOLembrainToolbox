#' ISAS data manipulation tools
#'
#' @param df ISAS data.
#' @import dplyr
#' 
#' @rdname as_numeric_RC
#' @export
as_numeric_RC <- function (df) {
  # 有C、R或S的變數轉成數字
  
  is_tbl <- inherits(df, "tbl")
  
  if (!is.data.frame(df)) {
    df <- dplyr::as_data_frame(df)
  } else if (is_tbl) {
    df <- dplyr::tbl_df(df)
  }
  
  #   seq1 <- grep("C|R|S", names(df), ignore.case = FALSE)
  #   seq2 <- grep("Q", names(df), ignore.case = FALSE)
  #   which_criteria <- intersect(seq1, seq2)
  
  which_criteria <-
    grep("^Q.+(C|R|S)", names(df), ignore.case = FALSE)
  
  df[, which_criteria] <-
    sapply(df[, which_criteria], function(x) {
      if (is.factor(x)) {
        as.integer(sjmisc::to_value(x))
      }
      else
        as.numeric(as.character(x))
    })
  
  if (!is.data.frame(df)) {
    df <- dplyr::as_data_frame(df)
  } else if (is_tbl) {
    df <- dplyr::tbl_df(df)
  }
  df
}

#' @import dplyr
#' @rdname as_numeric_RC
#' @export
as_character_OT <- function (df) {
  # 有T, O的變數轉成character
  
  is_tbl <- inherits(df, "tbl")
  
  if (!is.data.frame(df)) {
    df <- dplyr::as_data_frame(df)
  } else if (is_tbl) {
    df <- dplyr::tbl_df(df)
  }
  
  which_criteria <- grep("^Q[[:alnum:]]+[OT]|ID$",
                         names(df),
                         ignore.case = FALSE)
  
  # get temp labels
  temp_var_label <- sjmisc::get_label(df[, which_criteria])
  
  # to character and trim whitespace
  df[, which_criteria] <-
    sapply(df[, which_criteria],
           function(x)
             stringr::str_trim(as.character(x)))
  
  # restore variable lables
  df[, which_criteria] <-
    sjmisc::set_label(df[, which_criteria],
                      temp_var_label)
  
  if (!is.data.frame(df)) {
    df <- dplyr::as_data_frame(df)
  } else if (is_tbl) {
    df <- dplyr::tbl_df(df)
  }
  df
}

#' @import sjmisc
#' @rdname as_numeric_RC
#' @export
to_factor_RS <- function (df) {
  # 有R或S的變數轉成factor
  
  is_tbl <- inherits(df, "tbl")
  
  if (!is.data.frame(df)) {
    df <- dplyr::as_data_frame(df)
  } else if (is_tbl) {
    df <- dplyr::tbl_df(df)
  }
  
  which_criteria <- grep("^Q[[:alnum:]]+[RS]",
                         names(df),
                         ignore.case = FALSE)
  
  # get temp labels
  temp_var_label <- sjmisc::get_label(df[, which_criteria])
  
  #   for (i in which_criteria) {
  #     temp_val_label <- sjmisc::get_val_labels(df[[i]])
  #     df[[i]] <- sjmisc::to_label(df[[i]]) %>%
  #       sjmisc::set_val_labels()
  #   }
  
  df[, which_criteria] <-
    sapply(df[, which_criteria],
           function(x) {
             temp_val_label <- sjmisc::get_val_labels(x)
             x <- sjmisc::to_label(x)
             x <- sjmisc::set_val_labels(x, temp_val_label)
           })
  
  # restore variable lables
  df[, which_criteria] <-
    sjmisc::set_label(df[, which_criteria],
                      temp_var_label)
  
  if (!is.data.frame(df)) {
    df <- dplyr::as_data_frame(df)
  } else if (is_tbl) {
    df <- dplyr::tbl_df(df)
  }
  df
}

