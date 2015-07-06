merge_vars <- function(.data, to, from) { 
  
  to <- deparse(substitute(to))
  from_vars <- substitute(from)
  from_var_pos <- setNames(as.list(seq_along(.data)), names(.data))
  
  pos <- eval(from_vars, from_var_pos)
  
  y <- .data[[to]]
  
  # if not NA, then copy "from" value to "to"; else keep original value
  for(i in pos) {
    y <- ifelse(!is.na(.data[[i]]), 
                .data[[i]], y)
  }
  .data[[to]] <- y
  
  .data
}

# # Examples
# 
# df <- data.frame(
#   a = c(rep(0, 4), NA),
#   b = c(9, 9, NA, 9, NA),
#   c = c(NA, NA, 8, NA, NA)
# )
# 
# merge_vars(df, a, c(b:c))
# 

