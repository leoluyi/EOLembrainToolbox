#' Copy/Paste data out of/into R
#' 
#' Using the clipboard, or passing data quickly from Excel to R 
#' and back again.
#'
#' @param obj object to copy
#' @param size size of memory
#'
#' @export
#'
#' @references 
#' Using the Windows Clipboard, or Passing Data Quickly From Excel to R and Back Again
#' 
#' http://www.r-bloggers.com/using-the-windows-clipboard-or-passing-data-quickly-from-excel-to-r-and-back-again/
#' 
#' http://stackoverflow.com/questions/9035674/r-function-to-copy-to-clipboard-on-mac-osx
#'
#' @examples
#' # These all work
#' copy_tbl(1:100)
#' copy_tbl(letters)
#' copy_tbl(iris)
#' copy_tbl(table(iris$Species))
#' copy_tbl(matrix(1:20, nrow = 2))
#'
#' \dontrun{
#' # If my.df is of moderate size
#' copy_tbl(my.df)
#'
#' # If my.df is huge
#' copy_tbl(my.df, 10000)
#'
#' # Pasting works in a similar way. Select the range in Excel you want to copy
#' # (including the header) and press Ctrl+C. Then run
#'
#' other.df <- paste_tbl()
#' }
#' 
copy_tbl <- function(obj, size = 4096) {
  sn <- Sys.info()["sysname"]
  if (sn == "Darwin") {
    f <- pipe("pbcopy", "w")
  } else if (sn == "Windows") {
    clip <- paste('clipboard-', size, sep = '')
    f <- file(description = clip, open = 'w')
  } else {
    stop("Reading from the clipboard is not implemented for your system (",
         sn, ") in this package.")
  }
  
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)
}

#' @rdname copy_tbl
#' @export
# Paste data into R
paste_tbl <- function(header = TRUE) {
  sn <- Sys.info()["sysname"]
  if (sn == "Darwin") {
    f <- pipe("pbpaste")
  } else if (sn == "Windows") {
    f <- file(description = 'clipboard', open = 'r')
  } else {
    stop("Reading from the clipboard is not implemented for your system (",
         sn, ") in this package.")
  }
  
  df <- read.table(f, sep = '\t', header = header, stringsAsFactors = FALSE)
  close(f)
  df
}


