#' Paste character string from clipboard into R
#'
#' @param addC add \code{c(...)} or not
#'
#' @export
#'
#' @details 
#' \code{V}() for column of string vector
#' 
#' \code{V_tab}() for tab seperated string vector
#' 
#' @seealso
#' see \url{http://www.r-bloggers.com/efficient-variable-selection-in-r/}
#' 
#' @examples
#' ## Typical application:
#' 
#' # Step 1: paste a column of variable names from a meta database;  
#' # Step 2: Run this function in R: V()  
#' # Step 3: Press control(command) + V: Paste vector of clipboard  
#' 
#' 
V <- function(addC = TRUE) {
  # http://www.r-bloggers.com/efficient-variable-selection-in-r/
  # Typical application:
  # Step 1: paste a column of variable names from a meta database;
  # Step 2: Run this function in R: V()
  # Step 3: Press control + V: Paste vector of clipboard
  
  sn <- Sys.info()["sysname"]
  if (sn == "Darwin") {
    f <- pipe("pbpaste")
  } else if (sn == "Windows") {
    f <- file(description = 'clipboard', open = 'r')
  } else {
    stop("Reading from the clipboard is not implemented for your system (",
         sn, ") in this package.")
  }
  
  characterVector <- scan(f, what = "character")
  close(f)
  
  # Format string ready for inclusion in c(...).
  formattedString <- paste(characterVector , collapse="\", \"")
  formattedString <- paste("\"", formattedString, "\"", sep="")
  if (addC) formattedString <- paste("c(", formattedString , ")", sep = "")
  writeLines(formattedString, con = "clipboard-128", sep = " ")
}

#' @rdname V
#' @export
V_tab <- function(addC = TRUE) {
  # http://www.r-bloggers.com/efficient-variable-selection-in-r/
  # Typical application:
  # Step 1: paste a column of variable names from a meta database;
  # Step 2: Run this function in R: V_tab()
  # Step 3: Press control + V: Paste vector of clipboard
  
  sn <- Sys.info()["sysname"]
  if (sn == "Darwin") {
    f <- pipe("pbpaste")
  } else if (sn == "Windows") {
    f <- file(description = 'clipboard', open = 'r')
  } else {
    stop("Reading from the clipboard is not implemented for your system (",
         sn, ") in this package.")
  }
  
  characterVector <- scan("clipboard", what = "character", sep="\t")
  close(f)
  
  # Format string ready for inclusion in c(...).
  formattedString <-    paste(characterVector , collapse="\", \"")
  formattedString <- paste("\"", formattedString, "\"", sep="")
  if (addC) formattedString <- paste("c(", formattedString , ")", sep = "")
  writeLines(formattedString, con = "clipboard-128", sep = " ")
}
