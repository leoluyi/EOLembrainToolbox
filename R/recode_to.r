#' Recode variables 
#' 
#' @param df A data.frame object
#' @param ... Comma separated list of unquoted expressions. See examples.
#' @param match Variables matching  sequence. "first" or "last"
#' @examples
#' recode_to(mtcars,
#'   mpg < 20 ~ "a",
#'   vs == 1  ~ "b"
#' )
#' recode_to(mtcars,
#'   mpg < 20 ~ "a",
#'   vs == 1  ~ "b",
#'            ~ "c"
#' )
#' recode_to(mtcars,
#'   mpg < 20 ~ mpg,
#'            ~ mpg + 100
#' )
#' @references 
#' forked from hadley/recode.R
#' https://gist.github.com/hadley/2751ba61d1c7f4eaacab

#' @export
recode_to <- function(df, ..., match = c("first", "last")) {
  
  match <- match.arg(match)

  cases <- lapply(list(...), as.case)
  if (identical(match, "last")) cases <- rev(cases)

  n <- nrow(df)
  out <- rep(NA, length(n)) # logical will be upcast as needed

  # Simple loop-y implementation
  for (i in seq_len(n)) {
    row <- df[i, ]

    for (j in seq_along(cases)) {
      case <- cases[[j]]
      res <- eval(case$expr, row, case$env)

      if (isTRUE(res)) {
        val <- eval(case$val, row, case$env)
        out[[i]] <- val
        break
      }
    }
  }

  out
}

# Case data structure ----------------------------------------------------------

case <- function(expr, val, env) {
  structure(list(expr = expr, val = val, env = env), class = "case")
}

#' @export
as.case <- function(x) UseMethod("as.case")

#' @export
as.case.case <- function(x) x

#' @export
as.case.formula <- function(x) {
  if (length(x) == 3) {
    case(x[[2]], x[[3]], environment(x))
  } else if (length(x) == 2) {
    case(TRUE, x[[2]], environment(x))
  } else {
    stop("Invalid formula")
  }
}

#' @export
print.case <- function(x, ...) {
  cat("<case>\n")
  cat(" expr: ", deparse(x$expr), "\n", sep = "")
  cat(" val:  ", x$val, "\n", sep = "")
  cat(" env:  ",  format(x$env), "\n", sep = "")
}


