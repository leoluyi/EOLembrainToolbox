id_generator <- function(n, key) {
  paste0(toupper(key), 
         stringr::str_pad(sample(900000, n, replace = FALSE), 
                          width = 11, pad = "0"))
}
