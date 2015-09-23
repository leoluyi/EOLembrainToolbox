#' Read panel IDs
#'
#' @param path Either "lib_path" for built-in data, or "update" to update first.
#' 
#' @import dplyr
#' @return \code{tbl_df} object of panel data
#' 
#' @export
read_panel_id <- function(path = c("lib_path", "update")) {
  
  path <- match.arg(path)
  
  if (path == "update") EOLembrainToolbox::panel_id_crawler() # update data
  
  if (path == "lib_path") 
    path <- system.file("extdata/panel_data.csv", package = "EOLembrainToolbox")
  
  # check extension: csv
  if (tolower(tools::file_ext(path)) != "csv") 
    stop("the extention of panel id file must be .csv", call. = FALSE)
  
  ## read original panel file
  panel <- read.csv(con <- file(path, encoding = "UTF-8-BOM"),
                    fileEncoding="UTF-8",
                    colClasses = "character",
                    stringsAsFactors = FALSE) %>% dplyr::as_data_frame()
  closeAllConnections()
  
  panel[,c(2, 4:length(panel))] <-
    lapply(panel[c(2, 4:length(panel))], as.factor) %>%
    dplyr::as_data_frame()
  
  panel <- panel %>%
    dplyr::mutate(age = as.numeric(age)) %>%
    dplyr::mutate(aream = as.numeric(aream)) %>%
    dplyr::mutate(aream_name = factor(aream_name))
  
  #   area_table <- panel %>%
  #     select(aream, aream_name) %>%
  #     distinct %>%
  #     arrange(aream)
  
  ## Changing the order of levels of a factor
  
  levels(panel$aream_name) <-
    list(
      "\u57fa\u9686\u5e02" = "\u57fa\u9686\u5e02",
      "\u81fa\u5317\u5e02" = "\u81fa\u5317\u5e02",
      "\u65b0\u5317\u5e02" = "\u65b0\u5317\u5e02",
      "\u6843\u5712\u5e02" = "\u6843\u5712\u7e23",
      "\u65b0\u7af9\u7e23" = "\u65b0\u7af9\u7e23",
      "\u65b0\u7af9\u5e02" = "\u65b0\u7af9\u5e02",
      "\u82d7\u6817\u7e23" = "\u82d7\u6817\u7e23",
      "\u81fa\u4e2d\u5e02" = "\u81fa\u4e2d\u5e02(\u539f\u81fa\u4e2d\u5e02)",
      "\u81fa\u4e2d\u5e02" = "\u81fa\u4e2d\u5e02(\u539f\u81fa\u4e2d\u7e23)",
      "\u5f70\u5316\u7e23" = "\u5f70\u5316\u7e23",
      "\u5357\u6295\u7e23" = "\u5357\u6295\u7e23",
      "\u96f2\u6797\u7e23" = "\u96f2\u6797\u7e23",
      "\u5609\u7fa9\u7e23" = "\u5609\u7fa9\u7e23",
      "\u5609\u7fa9\u5e02" = "\u5609\u7fa9\u5e02",
      "\u81fa\u5357\u5e02" = "\u81fa\u5357\u5e02(\u539f\u81fa\u5357\u5e02)",
      "\u81fa\u5357\u5e02" = "\u81fa\u5357\u5e02(\u539f\u81fa\u5357\u7e23)",
      "\u9ad8\u96c4\u5e02" = "\u9ad8\u96c4\u5e02(\u539f\u9ad8\u96c4\u7e23)",
      "\u9ad8\u96c4\u5e02" = "\u9ad8\u96c4\u5e02(\u539f\u9ad8\u96c4\u5e02)",
      "\u5c4f\u6771\u7e23" = "\u5c4f\u6771\u7e23",
      "\u5b9c\u862d\u7e23" = "\u5b9c\u862d\u7e23",
      "\u82b1\u84ee\u7e23" = "\u82b1\u84ee\u7e23",
      "\u81fa\u6771\u7e23" = "\u81fa\u6771\u7e23",
      "\u6f8e\u6e56\u7e23" = "\u6f8e\u6e56\u7e23",
      "\u91d1\u9580\u7e23" = "\u91d1\u9580\u7e23",
      "\u9023\u6c5f\u7e23" = "\u9023\u6c5f\u7e23",
      "\u5357\u6d77\u8af8\u5cf6" = "\u5357\u6d77\u8af8\u5cf6",
      "\u91e3\u9b5a\u81fa\u5217\u5dbc" = "\u91e3\u9b5a\u81fa\u5217\u5dbc")
  
  panel <- panel %>%
    dplyr::mutate(aream = as.numeric(aream_name))
  
  panel
}

