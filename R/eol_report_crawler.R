#' Get Panel Supply IDs report
#'
#' @param survey_id ISAS survey id.
#'
#' @return a \code{tbl_df} object
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' 
#' # settings ----------------------------------------------------------------
#' 
#' survey_id <- 4026
#' n_test <- 6   # test ids
#' 
#' # count -------------------------------------------------------------------
#' 
#' result_table <- eol_report_crawler(survey_id) %>% 
#'     slice(-c(1:n_test))
#'    
#' tail(result_table, 10)
#' table(result_table$Result)
#' 
#' 
#' # export to xlsx ----------------------------------------------------------
#' 
#' xlsx::write.xlsx(as.data.frame(result_table, stringsAsFactors=FALSE),
#'                  file=sprintf("final_id_%s.xlsx", survey_id),
#'                  sheetName="ISAS", row.names=FALSE, append=FALSE)
#' 
#' 
#' }
eol_report_crawler <- function (survey_id) {
  url <- "http://survey.panelpower.com.tw/isasextension/adm/report.aspx"

  form_data <- list(
    `__VIEWSTATE` = "/wEPDwUKMTk5OTU5MzU1NA9kFgICAw9kFgICBQ88KwANAQAPFgQeC18hRGF0YUJvdW5kZx4LXyFJdGVtQ291bnRmZGQYAQUJR3JpZFZpZXcxDzwrAAoBCGZk3vREjfSh4Rh1B/azM/FDGmWgen0=",
    `__VIEWSTATEGENERATOR` = "18D4358D",
    `__EVENTVALIDATION` = "/wEWAwK5w9GEBALs0bLrBgKM54rGBqH77yjzL79QAzfOM9nu8Li47RLR",
    `TextBox1` = as.character(survey_id)
  )

  result <- httr::POST(url,
                     body = form_data,
                     encode = "form")

  result_content_text <- httr::content(result, type = "text", encoding = "UTF-8")
  result_content <- XML::htmlParse(result_content_text, encoding = "UTF-8")
  # result_table <- xpathSApply(result_content, "//table", xmlValue, encoding = "UTF-8")

  table_parse_list <- XML::readHTMLTable(result_content)
  if (length(table_parse_list) == 0) {
    cat("no result\n")
    return()
  }

  result_table <- dplyr::as_data_frame(table_parse_list[[1]])

  names(result_table) <- gsub("\\s", "_",names(result_table))

  result_table <- result_table %>%
    dplyr::mutate(End_date = stringr::str_extract(End_time,
                                                  "^[0-9]{4}\\/(1[012]|[1-9])\\/[0-9]+")) %>%
    dplyr::mutate(End_time =  stringr::str_extract(End_time, "[\\u4e0a\\u4e0b]\\u5348.+$"))
  
  result_table <- lapply(result_table, as.character) %>%
    dplyr::as_data_frame(.) %>%
    dplyr::mutate(End_date = as.Date(End_date, "%Y/%m/%e"))
  
  result_table
}


