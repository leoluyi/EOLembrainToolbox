syntax_crawler <- function (survey_id) {
  url <- "http://survey.panelpower.com.tw/isasextension/adm/CreatSyntax.aspx"

  headers = httr::add_headers("User-Agent"="Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.157 Safari/537.36",
                        "Referer" = "http://survey.panelpower.com.tw/isasextension/adm/CreatSyntax.aspx")

  form_data <- list(
    `__VIEWSTATE` = "/wEPDwUJLTUwNTY2NzEyZGS6LMG+apoJ1k/Ouglfo9SU/aGF1A==",
    `__VIEWSTATEGENERATOR` = "48D8CC54",
    `__EVENTVALIDATION` = "/wEWBgL84IV7AqCcpdIEAoznisYGAuzRsusGAuzR9tkMAuzRirUFSGC8FVqsc/xH/u5xtddHNh7V8/U=",
    `txtSurveyID` = as.character(survey_id),
    `Button1` = "%AB%D8%A5%DF"
  )

  result <- httr::POST(url,
                       headers,
                       body = form_data,
                       encode = "form")

  result_content_text <- httr::content(result, type = "text", encoding = "UTF-8")
  result_content <- XML::htmlParse(result_content_text, encoding = "UTF-8")
  result_text <- XML::xpathSApply(result_content, "//textarea[@id='TextBox3']", XML::xmlValue, encoding = "UTF-8")
  result_text <- unlist(strsplit(result_text, split = "(cross table)"))

  if(length(result_text) == 0) {
    cat("no result\n")
    return()
  }
  
  output <- gsub("\\\r\\\n", "\n", result_text)
  output[[1]] <- gsub("\\s+$", "\n.\n", output[[1]])

  dir.create("./syntax_crawler", showWarnings = FALSE)
  con <- file(sprintf("./syntax_crawler/1_label_%s.sps", survey_id), encoding = "UTF-8")
  cat(output[[1]], file=con); close(con)
  con <- file(sprintf("./syntax_crawler/variables_%s.txt", survey_id), encoding = "UTF-8")
  cat(output[[2]], file=con); close(con)
  cat("getting label finished")
}

