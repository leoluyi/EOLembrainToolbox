#' panel id crawler
#' 
#' save .csv file to system.file("extdata/panel_data.csv", package = "EOLembrainToolbox")
#' 
#' @import dplyr httr XML
#'
panel_id_crawler <- function () {
  url <- "http://survey.panelpower.com.tw/isasextension/adm/panelid_query.aspx"

  headers = add_headers("User-Agent"="Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.157 Safari/537.36",
                        "Referer" = "http://survey.panelpower.com.tw/isasextension/adm/panelid_query.aspx")

  form_data <- list(
    `__VIEWSTATE` = "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",
    `__VIEWSTATEGENERATOR` = "B696C301",
    `__EVENTVALIDATION` = "/wEW7gECqNT0+Q0CpNiAuAECu9iAuAECutiAuAEC3rCNsQQCwbCNsQQCwLCNsQQCw7CNsQQCwrCNsQQCxbCNsQQCxLCNsQQCx7CNsQQC1rCNsQQC2bCNsQQCwbDNsgQCwbDBsgQCwbDFsgQCwbD5sgQCwbD9sgQCwbDxsgQCwbD1sgQCwbDpsgQCwbCtsQQCwbChsQQCwLDNsgQCwLDBsgQCwLDFsgQCwLD5sgQCwLD9sgQCwLDxsgQCwLD1sgQCwLDpsgQCwLCtsQQCwLChsQQCw7DNsgQCw7DBsgQCw7DFsgQCw7D5sgQCw7D9sgQCw7DxsgQCw7D1sgQCw7DpsgQCw7CtsQQCw7ChsQQCwrDNsgQCwrDBsgQCwrDFsgQCwrD5sgQCwrD9sgQCwrDxsgQCwrD1sgQCwrDpsgQCwrCtsQQCwrChsQQCxbDNsgQCxbDBsgQCxbDFsgQCxbD5sgQCxbD9sgQCxbDxsgQCxbD1sgQCxbDpsgQCxbCtsQQCxbChsQQCxLDNsgQCxLDBsgQCxLDFsgQCxLD5sgQCxLD9sgQCxLDxsgQCxLD1sgQCxLDpsgQCxLCtsQQCxLChsQQCx7DNsgQCx7DBsgQCx7DFsgQCx7D5sgQCx7D9sgQCx7DxsgQCx7D1sgQCx7DpsgQCx7CtsQQCx7ChsQQC1rDNsgQC1rDBsgQC1rDFsgQC1rD5sgQC1rD9sgQC1rDxsgQC1rD1sgQC1rDpsgQC1rCtsQQC1rChsQQC2bDNsgQC2bDBsgQC2bDFsgQC2bD5sgQC2bD9sgQC2bDxsgQC7czk8QYC8szk8QYC88zk8QYC8Mzk8QYC8czk8QYC9szk8QYC98zk8QYC9Mzk8QYC5czk8QYC6szk8QYC8syk8gYC8syo8gYC8sys8gYC8syQ8gYC8syU8gYC8syY8gYC8syc8gYC8syA8gYC8szE8QYC8szI8QYC88yk8gYC88yo8gYC88ys8gYC88yQ8gYC88yU8gYC88yY8gYC88yc8gYC88yA8gYC88zE8QYC88zI8QYC8Myk8gYC8Myo8gYC8Mys8gYC8MyQ8gYC8MyU8gYC8MyY8gYC8Myc8gYC8MyA8gYC8MzE8QYC8MzI8QYC8cyk8gYC8cyo8gYC8cys8gYC8cyQ8gYC8cyU8gYC8cyY8gYC8cyc8gYC8cyA8gYC8czE8QYC8czI8QYC9syk8gYC9syo8gYC9sys8gYC9syQ8gYC9syU8gYC9syY8gYC9syc8gYC9syA8gYC9szE8QYC9szI8QYC98yk8gYC98yo8gYC98ys8gYC98yQ8gYC98yU8gYC98yY8gYC98yc8gYC98yA8gYC98zE8QYC98zI8QYC9Myk8gYC9Myo8gYC9Mys8gYC9MyQ8gYC9MyU8gYC9MyY8gYC9Myc8gYC9MyA8gYC9MzE8QYC9MzI8QYC5cyk8gYC5cyo8gYC5cys8gYC5cyQ8gYC5cyU8gYC5cyY8gYC5cyc8gYC5cyA8gYC5czE8QYC5czI8QYC6syk8gYC6syo8gYC6sys8gYC6syQ8gYC6syU8gYC6syY8gYC6syc8gYC6syA8gYC6szE8QYC6szI8QYCjJOB2A8CsNjU6QsCr9jU6QsCrtjU6QsC9urC5gcC7rKnwggCy6XF6QIC0Mzq9AQCvfaIggkCmpmuqQMC54DMtAUCzKvSww8C+cGSnQcCxuiwqAkCgYuFtwYC7rKrwggCy6XJ6QIC0Mzu9AQCvfaMggkCmpmSqQMC54CwtAUCzKvWww8C+cGWnQcCxui0qAkCgYuJtwYC7rKvwggCy6XN6QIC0MzS9AQCvfbwgwkCmpmWqQMC54C0tAUCzKvaww8CqZyI3A8Cz4m0qggC0Im0qggC0Ym0qggC372awAYC7468vwVsHLfHsLu27qaMrZ3uCTWPV31Aww==",
    ddlGender="0",
    ddlAge1="0",
    ddlAge2="0",
    ddlWedding="0",
    ddlAreaM="all",
    btnQuery="%E6%9F%A5%E8%A9%A2"
  )

  result <- httr::POST(url,
                       headers,
                       body = form_data,
                       encode = "form")

  result_content_text <- content(result, type = "text", encoding = "UTF-8")
  result_content <- htmlParse(result_content_text, encoding = "UTF-8")
  result_text <- xpathSApply(result_content, "//a[@id='hylDownload']", xmlValue, encoding = "UTF-8")


  url_parse <- parse_url(url)

  url_parse$path <-
    sprintf("isasextension/adm/%s",
            xmlGetAttr(getNodeSet(result_content, "//a[@id='hylDownload']")[[1]], "href"))

  data_url <- gsub("\\\\", "/", build_url(url_parse))

  # download.file(data_url, destfile = file.path(tempdir(), basename(data_url)))
  data_path <- system.file("extdata/panel_data.csv", package = "EOLembrainToolbox")
  download.file(data_url, destfile = data_path)
  # EOLembrainToolbox::read_panel_id(file.path(tempdir(), basename(data_url)))

  invisible()
}


