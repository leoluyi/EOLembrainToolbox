read_panel_id <- function(path = "lib_path") {
  
  if(path == "lib_path") 
    path <- system.file("extdata/panel_data.csv", package = "EOLembrainToolbox")
  
  # check extension: csv
  if(tolower(tools::file_ext(path)) != "csv") 
    stop("the extention of panel id file must be .csv", call. = FALSE)
  
  
  ## read original panel file
  panel <- readr::read_csv(
    path,
    col_names = TRUE,
    col_types = paste0(rep("c", 8), collapse = ""),
    na = "")
  
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
      "基隆市" = "基隆市",
      "臺北市" = "臺北市",
      "新北市" = "新北市",
      "桃園市" = "桃園縣",
      "新竹縣" = "新竹縣",
      "新竹市" = "新竹市",
      "苗栗縣" = "苗栗縣",
      "臺中市" = "臺中市(原臺中市)",
      "臺中市" = "臺中市(原臺中縣)",
      "彰化縣" = "彰化縣",
      "南投縣" = "南投縣",
      "雲林縣" = "雲林縣",
      "嘉義縣" = "嘉義縣",
      "嘉義市" = "嘉義市",
      "臺南市" = "臺南市(原臺南市)",
      "臺南市" = "臺南市(原臺南縣)",
      "高雄市" = "高雄市(原高雄縣)",
      "高雄市" = "高雄市(原高雄市)",
      "屏東縣" = "屏東縣",
      "宜蘭縣" = "宜蘭縣",
      "花蓮縣" = "花蓮縣",
      "臺東縣" = "臺東縣",
      "澎湖縣" = "澎湖縣",
      "金門縣" = "金門縣",
      "連江縣" = "連江縣",
      "南海諸島" = "南海諸島",
      "釣魚臺列嶼" = "釣魚臺列嶼")
  
  panel <- panel %>%
    dplyr::mutate(aream = as.numeric(aream_name))
  
  panel
}

