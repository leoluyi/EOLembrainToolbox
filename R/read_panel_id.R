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
      "�򶩥�" = "�򶩥�",
      "�O�_��" = "�O�_��",
      "�s�_��" = "�s�_��",
      "��饫" = "��鿤",
      "�s�˿�" = "�s�˿�",
      "�s�˥�" = "�s�˥�",
      "�]�߿�" = "�]�߿�",
      "�O����" = "�O����(��O����)",
      "�O����" = "�O����(��O����)",
      "���ƿ�" = "���ƿ�",
      "�n�뿤" = "�n�뿤",
      "���L��" = "���L��",
      "�Ÿq��" = "�Ÿq��",
      "�Ÿq��" = "�Ÿq��",
      "�O�n��" = "�O�n��(��O�n��)",
      "�O�n��" = "�O�n��(��O�n��)",
      "������" = "������(�찪����)",
      "������" = "������(�찪����)",
      "�̪F��" = "�̪F��",
      "�y����" = "�y����",
      "�Ὤ��" = "�Ὤ��",
      "�O�F��" = "�O�F��",
      "���" = "���",
      "������" = "������",
      "�s����" = "�s����",
      "�n���Ѯq" = "�n���Ѯq",
      "�����O�C��" = "�����O�C��")
  
  panel <- panel %>%
    dplyr::mutate(aream = as.numeric(aream_name))
  
  panel
}

