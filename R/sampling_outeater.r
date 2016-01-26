#' Outeater Sampling
#'
#' @param panel_id
#' @param sampleN
#' @param sent_id
#' @param finished_id
#'
#' @return character vector
#' @export
#'
#' @examples
#' sampleN <- list(
#' TP = list(N = 20, age = c(35:59), gender=1:2, include_sent=F),
#' NewTP = list(N = 50, age = c(15:19, 35:59), gender=1:2, include_sent=F),
#' KL_YL = list(N = 30, age = c(15:59), gender=1:2, include_sent=T),
#' TY_XC_ML = list(N = 100, age = c(15:20, 35:59), gender=1:2, include_sent=F),
#' TCH_CHW_NT = list(N = 130, age = c(15:20, 35:59), gender=1:2, include_sent=F),
#' YL_CHY_TN = list(N = 100, age = c(15:25, 35:59), gender=1:2, include_sent=F),
#' KS_PT = list(N = 100, age = c(15:20, 35:59), gender=1:2, include_sent=F),
#' HWL_TD = list(N = 20, age = c(15:49), gender=1:2, include_sent=T)
#' )
#'
#' id_output <- panel_id_all %>%
#' filter(active_grade %in% 1:2) %>%  # active, semi-active
#'   sampling_outeater(sampleN,
#'                     sent_id,
#'                     finished_id)
#'
#'
sampling_outeater <- function (panel_id=panel_id_active, sampleN,
                               sent_id = NULL,
                               finished_id = NULL) {

  cat("\u53f0\u5317:\t")
  id_TP <- panel_id %>%
    filter(aream_name %in% c("\u81fa\u5317\u5e02")) %>%           # city
    filter(gender %in% sampleN$TP[["gender"]]) %>%    # gender
    filter(age %in% sampleN$TP[["age"]]) %>%          # age
    sample_N(sampleN$TP$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$TP$include_sent,
             show = T)    # Sample N id

  cat("\u65b0\u5317:\t")
  id_NewTP <- panel_id %>%
    filter(aream_name %in% c("\u65b0\u5317\u5e02")) %>%            # city
    filter(gender %in% sampleN$NewTP[["gender"]]) %>% # gender
    filter(age %in% sampleN$NewTP[["age"]]) %>%        # age
    sample_N(sampleN$NewTP$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$NewTP$include_sent,
             show = T)     # Sample N id

  cat("\u57fa\u5b9c:\t")
  id_KL_YL <- panel_id %>%
    filter(aream_name %in% c("\u57fa\u9686\u5e02", "\u5b9c\u862d\u7e23")) %>% # city
    filter(gender %in% sampleN$KL_YL[["gender"]]) %>%    # gender
    filter(age %in% sampleN$KL_YL[["age"]]) %>%       # age
    sample_N(sampleN$KL_YL$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$KL_YL$include_sent,
             show = T)   # Sample N id

  cat("\u6843\u7af9\u82d7:\t")
  id_TY_XC_ML <- panel_id %>%
    filter(aream_name %in% c("\u6843\u5712\u5e02", "\u65b0\u7af9\u5e02",
                             "\u65b0\u7af9\u7e2", "\u82d7\u6817\u7e23")) %>%  # city
    filter(gender %in% sampleN$TY_XC_ML[["gender"]]) %>%     # gender
    filter(age %in% sampleN$TY_XC_ML[["age"]]) %>%     # age
    sample_N(sampleN$TY_XC_ML$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$TY_XC_ML$include_sent,
             show = T)   # Sample N id

  cat("\u4e2d\u5f70\u6295:\t")
  id_TCH_CHW_NT <- panel_id %>%
    filter(aream_name %in% c("\u81fa\u4e2d\u5e02", "\u5f70\u5316\u7e23", "\u5357\u6295\u7e23")) %>%  # city
    filter(gender %in% sampleN$TCH_CHW_NT[["gender"]]) %>%     # gender
    filter(age %in% sampleN$TCH_CHW_NT[["age"]]) %>%   # age
    sample_N(sampleN$TCH_CHW_NT$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$TCH_CHW_NT$include_sent,
             show = T)  # Sample N id

  cat("\u96f2\u5609\u5357:\t")
  id_YL_CHY_TN <- panel_id %>%
    filter(aream_name %in% c("\u96f2\u6797\u7e23", "\u5609\u7fa9\u5e02",
                             "\u5609\u7fa9\u7e2","\u81fa\u5357\u5e02")) %>% # city
    filter(gender %in% sampleN$YL_CHY_TN[["gender"]]) %>%    # gender
    filter(age %in% sampleN$YL_CHY_TN[["age"]]) %>%   # age
    sample_N(sampleN$YL_CHY_TN$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$YL_CHY_TN$include_sent,
             show = T)    # Sample N id

  cat("\u9ad8\u5c4f:\t")
  id_KS_PT <- panel_id %>%
    filter(aream_name %in% c("\u9ad8\u96c4\u5e02", "\u5c4f\u6771\u7e23")) %>% # city
    filter(gender %in% sampleN$KS_PT[["gender"]]) %>%    # gender
    filter(age %in% sampleN$KS_PT[["age"]]) %>%       # age
    sample_N(sampleN$KS_PT$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$KS_PT$include_sent,
             show = T)     # Sample N id

  cat("\u82b1\u6771:\t")
  id_HWL_TD <- panel_id %>%
    filter(aream_name %in% c("\u82b1\u84ee\u7e23", "\u81fa\u6771\u7e2")) %>% # city
    filter(gender %in% sampleN$HWL_TD[["gender"]]) %>%    # gender
    filter(age %in% sampleN$HWL_TD[["age"]]) %>%      # age
    sample_N(sampleN$HWL_TD$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$HWL_TD$include_sent,
             show = T)    # Sample N id

  id_output <- c(id_TP,
                 id_NewTP,
                 id_KL_YL,
                 id_TY_XC_ML,
                 id_TCH_CHW_NT,
                 id_YL_CHY_TN,
                 id_KS_PT,
                 id_HWL_TD)
  id_output
}
