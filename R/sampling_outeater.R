sampling_outeater <- function (panel_id=panel_id_active, sampleN,
                               sent_id = NULL,
                               finished_id = NULL) {
  
  cat("台北:\t")
  id_TP <- panel_id %>%
    filter(aream_name %in% c("臺北市")) %>%           # city
    filter(gender %in% sampleN$TP[["gender"]]) %>%    # gender
    filter(age %in% sampleN$TP[["age"]]) %>%          # age
    sample_N(sampleN$TP$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$TP$include_sent,
             show = T)    # Sample N id
  
  cat("新北:\t")
  id_NewTP <- panel_id %>%
    filter(aream_name %in% c("新北市")) %>%            # city
    filter(gender %in% sampleN$NewTP[["gender"]]) %>% # gender
    filter(age %in% sampleN$NewTP[["age"]]) %>%        # age
    sample_N(sampleN$NewTP$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$NewTP$include_sent,
             show = T)     # Sample N id
  
  cat("基宜:\t")
  id_KL_YL <- panel_id %>%
    filter(aream_name %in% c("基隆市", "宜蘭縣")) %>% # city
    filter(gender %in% sampleN$KL_YL[["gender"]]) %>%    # gender
    filter(age %in% sampleN$KL_YL[["age"]]) %>%       # age
    sample_N(sampleN$KL_YL$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$KL_YL$include_sent,
             show = T)   # Sample N id
  
  cat("桃竹苗:\t")
  id_TY_XC_ML <- panel_id %>%
    filter(aream_name %in% c("桃園市", "新竹市",
                             "新竹縣", "苗栗縣")) %>%  # city
    filter(gender %in% sampleN$TY_XC_ML[["gender"]]) %>%     # gender
    filter(age %in% sampleN$TY_XC_ML[["age"]]) %>%     # age
    sample_N(sampleN$TY_XC_ML$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$TY_XC_ML$include_sent,
             show = T)   # Sample N id
  
  cat("中彰投:\t")
  id_TCH_CHW_NT <- panel_id %>%
    filter(aream_name %in% c("臺中市", "彰化縣", "南投縣")) %>%  # city
    filter(gender %in% sampleN$TCH_CHW_NT[["gender"]]) %>%     # gender
    filter(age %in% sampleN$TCH_CHW_NT[["age"]]) %>%   # age
    sample_N(sampleN$TCH_CHW_NT$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$TCH_CHW_NT$include_sent,
             show = T)  # Sample N id
  
  cat("雲嘉南:\t")
  id_YL_CHY_TN <- panel_id %>%
    filter(aream_name %in% c("雲林縣", "嘉義市", "嘉義縣","臺南市")) %>% # city
    filter(gender %in% sampleN$YL_CHY_TN[["gender"]]) %>%    # gender
    filter(age %in% sampleN$YL_CHY_TN[["age"]]) %>%   # age
    sample_N(sampleN$YL_CHY_TN$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$YL_CHY_TN$include_sent,
             show = T)    # Sample N id
  
  cat("高屏:\t")
  id_KS_PT <- panel_id %>%
    filter(aream_name %in% c("高雄市", "屏東縣")) %>% # city
    filter(gender %in% sampleN$KS_PT[["gender"]]) %>%    # gender
    filter(age %in% sampleN$KS_PT[["age"]]) %>%       # age
    sample_N(sampleN$KS_PT$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$KS_PT$include_sent,
             show = T)     # Sample N id
  
  cat("花東:\t")
  id_HWL_TD <- panel_id %>%
    filter(aream_name %in% c("花蓮縣", "臺東縣")) %>% # city
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
}
