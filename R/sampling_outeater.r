sampling_outeater <- function (panel_id=panel_id_active, sampleN,
                               sent_id = NULL,
                               finished_id = NULL) {
  
  cat("¥x¥_:\t")
  id_TP <- panel_id %>%
    filter(aream_name %in% c("»O¥_¥«")) %>%           # city
    filter(gender %in% sampleN$TP[["gender"]]) %>%    # gender
    filter(age %in% sampleN$TP[["age"]]) %>%          # age
    sample_N(sampleN$TP$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$TP$include_sent,
             show = T)    # Sample N id
  
  cat("·s¥_:\t")
  id_NewTP <- panel_id %>%
    filter(aream_name %in% c("·s¥_¥«")) %>%            # city
    filter(gender %in% sampleN$NewTP[["gender"]]) %>% # gender
    filter(age %in% sampleN$NewTP[["age"]]) %>%        # age
    sample_N(sampleN$NewTP$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$NewTP$include_sent,
             show = T)     # Sample N id
  
  cat("°ò©y:\t")
  id_KL_YL <- panel_id %>%
    filter(aream_name %in% c("°ò¶©¥«", "©yÄõ¿¤")) %>% # city
    filter(gender %in% sampleN$KL_YL[["gender"]]) %>%    # gender
    filter(age %in% sampleN$KL_YL[["age"]]) %>%       # age
    sample_N(sampleN$KL_YL$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$KL_YL$include_sent,
             show = T)   # Sample N id
  
  cat("®ç¦Ë­]:\t")
  id_TY_XC_ML <- panel_id %>%
    filter(aream_name %in% c("®ç¶é¥«", "·s¦Ë¥«",
                             "·s¦Ë¿¤", "­]®ß¿¤")) %>%  # city
    filter(gender %in% sampleN$TY_XC_ML[["gender"]]) %>%     # gender
    filter(age %in% sampleN$TY_XC_ML[["age"]]) %>%     # age
    sample_N(sampleN$TY_XC_ML$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$TY_XC_ML$include_sent,
             show = T)   # Sample N id
  
  cat("¤¤¹ü§ë:\t")
  id_TCH_CHW_NT <- panel_id %>%
    filter(aream_name %in% c("»O¤¤¥«", "¹ü¤Æ¿¤", "«n§ë¿¤")) %>%  # city
    filter(gender %in% sampleN$TCH_CHW_NT[["gender"]]) %>%     # gender
    filter(age %in% sampleN$TCH_CHW_NT[["age"]]) %>%   # age
    sample_N(sampleN$TCH_CHW_NT$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$TCH_CHW_NT$include_sent,
             show = T)  # Sample N id
  
  cat("¶³¹Å«n:\t")
  id_YL_CHY_TN <- panel_id %>%
    filter(aream_name %in% c("¶³ªL¿¤", "¹Å¸q¥«", "¹Å¸q¿¤","»O«n¥«")) %>% # city
    filter(gender %in% sampleN$YL_CHY_TN[["gender"]]) %>%    # gender
    filter(age %in% sampleN$YL_CHY_TN[["age"]]) %>%   # age
    sample_N(sampleN$YL_CHY_TN$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$YL_CHY_TN$include_sent,
             show = T)    # Sample N id
  
  cat("°ª«Ì:\t")
  id_KS_PT <- panel_id %>%
    filter(aream_name %in% c("°ª¶¯¥«", "«ÌªF¿¤")) %>% # city
    filter(gender %in% sampleN$KS_PT[["gender"]]) %>%    # gender
    filter(age %in% sampleN$KS_PT[["age"]]) %>%       # age
    sample_N(sampleN$KS_PT$N,
             sent_id = sent_id,
             finished_id = finished_id,
             include_sent = sampleN$KS_PT$include_sent,
             show = T)     # Sample N id
  
  cat("ªáªF:\t")
  id_HWL_TD <- panel_id %>%
    filter(aream_name %in% c("ªá½¬¿¤", "»OªF¿¤")) %>% # city
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
