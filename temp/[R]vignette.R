devtools::source_url("https://github.com/leo-luyi/get_ids/raw/master/OutEater.R", encoding="UTF-8") # OutEater.R

library(plyr)
library(dplyr)
library(readr)
library(stringr) # for `str_sub`
library(tools)


## Panel total ID
panel_id_all <- read_panel_id("https://github.com/leo-luyi/get_ids/raw/master/data/panel_2015-06-08.csv")


# Import exclude_id files ---------------------------------------------------

## read files
sent_id <- plyr::llply(list.files(path="./exclude_id/", 
                                  pattern = "\\.txt$",
                                  recursive = TRUE,
                                  include.dirs = FALSE,
                                  full.names=TRUE), # path
                       read_table,
                       col_names = F,
                       col_types = "c") %>% unlist %>% unname

finished_id <- plyr::llply(list.files(path="./exclude_id/finished_ID/",
                                      pattern = "\\.txt$",
                                      include.dirs = FALSE,
                                      full.names=TRUE), # path
                           read_table,
                           col_names = F,
                           col_types = "c") %>% unlist %>% unname

panel_id_all %>% filter(active_grade %in% 1,
                        gender==2,
                        age %in% 28:32)

# get ids --------------------------------------------------------------------

## view area
# levels(panel_id_all$aream_name) %>% as.data.frame 

panel_id <- panel_id_all %>%
#   mutate(area_NCSE = ifelse(aream %in% c(1:6, 17), "N",
#                             ifelse(aream %in% c(7:10,18) , "C",
#                                    ifelse(aream %in% c(12:16,19), "S", NA))),
#          area_NCSE = factor(area_NCSE, levels = c("N", "C", "S"))) %>%
  filter(active_grade %in% 1:2)  # active, semi-active

## sampling
id_1 <- panel_id %>% 
  filter(gender==2, age %in% 15:24) %>%
  sample_N(147, sent_id = sent_id) 
id_2 <- panel_id %>% 
  filter(gender==2, age %in% 25:39) %>%
  sample_N(231, sent_id = sent_id) 



# write txt ------------------------------------------------------------------

c(id_1, id_2) %>%
  write.table(file = paste0("sent_id_", Sys.Date(), ".txt", collapse=""),
              quote=F, 
              row.names=F,
              col.names=F)

# write ids ------------------------------------------------------------

# n_sent_id("./exclude_id/", date_from = "2015-05-01", date_to = "2015-05-31")
