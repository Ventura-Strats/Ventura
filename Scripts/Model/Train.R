####################################################################################################
### Prepares models, run after we downloaded latest prices and recomputed technicals
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Train"
max_time_hours <<- 2

script_arg <<- as.integer(commandArgs(trailingOnly = TRUE)[2])
use_weights <- script_arg > 4
script_arg_1 <- as.character(script_arg %% 4)

strats_list <- switch(
  script_arg_1,
  "1" = c(4,6,11),
  "2" = c(1,10,13,7,9),
  "3" = c(2,3,5,12,8),
  "0" = 14
)

# 1   4162  
# 2   2074  
# 3   1314  
# 4   8880  
# 5   1958  
# 6   6474  
# 7   2604  
# 8   2533  
# 9   3096  
# 10  3330  
# 11  6652  
# 12  1882  
# 13  3130  
# 14  20014 

####################################################################################################
### Sub routines
####################################################################################################
backupYesterdayModels <- function() {
  if (script_arg == 1) {
    file_list <- paste0(DIRECTORY_DATA_SD, "Models") %>% 
      list.files("model_", full.names = TRUE)
    
    file_list_old_backup <- file_list[grepl("backup", file_list)]
    
  #  files_to_remove <- file_list_old_backup[which(file.mtime(file_list_old_backup) < (TO_DAY - 3))]
    
    dates_list <- file_list[grepl(paste0(DIRECTORY_DATA_SD, "Models/model_1_backup"), file_list)] %>% 
      gsub(paste0(DIRECTORY_DATA_SD, "Models/model_1_backup"), "", .) %>% 
      gsub(".RData", "", .) %>% 
      unique %>%
      head(-1)
    
    files_to_remove <- dates_list %>% 
      U.sapply(function(d) file_list[grepl(d, file_list)]) %>%
      unique
    
    unlink(files_to_remove)
    
    file_list <- setdiff(file_list, file_list_old_backup)
    file_list_new_backup <- gsub(".RData", sprintf("_backup_%s.RData", YESTERDAY), file_list)
    file.copy(file_list, file_list_new_backup)
  }

}

####################################################################################################
### Script
####################################################################################################
Script <- function() {
  backupYesterdayModels(); I.completeStage(1)
 # D.waitTillPreviousJobHasFinished("Load_Tech", 3, 1, 60, 25); I.completeStage(2)
  V.prepareMorningModel(strats_list, use_weights); I.completeStage(3)
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()