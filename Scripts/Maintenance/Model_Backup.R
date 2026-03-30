####################################################################################################
### Takes all saved models and makes a backup copy
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Model_Backup"
max_time_hours <<- 2

####################################################################################################
### Sub routines
####################################################################################################
backupModels <- function() {
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


####################################################################################################
### Script
####################################################################################################
Script <- function() {
  backupModels(); I.completeStage(1)
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()