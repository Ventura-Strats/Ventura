####################################################################################################
### Dumps all technicals from DB so we can reload them faster when training all models
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Load_Tech"
max_time_hours <<- 1

####################################################################################################
### Script
####################################################################################################
Script <- function() {
  file_name <- paste0(DIRECTORY_DATA_SD, "Technicals/Technicals.RData")
  backup_file_name <- file_name %>% 
    gsub(".RData", paste0("_", YESTERDAY, ".RData"), .) %>%
    gsub("SD/Technicals/", "HD/Technicals_Backup/", .)
  file.copy(file_name, backup_file_name); I.completeStage(1)
  dat <- T.getTechnicals(NULL, NULL, FALSE); I.completeStage(2)
#  file_name <- paste0(DIRECTORY_DATA, "Technicals/Technicals.RData")
  save(dat, file = file_name); I.completeStage(3)
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()