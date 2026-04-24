####################################################################################################
### Dumps all technicals from DB so we can reload them faster when training all models
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Load_Tech"
max_time_hours <<- 1


####################################################################################################
### Sub-routines
####################################################################################################
removeOlderFiles <- function() {
  # Deletes files that aren't the last one for a given month
  
  c(
    paste0(DIRECTORY_DATA_HD, "Technicals_Backup/"),
    "/home/fls/Data/Backup/D4/Work_HD/Data/Ventura/Technicals_Backup/"
  ) %>% 
    lapply(
      function(file_path) {
        df <- file_path %>% 
          list.files(pattern = "*.RData") %>%
          data.frame(filename = .) %>%
          mutate(
            month_group = substr(filename, 12, 18) 
          ) %>%
          group_by(month_group) %>%
          arrange(filename) %>%
          filter(row_number() < n())
        
        # Delete the identified files
        unlink(paste0(file_path, df$filename))
      }
    )
}


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
  removeOlderFiles(); I.completeStage(4)
  
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()