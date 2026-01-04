###########################################################################################################################
### Computes exit trades ledger and saves down to github
###########################################################################################################################
start_time <- Sys.time()
script_name <- "PnL_Github"
max_time_hours <- 0.25

###########################################################################################################################
### Sub routines
###########################################################################################################################

###########################################################################################################################
### Script
###########################################################################################################################
Script <- function() {
  PATH_FROM <- paste0(DIRECTORY_CODE_HD, "Code/")
  PATH_GIT <- paste0(DIRECTORY_DATA_HD, "Git/Ventura/docs/")
  PATH_SAVE <- paste0(DIRECTORY_DATA_HD, "Git_PnL/")
  
  V.buildTradesLifeCycle(); I.completeStage(1)
  pnl <- V.calcPnLVenturaLiveGithub(); I.completeStage(2)
  
  rmarkdown::render("Ventura_Report.Rmd"); I.completeStage(3)
  file_name <- "%s_Ventura_Report.html" %>% sprintf(YESTERDAY)
  file.copy(paste0(PATH_FROM, "Ventura_Report.html"), paste0(PATH_SAVE, file_name), overwrite=TRUE); I.completeStage(4)
  file.copy(paste0(PATH_FROM, "Ventura_Report.html"), paste0(PATH_GIT, file_name), overwrite=TRUE); I.completeStage(5)
  
  dat_reporting <- readLines("Ventura_Reporting.Rmd")
  pos_start <- which(dat_reporting == "## List of reports")
  pos_new <- pos_start+1
  is_already_there <- grepl(YESTERDAY, dat_reporting[pos_new])
  if (!is_already_there) {
    dat_reporting <- dat_reporting[c(1:pos_start, pos_new, pos_new:length(dat_reporting))]
    last_date <- substr(dat_reporting[pos_new],2,11)
    dat_reporting[pos_new] <- gsub(last_date, YESTERDAY, dat_reporting[pos_new])
    writeLines(dat_reporting, "Ventura_Reporting.Rmd")
  }
  I.completeStage(6)
  
  rmarkdown::render("Ventura_Reporting.Rmd"); I.completeStage(7)
  file.copy(paste0(PATH_FROM, "Ventura_Reporting.html"), paste0(PATH_GIT, "Reporting.html"), overwrite=TRUE); I.completeStage(8)
  
  rmarkdown::render("Ventura_HomePage.Rmd"); I.completeStage(9)
  file.copy(paste0(PATH_FROM, "Ventura_HomePage.html"), paste0(PATH_GIT, "index.html"), overwrite=TRUE); I.completeStage(10)
  
  
  setwd(gsub("docs/", "", PATH_GIT))
  system("GitPushVentura.sh docs index.html"); I.completeStage(11)
  system("GitPushVentura.sh docs Reporting.html"); I.completeStage(12)
  system(paste0("GitPushVentura.sh docs ", file_name)); I.completeStage(13)
  
}

###########################################################################################################################
### Execution
###########################################################################################################################
source("Init.R"); I.executeScript()