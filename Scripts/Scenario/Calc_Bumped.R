####################################################################################################
### Calculates bumped technicals for today on latest live prices by moving the prices across a scale
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Calc_Bumped"
max_time_hours <<- 0.75

nb_batches <- 25
bumps_to_do_index <<- as.integer(commandArgs(trailingOnly = TRUE)[2])

####################################################################################################
### Sub routines
####################################################################################################
whichBumpsToDo <- function() {
  D.loadTableLocal("scenario_bump")$bump %>%
    U.splitInBatch(bumps_to_do_index, nb_batches) %>% 
    U.debug("Starting technicals for these bumps:")
}
####################################################################################################
### Script
####################################################################################################
Script <- function() {
  pair_list <- filter(INSTRUMENTS, use_for_trading == 1)$pair
  bumps_to_do <- whichBumpsToDo()
  D.waitTillPreviousJobHasFinished("Live", 3, 17, 5)
  for (bump in bumps_to_do) {
    U.printTickerProgressVerbose(bump, bumps_to_do)
    E.prepareTechnicalsLive(pair_list, TRUE, bump)
    I.completeStage(which(bumps_to_do == bump))
  }
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()