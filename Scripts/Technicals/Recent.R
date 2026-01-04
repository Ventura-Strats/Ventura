####################################################################################################
### Calculates Technicals only for the last few months or so
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Recent"
max_time_hours <<- 2

nb_batches <- 20
pairs_to_do_index <- as.integer(commandArgs(trailingOnly = TRUE)[2])

####################################################################################################
### Sub routines
####################################################################################################
whichPairsToDo <- function() {
  INSTRUMENTS %>%
    filter((use_for_training + use_for_trading >= 1)) %>%
    .$pair %>%
    U.splitInBatch(pairs_to_do_index, nb_batches) %>% 
    U.debug("Will do these pairs:")
}

####################################################################################################
### Script
####################################################################################################
Script <- function() {
  pair_list <<- whichPairsToDo(); I.completeStage(1)
  dat_tech <- T.calcTechnicalsRecentOnly(pair_list); I.completeStage(2)
  res <- pair_list %>% lapply(T.addTradingDataAllPossibilities); I.completeStage(3)
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()