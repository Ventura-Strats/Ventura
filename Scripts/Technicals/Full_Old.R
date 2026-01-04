####################################################################################################
### Recomputes all technicals history (very slow)
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Full"
max_time_hours <<- 5

nb_batches <- 60
pairs_to_do_index <<- as.integer(commandArgs(trailingOnly = TRUE)[2])

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

calcTech_Try <- function(this_pair) {
  U.printTickerProgressVerbose(this_pair, pair_list)
  T.calcTechnicalsFull(this_pair)
}

calcTech <- function(this_pair) 
  U.try(calcTech_Try, NULL)(this_pair) 

####################################################################################################
### Script
####################################################################################################
Script <- function() {
  pair_list <<- whichPairsToDo(); I.completeStage(1)
  dat_tech <- lapply(pair_list, calcTech); I.completeStage(2)
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()