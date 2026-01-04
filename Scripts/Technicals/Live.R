####################################################################################################
### Calculates technicals for today on latest live prices
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Live"
max_time_hours <<- 6/60

nb_batches <- 20
pairs_to_do_index <<- as.integer(commandArgs(trailingOnly = TRUE)[2])

####################################################################################################
### Sub routines
####################################################################################################
whichPairsToDo <- function(exec_time_id) {
  dat_pair <- filter(INSTRUMENTS, use_for_trading == 1)
  if (!is.na(exec_time_id)) {
    dat_pair <- filter(dat_pair, execution_time_id == exec_time_id)
  }
  dat_pair$pair %>%
    U.splitInBatch(pairs_to_do_index, nb_batches) %>%
    U.debug("Starting technicals for these pairs:")
}

####################################################################################################
### Script
####################################################################################################
Script <- function() {
  exec_time_id <- A.getExecutionTimeId()
  pair_list <<- whichPairsToDo(exec_time_id); I.completeStage(1)
  D.waitTillPreviousJobHasFinished(
    previous_script_name = "Price_Live", 
    last_completed_stage = 6, 
    nb_jobs_total = 1, 
    max_lag_prices_minutes = 5, 
    max_wait_time_before_continuing_minutes = case_when(is.na(exec_time_id) ~ 6, TRUE ~ 2)
    ); I.completeStage(2)

  U.try(E.prepareTechnicalsLive)(pair_list, TRUE, NULL) %>% U.debug("Technicals - finished:"); I.completeStage(3)
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()