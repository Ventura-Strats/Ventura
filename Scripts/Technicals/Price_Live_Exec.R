####################################################################################################
### Calculates technicals for today on latest live prices
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Price_Live_Exec"
max_time_hours <<- 5/60

####################################################################################################
### Sub routines
####################################################################################################
getPairList <- function() {
  exec_time_id <- A.getExecutionTimeId(time_threshold = 15)
  pair_list <- INSTRUMENTS$pair
  if (!is.na(exec_time_id)) {
    pair_list <- INSTRUMENTS %>% 
      filter(
        use_for_trading == 1, 
        execution_time_id == A.getExecutionTimeId(time_threshold = 15)
      ) %>%
      .$pair
  }
  pair_list
}

####################################################################################################
### Script
####################################################################################################
Script <- function() {
  system("PScriptVentura.sh Price_IB.py"); I.completeStage(1)
  pair_list <- getPairList(); I.completeStage(2)
  dat_live <- U.try(T.getLivePrices)(pair_list); I.completeStage(3)
  U.try(T.getLivePriceFuture)(); I.completeStage(4)
  system("PScriptVentura.sh Price_IB_Future.py"); I.completeStage(5)
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()