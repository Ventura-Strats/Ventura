####################################################################################################
### Calculates technicals for today on latest live prices
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Price_Live"
max_time_hours <<- 5/60

####################################################################################################
### Sub routines
####################################################################################################
getPairList <- function() {
  exec_time_id <- A.getExecutionTimeId()
  pair_list <- filter(INSTRUMENTS, use_for_trading == 1)
  if (!is.na(exec_time_id)) {
    pair_list <- filter(pair_list, execution_time_id == A.getExecutionTimeId())
  }
  pair_list$pair
}

####################################################################################################
### Script
####################################################################################################
Script <- function() {
  system("PScriptVentura.sh Price_IB.py 1 & PScriptVentura.sh Price_IB.py 2"); I.completeStage(1)
  pair_list <- getPairList(); I.completeStage(2)
  dat_live <- U.try(T.getLivePrices)(pair_list); I.completeStage(3)
  U.try(T.getLivePriceFuture)(); I.completeStage(4)
  system("PScriptVentura.sh Price_IB_Future.py"); I.completeStage(5)
  U.try(T.estimateTodayOHLCIndexPriceWhenNotYetOpenFromLiveFuture)(TRUE); I.completeStage(6)
  dat_histo_fx_vs_usd <- U.try(T.histoPXvsUSD)(); I.completeStage(7)
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()