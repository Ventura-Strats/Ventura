####################################################################################################
### Downloads price history on close, calculates latest technicals, 
### and also downloads latest intraday prices
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Price_Histo"
max_time_hours <<- 0.25

####################################################################################################
### Script
####################################################################################################
Script <- function() {
  system("PScriptVentura.sh Price_IB.py 1 & PScriptVentura.sh Price_IB.py 2"); I.completeStage(1)
  dat_histo_ohlc <- U.try(T.getHistoPrices)(); I.completeStage(2)
  dat_histo_fx_vs_usd <- U.try(T.histoPXvsUSD)(); I.completeStage(3)
  dat_intraday <- U.try(T.downloadIntradayFXSpot)(); I.completeStage(4)
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()