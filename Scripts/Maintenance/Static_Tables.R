####################################################################################################
### Downloads main static tables and prepares frequently used tables 
### to reduce DB load
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Static_Tables"
max_time_hours <<- 0.25

####################################################################################################
### Sub routines
####################################################################################################
listAllTables <- function() {
  "SELECT TABLE_NAME 
    FROM information_schema.tables 
    WHERE table_schema = 'Ventura'" %>%
    D.select %>%
    filter(U.left(TABLE_NAME, 1) != "z") %>%
    U.dfNoRows2Null %>% 
    U.vectorize
}

downloadTable <- function(tbl_name) D.SQL(paste0("SELECT * FROM ", tbl_name))

saveTable <- function(dat, tbl_name) {
  "%sTables_Local/%s.csv" %>%
    sprintf(DIRECTORY_DATA_HD, tbl_name) %>%
    U.write.csv(dat, .)
}

downloadThenSaveTable <- function(tbl_name) {
  U.printTickerProgressVerbose(tbl_name, db_tbl_list)
  tbl_name %>% 
    downloadTable %>% 
    saveTable(tbl_name)
}

getScheduleExecutionTimes <- function() {
  D.loadTableLocal("schedule_execution") %>%
    mutate(
      execution_time = as.POSIXct(paste0(TO_DAY, " ", execution_time), tz = TZ_LOCAL)
    )
}

getMarketExecutionTimes <- function() {
  dat <- "SELECT DISTINCT M.market_id, M.market, M.execution_time_tz_close, T.time_zone 
   FROM static_instrument I
   LEFT JOIN static_market M ON M.market_id = I.market_id
   LEFT JOIN static_time_zone T ON T.time_zone_id = M.tz_close_id
   WHERE I.use_for_trading_ib = 1
   ORDER BY M.market_id" %>%
    D.SQL %>%
    mutate(
      execution_time = Sys.time()
    )
  for (i in 1:nrow(dat)) {
    dat$execution_time[i] <-  dat$execution_time_tz_close[i] %>%
      paste0(TO_DAY, " ", .) %>%
      as.POSIXct(tz = dat$time_zone[i]) %>%
      format(tz = TZ_LOCAL) %>%
      as.POSIXct
  }
  dat
}

prepareSpecificTables <- function() {
  dat_schedule <- getScheduleExecutionTimes()
  dat_markets <- getMarketExecutionTimes()
  dat_exec <- dat_markets %>% 
    left_join(dat_schedule, by = "execution_time") %>%
    select(market, execution_time, execution_time_id)
  
  dat_conidfx <- "SELECT instrument_id, value AS conid_spot
    FROM static_instrument_attribute_int A
    LEFT JOIN static_instrument_attribute_type T ON A.attribute_id = T.attribute_id
    WHERE T.attribute = 'conid_spot'" %>% 
    D.select
  
  dat_isetf <- "SELECT instrument_id, value AS is_etf
    FROM static_instrument_attribute_int A
    LEFT JOIN static_instrument_attribute_type T ON A.attribute_id = T.attribute_id
    WHERE T.attribute = 'is_etf'" %>% 
    D.select
  
  INSTRUMENTS <- 
    "SELECT I.*, C.ccy, A.code, A.asset, T.asset_class, M.market, 
      Y.trade_instrument_type AS trade_instrument_type
      FROM static_instrument I
      LEFT JOIN static_currency C ON C.ccy_id = I.ccy_id
      LEFT JOIN static_asset A ON A.asset_id = I.asset_id
      LEFT JOIN static_asset_class T ON T.asset_class_id = I.asset_class_id
      LEFT JOIN static_market M ON M.market_id = I.market_id
      LEFT JOIN static_trade_instrument_type Y ON I.trade_instrument_type_id = Y.trade_instrument_type_id " %>%
    D.select %>%
    mutate(
      pair = paste0(code, ccy),
      asset_class = factor(
        asset_class,
        levels = c("fx_dm", "fx_em", "index", "metal", "bond")
      )
    ) %>%
    left_join(dat_exec, by = "market") %>%
    left_join(dat_conidfx, by = "instrument_id") %>%
    left_join(dat_isetf, by = "instrument_id") %>%
    U.dfReplaceNAColumnsWithZero("is_etf") %>%
    select(
      instrument_id, pair, ticker, ccy, asset, asset_class, 
      use_for_training, use_for_trading, use_for_trading_ib,
      market, market_id, execution_time_id, execution_time,
      trade_instrument_type,
      conid_spot, is_etf
    ) %>%
    arrange(market, pair) 
  
    "%sTables_Local/%s.csv" %>%
    sprintf(DIRECTORY_DATA_HD, "INSTRUMENTS") %>%
    U.write.csv(INSTRUMENTS, .)
  
  URL_INVESTING <- "SELECT A.instrument_id, A.value FROM 
      static_instrument_attribute_chr A
      LEFT JOIN static_instrument_attribute_type T ON T.attribute_id = A.attribute_id
      WHERE T.attribute = 'url_investing'" %>%
    D.select %>%
    left_join(INSTRUMENTS, by = "instrument_id") %>%
    select(pair, value)
  
  "%sTables_Local/%s.csv" %>%
    sprintf(DIRECTORY_DATA_HD, "URL_INVESTING") %>%
    U.write.csv(URL_INVESTING, .)
  
  etf_proxy <- D.loadTableLocal("etf_proxy") %>% select(instrument_id, conid)
  etf_direct <- D.select("SELECT instrument_id, value AS conid 
                       FROM static_instrument_attribute_int 
                       WHERE attribute_id = 7")
  rbind(etf_proxy, etf_direct) %>%
    U.write.csv(paste0(DIRECTORY_DATA_HD, "Tables_Local/ETF.csv"))
  
  
  
}

####################################################################################################
### Script
####################################################################################################
Script <- function() {
  db_tbl_list <<- listAllTables()
  db_tbl_list <<- db_tbl_list[grepl("static_", db_tbl_list)]
  db_tbl_list %>%
    lapply(downloadThenSaveTable)
  I.completeStage(1)
  
  prepareSpecificTables(); I.completeStage(2)
  
  dbSendQuery(dbCon, "SET GLOBAL local_infile = true;"); I.completeStage(3)
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()