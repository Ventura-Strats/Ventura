A.addETFToSystem <-
function (fx_pair, ticker, yahoo_ticker, ib_ticker, url_investing, tick_size, have_ib_histo,
          this_exchange, this_asset_class, asset_description, historics_done = FALSE) 
{
    ####################################################################################################
    ### Script description
    ####################################################################################################
    # 1 - Creates Asset
    # 2 - Add historics
    
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    asset_code <- U.left(fx_pair, 3)
    asset_ccy <- U.right(fx_pair, 3)
    ccy_id <- filter(CURRENCIES, ccy == asset_ccy)$ccy_id
    asset_id <- D.select("SELECT 1+MAX(asset_id) AS asset_id FROM static_asset")$asset_id
    instrument_id <- "SELECT 1+MAX(instrument_id) AS instrument_id FROM static_instrument" %>%
        D.select %>% 
        .$instrument_id
    asset_class_id <- D.loadTableLocal("asset_class") %>%
        filter(asset_class == this_asset_class) %>%
        .$asset_class_id
    market_id <- switch(
        asset_ccy,
        "USD" = 9,
        "EUR" = 27,
        "JPY" = 15,
        "GBP" = 37,
        "HKD" = 13,
        "AUD" = 19
    )
    
    ATTRIBUTE_TYPE <- D.loadTableLocal("instrument_attribute_type")
    
    EXCHANGES <- D.loadTableLocal("exchange")
    
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    createAsset <- function() {
        dat_asset <- data.frame(
            asset_id,
            code = asset_code, 
            asset = asset_description
        ) %>% 
            U.data2Tibble %>%
            D.insertDataIntoTable("static_asset", .)
    }

    createInstrument <- function() {
        data.frame(
            instrument_id,
            asset_id,
            ccy_id,
            ticker,
            asset_class_id,
            use_for_training = 1,
            use_for_trading = 1,
            use_for_trading_ib = 1,
            use_for_trading_gs = 1,
            tradable_long = 1,
            tradable_short = 1,
            market_id, 
            trade_instrument_type_id = 3
        ) %>% 
            U.data2Tibble %>%
            D.insertDataIntoTable("static_instrument", .)
    }
    
    addInstrumentAttributes <- function() {
        attribute_id <- filter(ATTRIBUTE_TYPE, attribute == "url_investing")$attribute_id
        data.frame(
            instrument_id,
            attribute_id,
            value = url_investing
        ) %>% 
            D.insertDataIntoTable("static_instrument_attribute_chr", .)
        
        attribute_id <- filter(ATTRIBUTE_TYPE, attribute == "yahoo_ticker")$attribute_id
        data.frame(
            instrument_id,
            attribute_id,
            value = yahoo_ticker
        ) %>% 
            D.insertDataIntoTable("static_instrument_attribute_chr", .)
        
        attribute_id <- filter(ATTRIBUTE_TYPE, attribute == "ib_symbol")$attribute_id
        data.frame(
            instrument_id,
            attribute_id,
            value = ib_ticker
        ) %>% 
            D.insertDataIntoTable("static_instrument_attribute_chr", .)
        
        attribute_id <- filter(ATTRIBUTE_TYPE, attribute == "have_ib_histo")$attribute_id
        data.frame(
            instrument_id,
            attribute_id,
            value = have_ib_histo
        ) %>% 
            D.insertDataIntoTable("static_instrument_attribute_int", .)
        
        attribute_id <- filter(ATTRIBUTE_TYPE, attribute == "exchange_id")$attribute_id
        exchange_id <- filter(EXCHANGES, ib_exchange == this_exchange)$exchange_id
        data.frame(
            instrument_id,
            attribute_id,
            value = exchange_id
        ) %>% 
            D.insertDataIntoTable("static_instrument_attribute_int", .)
        
        attribute_id <- filter(ATTRIBUTE_TYPE, attribute == "is_etf")$attribute_id
        data.frame(
            instrument_id,
            attribute_id,
            value = 1
        ) %>% 
            D.insertDataIntoTable("static_instrument_attribute_int", .)
        
        attribute_id <- filter(ATTRIBUTE_TYPE, attribute == "tick_size")$attribute_id
        data.frame(
            instrument_id,
            attribute_id,
            value = tick_size
        ) %>% 
            D.insertDataIntoTable("static_instrument_attribute_dbl", .)
        
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    createAsset() %>% U.debug(1) 
    createInstrument() %>% U.debug(2) 
    addInstrumentAttributes() %>% U.debug(3)
    this_instrument_id <- instrument_id
    T.importInvestingComHistoFile(fx_pair) %>% 
        filter(pair == fx_pair) %>% 
        mutate(instrument_id = this_instrument_id) %>%
        select(instrument_id, date, open, high, low, close) %>%
        D.insertDataIntoTable("histo_px_daily", .)
    # clean up historics then run Pareto then run full technicals
}
A.addIndexToSystem <-
function (fx_pair, ticker, yahoo_ticker, ib_ticker, url_investing, tick_size, have_ib_histo,
          this_exchange, this_asset_class, asset_description, historics_done = FALSE) 
{
    ####################################################################################################
    ### Script description
    ####################################################################################################
    # 1 - Creates Asset
    # 2 - Add historics
    
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    asset_code <- U.left(fx_pair, 3)
    asset_ccy <- U.right(fx_pair, 3)
    ccy_id <- filter(CURRENCIES, ccy == asset_ccy)$ccy_id
    asset_id <- D.select("SELECT 1+MAX(asset_id) AS asset_id FROM static_asset")$asset_id
    instrument_id <- "SELECT 1+MAX(instrument_id) AS instrument_id FROM static_instrument" %>%
        D.select %>% 
        .$instrument_id
    asset_class_id <- D.loadTableLocal("asset_class") %>%
        filter(asset_class == this_asset_class) %>%
        .$asset_class_id
    market_id <- switch(
        asset_ccy,
        "USD" = 9,
        "EUR" = 27,
        "JPY" = 15,
        "GBP" = 37,
        "HKD" = 13,
        "AUD" = 19
    )
    
    ATTRIBUTE_TYPE <- D.loadTableLocal("instrument_attribute_type")
    
    EXCHANGES <- D.loadTableLocal("exchange")
    
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    createAsset <- function() {
        dat_asset <- data.frame(
            asset_id,
            code = asset_code, 
            asset = asset_description
        ) %>% 
            U.data2Tibble %>%
            D.insertDataIntoTable("static_asset", .)
    }

    createInstrument <- function() {
        data.frame(
            instrument_id,
            asset_id,
            ccy_id,
            ticker,
            asset_class_id,
            use_for_training = 1,
            use_for_trading = 1,
            use_for_trading_ib = 1,
            use_for_trading_gs = 1,
            market_id, 
            trade_instrument_type_id = 3
        ) %>% 
            U.data2Tibble %>%
            D.insertDataIntoTable("static_instrument", .)
    }
    
    addInstrumentAttributes <- function() {
        attribute_id <- filter(ATTRIBUTE_TYPE, attribute == "url_investing")$attribute_id
        data.frame(
            instrument_id,
            attribute_id,
            value = url_investing
        ) %>% 
            D.insertDataIntoTable("static_instrument_attribute_chr", .)
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    createAsset() %>% U.debug(1) 
    createInstrument() %>% U.debug(2) 
    addInstrumentAttributes() %>% U.debug(3)
    this_instrument_id <- instrument_id
    T.importInvestingComHistoFile(fx_pair) %>% 
        filter(pair == fx_pair) %>% 
        mutate(instrument_id = this_instrument_id) %>%
        select(instrument_id, date, open, high, low, close) %>%
        D.insertDataIntoTable("histo_px_daily", .)
    # clean up historics then run Pareto then run full technicals
}
A.ccyVsccyId <-
function (ccy_input) 
{
    ####################################################################################################
    ### Sub Routines
    ####################################################################################################
    
    ccyTranslate <- function(this_ccy_input, input_type) {
        res <- switch(
            input_type,
            "ccy" = filter(CURRENCIES, ccy == this_ccy_input)$ccy_id,
            "ccy_id" = filter(CURRENCIES, ccy_id == this_ccy_input)$ccy
        )
        if (length(res) == 0) 
            res <- switch(input_type, "ccy" = INT_NA, "ccy_id" = CHAR_NA)
        res;
    }
    
    conversion_Try <- function(ccy_input) {
        ccy_input <- U.vectorize(ccy_input) %>% 
            U.noData2Null;
        
        convert_ccy <- NULL;
        
        if (!is.null(ccy_input)) {
            input_type <- switch(
                class(ccy_input[1]),
                "character" = "ccy",
                "integer" = "ccy_id",
                "numeric" = "ccy_id",
                NA
            )
            
            if (!is.na(input_type)) { 
                ccyTranslateLocal <- function(this_ccy_input) 
                    ccyTranslate(this_ccy_input, input_type)
                convert_ccy <- U.sapply(ccy_input, ccyTranslateLocal);
            }
        }
        convert_ccy;
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    U.try(conversion_Try)(ccy_input)
    
}
A.getExecutionTimeId <-
function (time_now = Sys.time()) 
{
    ####################################################################################################
    ### Script
    ####################################################################################################
    execution_time_id <- D.loadTableLocal("schedule_execution") %>%
        mutate(
            execution_time = as.POSIXct(paste0(TO_DAY, " ", execution_time), tz = TZ_LOCAL),
            diff_vs_now = as.numeric(difftime(time_now, execution_time, units = "mins")),
            diff_vs_now = case_when(
                (diff_vs_now < 0) ~ NUM_NA,
                (diff_vs_now > TIME_THRESHOLD_EXEC_ROUTINE_MINUTES) ~ NUM_NA,
                TRUE ~ diff_vs_now
            )
        ) %>%
        filter(!is.na(diff_vs_now)) %>%
        arrange(diff_vs_now) %>%
        .$execution_time_id %>%
        head(1)
    
    if (length(execution_time_id) == 0) {
        execution_time_id <- INT_NA
    }
    execution_time_id %>% U.debug("Execution time id:")
}
A.getInstrumentFutureConId <-
function (instrument_list) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    FUTURE <- D.loadTableLocal("future_contract")
    FUTURE_EXPIRY <- D.loadTableLocal("future_expiry") %>%
        mutate(expiry = as.Date(expiry))
    FUTURE_ACTIVE <- D.loadTableLocal("future_active")
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    getOneInstrument_Try <- function(instrument) {
        this_instrument_id <- A.getInstrumentId(instrument)
        this_pair <- filter(INSTRUMENTS, instrument_id == this_instrument_id)$pair
        this_future_id <- filter(FUTURE, instrument_id == this_instrument_id)$future_id
        this_conid <- filter(FUTURE_ACTIVE, future_id == this_future_id)$conid
        this_expiry <- filter(FUTURE_EXPIRY, conid == this_conid)$expiry
        
        data.frame(
            instrument_id = this_instrument_id,
            pair = this_pair,
            future_id = this_future_id,
            conid = this_conid,
            expiry = this_expiry
        ) %>% 
            U.data2Tibble
    }
    
    getOneInstrument <- function(instrument)
        U.try(getOneInstrument_Try, NULL)(instrument)

    ####################################################################################################
    ### Script
    ####################################################################################################
    instrument_list %>%
        lapply(getOneInstrument) %>%
        bind_rows
}
A.getInstrumentId <-
function (input_list) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    pair_list <- INSTRUMENTS$pair;
    ticker_list <- INSTRUMENTS$ticker;
    instrument_id_list <- INSTRUMENTS$instrument_id;
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    getOneInstrumentId_Try <- function(this_input) {
        instrument_id <- INT_NA;
        if (this_input %in% instrument_id_list) {
            instrument_id <- this_input;
        }
        else if (this_input %in% pair_list) {
            instrument_id <- filter(INSTRUMENTS, pair == this_input)$instrument_id[1];
        }
        else if (this_input %in% ticker_list) {
            instrument_id <- filter(INSTRUMENTS, ticker == this_input)$instrument_id[1];
        }
        else if (this_input == "train") {
            instrument_id <- "train"
        }
        instrument_id;
    }
    getOneInstrumentId <- function(this_input)
        U.try(getOneInstrumentId_Try, INT_NA)(this_input)
        
    ####################################################################################################
    ### Script
    ####################################################################################################
    U.sapply(input_list, getOneInstrumentId)
}
A.instrumentsWithBadData <-
function () 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    
    
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    instrumentsStatus <- function() {
        dat_status <- D.SQL("SELECT * FROM status_instrument")
        dat_tech <- "SELECT instrument_id, 
            MIN(date) AS min_date_technicals, MAX(date) AS max_date_technicals 
            FROM histo_technicals_dbl 
            GROUP BY instrument_id" %>%
            D.select
        dat_px <- "SELECT instrument_id, 
            MIN(date) AS min_date_price, MAX(date) AS max_date_price
            FROM histo_px_daily 
            GROUP BY instrument_id" %>%
            D.select
        INSTRUMENTS %>% 
            filter(use_for_trading + use_for_training >= 1) %>%
            select(instrument_id, pair, asset_class) %>% 
            left_join(dat_status, by="instrument_id") %>% 
            left_join(dat_px, by="instrument_id") %>% 
            left_join(dat_tech, by="instrument_id")
            
    }

    ####################################################################################################
    ### Script 
    ####################################################################################################
    instrumentsStatus()
}
A.save <-
function() {
    All_Objects <- ls(".GlobalEnv", pattern = "A.");
    All_Objects <- All_Objects[substr(All_Objects,1,2) == "A."];
    dump(All_Objects, paste0(DIRECTORY_CODE_HD, "/Code/Assets.R"));
}
