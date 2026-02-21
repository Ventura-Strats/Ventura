B.calcSharpe <-
function (dat_nav) {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    calcDailyNAV <- function() {
        dat_nav %>% 
            rename(timestamp=date) %>% 
            mutate(date = as.Date(timestamp)) %>%
            arrange(timestamp) %>%
            group_by(date) %>% 
            summarize(nav = last(value)) %>%
            ungroup 
    }

    ####################################################################################################
    ### Script
    ####################################################################################################
    dat <- calcDailyNAV()
    dat <- data.frame(date = seq(min(dat$date), max(dat$date), 1)) %>% 
        U.data2Tibble %>% 
        left_join(dat, by="date") %>% 
        na.locf(na.rm=FALSE) %>% 
        mutate(rtn = log(nav/lag(nav, 1))) %>% 
        na.omit
    
    dat$vol_1y <- T.rollApply(dat$rtn, 365, sd)*sqrt(365)
    dat$vol_6m <- T.rollApply(dat$rtn, 183, sd)*sqrt(365)
    dat %>% 
        mutate(
            rtn_1y = nav / lag(nav, 365) - 1,
            sharpe = rtn_1y / vol_1y
        )    
}
B.closeTradeFromLegs <-
function (trade_id, exit_type, exit_date, leg_id_list) {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    exit_type <- tolower(exit_type)
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    determineTradeOutcome <- function() {
        buy_sell <- "SELECT buy_sell FROM book_trade_leg WHERE leg_id = %s" %>%
            sprintf(leg_id_list[1]) %>%
            D.SQL %>%
            U.vectorize
        
        if (buy_sell == 1) {
            switch(exit_type, "target" = "down", "stop" = "up", "exit_maturity" = "flat", "flat")
        } 
        else if (buy_sell == -1) {
            switch(exit_type, "target" = "up", "stop" = "down", "exit_maturity" = "flat", "flat")
        }
        else "unknown"
    }
    
    determineTradeOutcomeId <- function(trade_outcome) {
        trade_outcome <- determineTradeOutcome()
        filter(TRADE_OUTCOMES, outcome == trade_outcome)$outcome_id
    }
    
    closeTradeId <- function() {
        "UPDATE book_trade
        SET trade_outcome_id = %s, date_exit = '%s'
        WHERE trade_id = %s" %>%
            sprintf(determineTradeOutcomeId(), exit_date, trade_id) %>%
            D.SQL
    }
    
    addExitLegsMap <- function() {
        data.frame(
            trade_id = trade_id,
            leg_id = leg_id_list,
            trade_category_id = 2,
            size = NUM_NA
        ) %>%
            U.data2Tibble %>%
            D.insertDataIntoTable("book_trade_map", .)
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    closeTradeId()
    addExitLegsMap()
}
B.createNewTradeIDFromLegs <-
function (trade_date, strategy_id, tp_pct, leg_id_list) {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    getNewTradeId <- function() 1 + U.vectorize(D.SQL("SELECT MAX(trade_id) FROM book_trade"))
    
    createNewTradeIdEntry <- function(trade_id) {
        data.frame(
            trade_id,
            strategy_id,
            trade_outcome_id = 0,
            date_entry = trade_date,
            date_exit = DATE_NA,
            target_pct = 100 * tp_pct
        ) %>%
            U.data2Tibble %>% 
            D.insertDataIntoTable("book_trade", .)
        trade_id
    }
    
    addEntryLegsMap <- function(trade_id) {
        data.frame(
            trade_id,
            leg_id = leg_id_list,
            trade_category_id = 1,
            size = NUM_NA
        ) %>%
            U.data2Tibble %>%
            D.insertDataIntoTable("book_trade_map", .)
        trade_id
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    getNewTradeId() %>%
        createNewTradeIdEntry %>%
        addEntryLegsMap
}
B.listAllTrades <-
function () {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    dat_trd <- D.loadTable("book_trade")
    dat_map <-D.loadTable("book_trade_map")
    dat_fx <- D.SQL("SELECT * FROM book_trade_fx")
    dat_fut <- D.SQL("SELECT * FROM book_trade_future")
    
    FUTURES <- D.loadTableLocal("future_contract")
    FUTURES_EXPIRY <- D.loadTableLocal("future_expiry")
    
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################

    
    ####################################################################################################
    ### Script
    ####################################################################################################
    

}
B.liveTrades <-
function (dat_trd) {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################

    ####################################################################################################
    ### Sub routines
    ####################################################################################################

    ####################################################################################################
    ### Script
    ####################################################################################################
    
    
    
    buildTradesList("entry") %>%
        left_join(
            buildTradesList("exit"), 
            by = c("trade_id", "account_id", "identifier", "date_entry", "date_exit")
        ) %>%
        filter(
            buy_sell_entry == -buy_sell_exit,
            size_entry == size_exit
        ) %>%
        rename(
            buy_sell = buy_sell_entry,
            size = size_entry
        ) %>%
        select(-size_exit, -buy_sell_exit) %>% 
        addTradeTypeAndIdentifiers %>%
        calculateTargetStop
    
}
B.matchNewTradeLegs <-
function () {
    ####################################################################################################
    ### Script variables
    ####################################################################################################

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    prepareNewLegs <- function() {
        dat_new_leg <- B.recentLegsNotMatched() %>%
            as_tibble %>%
            mutate(timestamp = as.POSIXct(timestamp)) %>%
            filter(timestamp >= Sys.time() %m+% days(-1))
        print(minute(dat_new_leg$timestamp))
        minute(dat_new_leg$timestamp) <- U.mround(minute(dat_new_leg$timestamp), 15)
        second(dat_new_leg$timestamp) <- 0
        dat_new_leg
    }
    
    prepareNewTrades <- function(dat_new_leg) {
        dat_new_leg %>% 
            mutate(px_size = size * price) %>%
            group_by(account_id, identifier, buy_sell, timestamp) %>%
            summarize(
                size = sum(size),
                price = sum(px_size) / size
            ) %>%
            ungroup
    }
    
    prepareBookLiveTrades <- function() {
        B.readTradesFromDB()$trd_live %>%
            mutate(
                identifier = case_when(is.na(conid) ~ instrument_id, TRUE ~ conid),
                acceptable_trading_width = 0.01 * price_entry * target_pct,
                target_trd_dn = target - acceptable_trading_width,
                target_trd_up = target + acceptable_trading_width,
                stop_trd_dn = stop - acceptable_trading_width,
                stop_trd_up = stop + acceptable_trading_width
            )
    }
    
    identifyTargetAndStops <- function(target_or_stop, dat_new_trd, dat_book, dat_new_leg) {
        dat_match <- dat_new_trd %>% 
            mutate(buy_sell = -buy_sell) %>%
            semi_join(dat_book, by = c("account_id", "identifier", "size", "buy_sell")) %>%
            left_join(dat_book, by = c("account_id", "identifier", "size", "buy_sell"))
        dat_match <- switch(
            target_or_stop,
            "target" = filter(dat_match, price >= target_trd_dn, price <= target_trd_up),
            "stop" = filter(dat_match, price >= stop_trd_dn, price <= stop_trd_up)
        ) %>%
            mutate(buy_sell = -buy_sell) %>%
            select(trade_id, account_id, identifier, timestamp, buy_sell) %>%
            left_join(a.1[[1]], by=c("account_id", "identifier", "timestamp", "buy_sell")) %>%
            mutate(
                exit_date = case_when(
                    hour(timestamp) <= 10 ~ U.calcPreviousDay(as.Date(timestamp)),
                    TRUE ~ as.Date(timestamp)
                )
            ) %>%
            select(trade_id, exit_date, leg_id) %>% 
            group_by(trade_id, exit_date, ) %>%
            summarize(leg_id_list = paste0("c(", paste(leg_id, collapse=","), ")")) %>%
            ungroup %>% U.debug("AAA1") %>%
            mutate(
                instruction = "B.closeTradeFromLegs(%s, '%s', '%s', %s)" %>%
                    sprintf(trade_id, target_or_stop, exit_date, leg_id_list)
            )
    }
    identifyStops <- function(dat_new_trd, dat_book, dat_new_leg) {
        identifyTargetAndStops("stop", dat_new_trd, dat_book, dat_new_leg)
    }
    identifyTargets <- function(dat_new_trd, dat_book, dat_new_leg) {
        identifyTargetAndStops("target", dat_new_trd, dat_book, dat_new_leg)
    }

    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_new_leg <- prepareNewLegs() %>% U.debug(1)
    dat_new_trd <- prepareNewTrades(dat_new_leg) %>% U.debug(2)
    dat_book <- prepareBookLiveTrades() %>% U.debug(3)
    dat_stop <- identifyStops(dat_new_trd, dat_book, dat_new_leg) %>% U.debug(4)
    dat_book <- dat_book %>%
        anti_join(dat_stop, by = "trade_id") %>% U.debug(5)
    
    used_leg_id <- dat_stop %>% 
        mutate(leg_id_list = substr(leg_id_list, 3, nchar(leg_id_list) - 1)) %>% 
        .$leg_id_list %>%
        paste(collapse=",") %>%
        c("c(", ., ")") %>%
        paste(collapse="") %>%
        parse(text=.) %>%
        eval %>% U.debug(6)
    print(used_leg_id)
    if (is.null(used_leg_id)) {
        used_leg_id <- 0
    }

    dat_new_leg <- dat_new_leg %>%
        filter(!(leg_id %in% used_leg_id)) %>% U.debug(8)
    
    dat_new_trd <- prepareNewTrades(dat_new_leg) %>% U.debug(9)
    
    dat_target <- identifyTargets(dat_new_trd, dat_book, dat_new_leg) %>% U.debug(10)
    dat_book <- dat_book %>%
        anti_join(dat_target, by = "trade_id") %>% U.debug(11)
    list(dat_new_leg, dat_new_trd, dat_book, dat_stop, dat_target)
}
B.matchTradeLegs <-
function (dat_trd) {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################

    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    dat_entry <- dat_trd %>% 
        filter(trade_category == "entry") %>%
        rename(
            price_entry = price,
            size_entry = size,
            buy_sell_entry = buy_sell
        ) %>%
        select(-trade_category)
    
    dat_exit <- dat_trd %>%
        filter(trade_category == "exit") %>%
        rename(
            price_exit = price,
            size_exit = size,
            buy_sell_exit = buy_sell
        ) %>%
        select(
            -trade_category, -target, -stop, -future_id, -expiry, -conid, 
            -instrument_type, -pair, -target_pct
        )
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_entry %>%
        left_join(
            dat_exit, 
            by = c(
                "trade_id", "strategy_id", "account_id", "instrument_id", 
                "trade_status", 
                "date_entry", "date_exit"
            )
        ) %>%
        filter(
            buy_sell_entry == -buy_sell_exit,
            size_entry == size_exit
        ) %>%
        rename(
            buy_sell = buy_sell_entry,
            size = size_entry
        ) %>%
        select(
            trade_id, strategy_id, account_id, 
            instrument_type, pair, instrument_id, future_id, expiry, conid,
            trade_status, date_entry, date_exit, buy_sell,   
            size, price_entry, price_exit, target_pct, target, stop
        )
}
B.processTradesFromIB <-
function (just_today = FALSE, save_to_db = FALSE) {
    ####################################################################################################
    ### Script Description
    ####################################################################################################
    ### Load IB Trades - only the ones that haven't been archived yet
    ####################################################################################################
    
    ####################################################################################################
    ### Script variables and parameters
    ####################################################################################################
    nb_accounts <- 2
    dat_leg <- D.SQL("SELECT ib_trade_id FROM book_trade_leg")
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    readTradesPast <- function() {
        dat <- NULL
        if (!just_today) {
            dat <- 1:nb_accounts %>%
                lapply(function(i) B.readTradesFromIB(i, "Trades")
                ) %>% 
                bind_rows %>%
                U.noData2Null
        }   
        dat
    }
    
    readTradesNew <- function(trd_past) {
        dat <- 1:nb_accounts %>%
            lapply(function(i) B.readTradesFromIB(i, "Trades_Today")
            ) %>% 
            bind_rows %>% 
            U.noData2Null
        if ((!just_today) * !is.null(dat)) 
            dat <- dat %>% 
                anti_join(trd_past, by = "TradeID")
        dat
    }
    
    prepareIBTradesList <- function() {
        U.debug("1 - Reading IB trades list")
        trd_past <- readTradesPast()
        trd_new <- readTradesNew(trd_past)
        trd <- rbind(trd_past, trd_new)
        if (!is.null(trd)) {
            for (j in 1:ncol(trd)) trd[,j] <- U.vectorize(trd[,j])
        }
        trd <- trd %>%
            U.dfReplaceNAColumnsWithZero(
                c(
                    "Commission", "BrokerExecutionCommission",
                    "BrokerClearingCommission", "ThirdPartyExecutionCommission",
                    "ThirdPartyClearingCommission", "ThirdPartyRegulatoryCommission",
                    "OtherCommission", "Tax"
                )
            ) %>%
            U.debug("1 - Reading IB trades list - done")
        print(data.frame(trd))
        trd
    }
    
    updateDBTradeInformationForMatchedTrades_Try <- function(dat) {
        for (i in 1:nrow(dat)) {
            "UPDATE book_trade_leg
            SET ib_trade_id = %s, size = %s, price = %s, fees = %s, fees_ccy_id = %s, tax = %s
            WHERE ib_exec_id = '%s'" %>%
                sprintf(
                    dat$ib_trade_id[i],
                    dat$size[i],
                    dat$price[i],
                    dat$fees[i],
                    dat$fees_ccy_id[i],
                    dat$tax[i],
                    dat$ib_exec_id[i]
                ) %>%
            #    U.debug(paste0("SQL ", dat$identifier[i])) %>%
                D.SQL
        }
        dat
    }
    updateDBTradeInformationForMatchedTrades <- function(dat)
        U.try(updateDBTradeInformationForMatchedTrades_Try, dat)(dat)
    
    matchWithLegsAlreadySavedViaAPI <- function(dat) {
        dat_db <- "SELECT DISTINCT * 
        FROM book_trade_leg 
        WHERE timestamp >= '%s'" %>%
            sprintf(YESTERDAY) %>%
            D.SQL
        
        max_leg_id <- "SELECT MAX(leg_id) AS leg_id FROM book_trade_leg" %>%
            D.select %>% 
            U.vectorize
        
        dat_matched <- dat %>%
            semi_join(dat_db, by = "ib_exec_id") %>% 
            U.noData2Null %>% U.debug("Trades Matched, will update DB:") %>%
            updateDBTradeInformationForMatchedTrades
        
        dat_unmatched <- dat %>%
            anti_join(dat_db, by = "ib_exec_id")  %>%
            U.noData2Null %>%
            U.debug("Trades Unmatched, adding:")
        if (U.dfContainsData(dat_unmatched)) {
            D.insertDataIntoTable("book_trade_leg", dat_unmatched, FALSE)
        }

        U.printBanner("Legs matched: ")
        print(dat_matched)
        U.printBanner("Legs unmatched: ")
        print(dat_unmatched)
        
        rbind(dat_matched, dat_unmatched) %>% 
            arrange(leg_id)
    }
    
    saveToDB <- function(dat) {
        if (save_to_db) {
            dat <- dat %>%
                U.printMilestone("Trades processing done, will save in DB") %>%
                matchWithLegsAlreadySavedViaAPI %>%
                U.printMilestone("Trades now in DB")
        }
        else {
            U.printBanner("Not saving into DB", FALSE)
        }
        dat
    }
    
    processTrades_Try <- function(trd) {
        U.debug("2 - Processing new trades and saving to DB")
        trd <- trd %>%
            rename(
                asset_class = AssetClass,
                ib_trade_id = TradeID,
                ib_exec_id = ExecID,
                conid = Conid,
                price = Price
            ) %>%
            mutate(
                timestamp = as.POSIXct(Date.Time, "%Y%m%d;%H%M%S", tz = TZ_AMERICA),
                timestamp = format(timestamp, tz = TZ_LOCAL)
            ) %>% 
            filter(
                asset_class %in% c("FUT", "CASH"),
                timestamp >= as.POSIXct("2020-11-01 00:00:01")
            ) %>% 
            anti_join(dat_leg, by = "ib_trade_id") %>%
            mutate(
                fees = abs(Commission) + abs(BrokerExecutionCommission) + 
                    abs(BrokerClearingCommission) + abs(ThirdPartyExecutionCommission) + 
                    abs(ThirdPartyClearingCommission) + abs(ThirdPartyRegulatoryCommission) + 
                    abs(OtherCommission),
                tax = abs(Tax),
                buy_sell = (Buy.Sell == "BUY") - (Buy.Sell == "SELL"),
                size = abs(Quantity),
                size_price = size * price,
                pair = gsub(".", "", Symbol, fixed = TRUE)
            ) %>% 
            left_join(
                CURRENCIES %>%
                    select(ccy_id, ccy) %>%
                    rename(fees_ccy_id = ccy_id, CommissionCurrency = ccy),
                by = "CommissionCurrency") %>%
            left_join(select(INSTRUMENTS, instrument_id, pair), by = "pair") %>%
            mutate(
                identifier = case_when(
                    asset_class == "FUT" ~ conid,
                    asset_class == "CASH" ~ instrument_id
                )
            ) %>%
            filter(!is.na(identifier)) %>%
            select(
                ib_trade_id, ib_exec_id, account_id, identifier, timestamp, 
                buy_sell, size, price, fees, fees_ccy_id, tax
            ) %>% 
            saveToDB
        
        print(data.frame(trd))
        U.debug("2 - Processing new trades and saving to DB - Done")
        trd
        
    }
    processTrades <- function(trd)
        U.try(processTrades_Try, trd)(trd)
    
    
    ####################################################################################################
    ### Main Script
    ####################################################################################################
    prepareIBTradesList() %>%
        processTrades
}
B.readTradesFromDB <-
function () {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    dat_trd <- D.loadTable("book_trade")
    dat_map <- D.loadTable("book_trade_map")
    dat_leg <- D.SQL("SELECT * FROM book_trade_leg")
    
    FUTURE <- D.loadTableLocal("future_contract")
    FUTURE_EXPIRY <- D.loadTableLocal("future_expiry")
    TRADE_CATEGORY <- D.loadTableLocal("trade_category")
    IDENTIFIER <- D.loadTableLocal("trade_instrument_identifier")
    TRADE_OUTCOME <- D.loadTableLocal("trade_outcome")
    
    FUTURE_CONID_MAP <- FUTURE_EXPIRY %>%
        left_join(FUTURE, by = "future_id") %>%
        select(conid, future_id, instrument_id, expiry)
    
    ETF <- D.loadTableLocal("ETF")
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    addTradeTypeAndIdentifiers <- function(dat) {
        dat %>% 
            left_join(rename(FUTURE_CONID_MAP, identifier = conid), by = "identifier") %>%
            left_join(
                rename(ETF, instrument_id_etf = instrument_id, identifier = conid), 
                by = "identifier"
                ) %>%
            mutate(
                identifier = as.integer(identifier),
                instrument_type = case_when(
                    !is.na(future_id) ~ "Future",
                    !is.na(instrument_id_etf) ~ "ETF",
                    TRUE ~ "FX"
                ),
                instrument_id = case_when(
                    (instrument_type == "FX") ~ identifier,
                    (instrument_type == "ETF") ~ instrument_id_etf,
                    (instrument_type == "Future") ~ instrument_id
                )
            ) %>%
            left_join(select(INSTRUMENTS, instrument_id, pair), by = "instrument_id") %>%
            mutate(
                conid = case_when(
                    (instrument_type %in% c("Future", "ETF")) ~ identifier,
                    TRUE ~ INT_NA
                ),
                expiry = format(as.Date(expiry), "%b%y")
            ) %>%
            select(-identifier) 
    }
    
    buildTradesList <- function() {
        dat_map %>%
            rename(size_map = size) %>%
            left_join(dat_trd, by = "trade_id") %>%
            left_join(dat_leg, by = "leg_id") %>%
            left_join(TRADE_CATEGORY, by = "trade_category_id") %>%
            left_join(
                rename(TRADE_OUTCOME, trade_outcome_id = outcome_id), 
                by = "trade_outcome_id"
            ) %>% 
            filter(trade_category %in% c("entry", "exit")) %>%
            mutate(
                size = as.numeric(size),
                size_map = as.numeric(size_map),
                size = case_when(
                    !is.na(size_map) ~ size_map,
                    TRUE ~ size
                ),
                buy_sell = buy_sell * sign(size),
                size = abs(size),
                size_price = price * size,
                trade_outcome = case_when(
                    outcome == "up" ~ 1,
                    outcome == "down" ~ -1,
                    outcome %in% c("live", "flat") ~ 0,
                    TRUE ~ NUM_NA
                ),
                category_int = (trade_category == "entry") - (trade_category == "exit"),
                trade_status = case_when(
                    is.na(category_int * trade_outcome) ~ CHAR_NA,
                    outcome == "live" ~ "live",
                    category_int * buy_sell * trade_outcome == 1 ~ "target",
                    category_int * buy_sell * trade_outcome == -1 ~ "stop",
                    category_int * buy_sell * trade_outcome == 0 ~ "flat",
                    TRUE ~ CHAR_NA
                ),
                target_pct = target_pct / 100
            ) %>% 
            group_by(
                trade_id, strategy_id, account_id, identifier, 
                trade_category, trade_status, 
                date_entry, date_exit, 
                buy_sell, target_pct
            ) %>%
            summarize(
                size = sum(size),
                price = sum(size_price) / size
            ) %>% 
            ungroup
    }
    
    calculateTargetStopTrades <- function(dat_trd) {
        dat_tp <- dat_trd %>% 
            filter(trade_category == "entry") %>%
            mutate(size_price = size * price) %>%
            group_by(trade_id) %>% 
            summarize(
                buy_sell = first(buy_sell),
                target_pct = mean(target_pct),
                size = sum(size),
                price = sum(size_price) / size
            ) %>% 
            ungroup %>%
            mutate(
                target = price * (1 + buy_sell * target_pct),
                stop = 2 * price - target
            ) %>% 
            select(trade_id, target, stop)
        
        dat_trd %>%
            left_join(dat_tp, by = "trade_id")
    }
    
    readTrades <- function() {
        buildTradesList() %>%
            addTradeTypeAndIdentifiers %>% 
            calculateTargetStopTrades %>%
            select(
                trade_id, strategy_id, account_id, instrument_type,
                pair, instrument_id, future_id, expiry, conid,
                trade_category, trade_status, date_entry,  date_exit,
                buy_sell, size, price, target_pct, target, stop
            )
    }
    
    groupTradesAcrossAccounts <- function(dat_trd) {
        dat_trd %>%
            mutate(size_price = size * price) %>%
            group_by(
                trade_id, strategy_id, instrument_type,
                pair, instrument_id, future_id, expiry, conid,
                trade_category, trade_status, 
                date_entry, date_exit, 
                buy_sell
            ) %>%
            summarize(
                size = sum(size),
                price = sum(size_price) / size,
                target = first(target),
                stop = first(stop),
                target_pct = first(target_pct)
            ) %>% 
            ungroup 
    }
    
    
    matchTrades <- function(dat_trd) {
        dat_entry <- dat_trd %>% 
            filter(trade_category == "entry") %>%
            rename(
                price_entry = price,
                size_entry = size,
                buy_sell_entry = buy_sell
            ) %>%
            select(-trade_category)
        
        dat_exit <- dat_trd %>%
            filter(trade_category == "exit") %>%
            rename(
                price_exit = price,
                size_exit = size,
                buy_sell_exit = buy_sell
            ) %>%
            select(
                -trade_category, -target, -stop, -future_id, -expiry, -conid, 
                -instrument_type, -pair, -target_pct
            )
        
        col_join <- c(
            "trade_id", "strategy_id", "account_id", "instrument_id", 
            "trade_status", 
            "date_entry", "date_exit"
        ) %>% intersect(colnames(dat_entry))
        
        col_select <- c(
            "trade_id", "strategy_id", "account_id", 
            "instrument_type", "pair", "instrument_id", "future_id", "expiry", "conid",
            "trade_status", "date_entry", "date_exit", "buy_sell",   
            "size", "price_entry", "price_exit", "target_pct", "target", "stop"
        )
        if (!("account_id" %in% colnames(dat_entry))) {
            col_select <- setdiff(col_select, "account_id")
        }
        
        dat_entry %>%
            left_join(dat_exit, by = col_join) %>% 
            filter(
                buy_sell_entry == -buy_sell_exit,
                size_entry == size_exit
            ) %>%
            rename(
                buy_sell = buy_sell_entry,
                size = size_entry
            ) %>%
            select(col_select) 
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    trd_all <- readTrades() 
    trd_all_grouped <- groupTradesAcrossAccounts(trd_all) 
    
    trd_matched <- matchTrades(trd_all) 
    trd_matched_grouped <- matchTrades(trd_all_grouped) 
    
    trd_unmatched <- anti_join(trd_all, trd_matched, by = "trade_id")
    trd_unmatched_grouped <- anti_join(trd_all_grouped, trd_matched_grouped, by = "trade_id")
    
    trd_live <- filter(trd_unmatched, trade_status == "live") %>%
        mutate(date_exit = date_entry + 7) %>%
        rename(price_entry = price) 
    trd_live_grouped <- filter(trd_unmatched_grouped, trade_status == "live") %>%
        mutate(date_exit = date_entry + 7) %>%
        rename(price_entry = price) 
    
    list(
        trd_all = trd_all,
        trd_all_grouped = trd_all_grouped,
        trd_matched = trd_matched,
        trd_matched_grouped = trd_matched_grouped,
        trd_unmatched = trd_unmatched,
        trd_unmatched_grouped = trd_unmatched_grouped,
        trd_live = trd_live,
        trd_live_grouped = trd_live_grouped
    )
}
B.readTradesFromIB <-
function (this_account_id, this_query) {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    nb_trys <- 10
    
    ib_token <- switch(
        this_account_id,
        "1" = "106713781439309215279050",
        "2" = "5179386948882217289232"
    )
    
    flex_query_id <- switch(
        this_account_id, 
        "1" = switch(this_query, "Trades" = 193803, "Trades_Today" = 235641, "PnL" = 254909),
        "2" = switch(this_query, "Trades" = 254481, "Trades_Today" = 254717)
    )
    
    url_ib <- "https://gdcdyn.interactivebrokers.com/Universal/servlet/FlexStatementService."
    url_query_1 <- paste0(url_ib, "SendRequest?t=")
    url_query_2 <- paste0(url_ib, "GetStatement?q=")
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    getLinkCode_Try <- function(this_url) {
        dat_ib <- this_url %>%
            read_html %>%
            xmlParse %>%
            xmlToList
        ib_code <- as.numeric(dat_ib$body$flexstatementresponse$code)
        if (is.na(ib_code))
            ib_code <- NULL
        ib_code
    }
    getLinkCode <- function(this_url) U.try(getLinkCode_Try)(this_url)
    
    retrievePageLinkCodeByTryingSeveralTimesBecauseItFailsFrequently_Try <- function() {
        url_1 <- "%s%s&q=%s&v=2" %>% 
            sprintf(url_query_1, ib_token, flex_query_id);
        try_id <- 1
        data_retrieved <- FALSE
        ib_code <- NULL
        
        while ((try_id < nb_trys) & !data_retrieved) {
            "Retrieving Account %s %s - Try #%s" %>%
                sprintf(this_account_id, this_query, try_id) %>%
                U.printBanner(FALSE);
            ib_code <- getLinkCode(url_1)
            if (is.null(ib_code)) 
                Sys.sleep(10) 
            else 
                data_retrieved <- TRUE
            try_id <- try_id + 1
        }
        ib_code
    }
    retrievePageLinkCode <- function()
        U.try(retrievePageLinkCodeByTryingSeveralTimesBecauseItFailsFrequently_Try)()
    
    retrieveIBReport_Try <- function(ib_code) {
        url_2 <- "%s%s&t=%s&v=2" %>% 
            sprintf(url_query_2, ib_code, ib_token);
        Sys.sleep(U.ifelse(this_query == "PnL", 240, 5))
        
        readCSV <- function(file_name) read.csv(file_name, stringsAsFactors = FALSE)
        dat <- U.try(readCSV)(url_2) %>%
            U.noData2Null
        dat <- cbind(account_id = this_account_id, dat)
        
        if (this_query %in% c("Trades", "Trades_Today")) {
            for (j in 1:ncol(dat)) dat[,j] <- U.vectorize(dat[,j])
            dat$Symbol <- as.character(dat$Symbol)
        }
        dat
    }
    retrieveIBReport <- function(ib_code)
        U.tryNull(retrieveIBReport_Try, ib_code)
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    retrievePageLinkCode() %>%
        retrieveIBReport
}
B.recentLegsNotMatched <-
function () 
{
    ####################################################################################################
    ### Script
    ####################################################################################################
    "SELECT * 
    FROM book_trade_leg 
    WHERE leg_id NOT IN (
        SELECT leg_id FROM book_trade_map
    )
    AND timestamp >= '%s'" %>%
        sprintf(YESTERDAY-7) %>%
        D.SQL %>%
        left_join(
            INSTRUMENTS %>% 
                mutate(
                    identifier = case_when(
                        asset_class %in% c("fx_dm", "fx_em") ~ instrument_id,
                        TRUE ~ conid_spot
                    )
                ) %>%
                select(pair, identifier),
            by = "identifier"
        )
}
B.save <-
function() {
    All_Objects <- ls(".GlobalEnv", pattern = "B.");
    All_Objects <- All_Objects[substr(All_Objects,1,2) == "B."];
    dump(All_Objects, paste0(DIRECTORY_CODE_HD, "/Code/Book.R"));
}
B.updateExecutionsFromfiles <-
function () {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    FILE_NAME <- paste0(DIRECTORY_DATA_HD, "Executions_IBAPI/executions_last.csv")

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    readBookedLegs <- function() {
        "SELECT * FROM book_trade_leg WHERE timestamp > '%s'" %>%
            sprintf(TO_DAY-28) %>% 
            D.SQL
    }
    readExecs <- function() {
        read_csv(FILE_NAME) %>% 
            group_by(account_id, ib_trade_id, ib_exec_id, identifier, buy_sell) %>% 
            summarize(
                timestamp = last(timestamp), 
                price = sum(size*price)/sum(size),
                size=sum(size)
            ) %>% 
            arrange(timestamp) %>%
            ungroup %>% 
            mutate(fees=0, fees_ccy_id=1, tax=0) 
    }
    
    removeLegsAlreadyinDB <- function(dat, dat_existing) {
        dat %>% 
            anti_join(dat_existing, by="ib_exec_id")
    }
    
    addLegIdAndFormatToDB <- function(dat, dat_existing) {
        max_leg_id <- max(dat_existing$leg_id)
        col_names <- colnames(dat_existing)
        dat %>% 
            mutate(leg_id = max_leg_id + 1:nrow(.)) %>% 
            select(all_of(col_names))
    }
    
    saveToDB <- function(dat) {
        D.insertDataIntoTable("book_trade_leg", dat, FALSE)
        dat
    }

    ####################################################################################################
    ### Script
    ####################################################################################################
    book_trade_legs <- readBookedLegs()
    
    readExecs() %>% 
        removeLegsAlreadyinDB(book_trade_legs) %>% 
        addLegIdAndFormatToDB(book_trade_legs) %>% 
        saveToDB
}
B.updateNAVFromfiles <-
function () {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    NB_DAYS <- 1
    FILE_PATH <- paste0(DIRECTORY_DATA_HD, "Account_Data/Account/")
    ACCOUNTS <- D.loadTableLocal("account")

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    accountCCY <- function(account_nb) {
        ACCOUNTS %>% 
            filter(account_id == account_nb) %>% 
            left_join(CURRENCIES, by="ccy_id") %>% 
            .$ccy
    }
    
    readNAV_Try <- function(file_name) {
        dat <- U.read.csv(paste0(FILE_PATH, file_name))
        dt <- file_name %>% 
            U.right(23) %>% 
            U.left(17) %>%
            paste0("00") %>%
            as.POSIXct
        
        account_nb = as.integer(substr(file_name, 9,9))
        account_ccy <- accountCCY(account_nb)
        
        fxRate <- function(ccy1) as.numeric(filter(dat, key == "ExchangeRate", ccy == ccy1)$value)
        fx_rate <- fxRate(account_ccy) / fxRate("USD")
        
        dat %>% 
            filter(key == "NetLiquidation") %>% 
            head(1) %>%
            mutate(
                account_id = account_nb,
                date = as.Date(dt),
                timestamp = dt,
                nav_ccy = as.numeric(value),
                nav_usd = nav_ccy * fx_rate
            ) %>% 
            select(account_id, date, timestamp, nav_ccy, nav_usd) 
    }
    readNAV <- function(file_name) U.tryNull(readNAV_Try, file_name)
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    file_list <- list.files(FILE_PATH)
    file_dates <- as.Date(substr(file_list, 11,20))
    file_list <- file_list[!is.na(file_dates)][file_dates >= (TO_DAY-NB_DAYS)]
    
    dat <- file_list %>% 
        lapply(readNAV) %>% 
        bind_rows
    D.replaceDataIntoTable("book_nav", dat, FALSE)
    dat

}
B.generateOrders <-
function(
    dat_predict = NULL,
    risk_per_bet_pct = 0.5,
    max_daily_risk_pct = 5,
    correlation_adjustment = 0,
    account_ids = c(1, 2),
    export_csv = TRUE,
    export_path = NULL
) {
    ####################################################################################################
    ### Script description:
    ### Converts signals to sized orders for execution via IB.
    ### Uses V.portfolioSizing() for eigenvalue-based N_effective sizing.
    ### Generates orders for multiple accounts, scaled by each account's NAV.
    ###
    ### Returns a list with:
    ###   orders_all: all orders combined
    ###   orders_by_account: list of tibbles, one per account
    ###   nav_data: NAV used for each account
    ###   sizing_metadata: N_effective, total_risk, correlation matrix
    ####################################################################################################

    ####################################################################################################
    ### Script variables
    ####################################################################################################
    if (is.null(export_path)) {
        export_path <- DIRECTORY_DATA_HD
    }

    FUTURES_ACTIVE <- D.loadTableLocal("future_active")
    FUTURES_EXPIRY <- D.loadTableLocal("future_expiry")
    FUTURES <- D.loadTableLocal("future_contract")
    TICK_SIZES <- D.loadTableLocal("instrument_attribute_dbl") %>%
        filter(attribute_id == 4) %>%
        select(instrument_id, tick_size = value)

    tradable_instruments <- c(
        'A50CNY', 'ASXAUD', 'AUDCAD', 'AUDCHF', 'AUDJPY', 'AUDNZD', 'AUDUSD',
        'CADJPY', 'CHFJPY', 'CHFSEK', 'DAXEUR', 'DJIUSD', 'EURAUD', 'EURCAD',
        'EURCHF', 'EURCZK', 'EURGBP', 'EURHUF', 'EURJPY', 'EURNOK', 'EURNZD',
        'EURPLN', 'EURSEK', 'EURUSD', 'FTSGBP', 'GBPAUD', 'GBPCAD', 'GBPCHF',
        'GBPJPY', 'GBPNZD', 'GBPSEK', 'GBPUSD', 'HSIHKD', 'IBXEUR', 'KSPKRW',
        'MIBEUR', 'NDXUSD', 'NKYJPY', 'NZDCAD', 'NZDCHF', 'NZDJPY', 'NZDUSD',
        'PX1EUR', 'RUTUSD', 'SEKJPY', 'SMICHF', 'SPXUSD', 'SSECNY', 'STXEUR',
        'TPXJPY', 'TSXCAD', 'USDBRL', 'USDCAD', 'USDCHF', 'USDCLP', 'USDINR',
        'USDJPY', 'USDMXN', 'USDNOK', 'USDSEK', 'USDSGD', 'USDZAR', 'XAGUSD',
        'XAUUSD', 'CHFNOK', 'GBPNOK', 'GBPPLN', 'NOKSEK', 'EEMUSD'
    )

    ####################################################################################################
    ### Sub routines
    ####################################################################################################

    loadSignals <- function() {
        if (is.null(dat_predict)) {
            G.Predict.Data.predict()
        } else {
            dat_predict
        }
    }

    getLatestNAV <- function() {
        "SELECT N.account_id, N.nav_ccy, N.nav_usd
        FROM book_nav N
        INNER JOIN (
            SELECT account_id, MAX(timestamp) as max_ts
            FROM book_nav
            WHERE date < '%s'
            GROUP BY account_id
        ) M ON N.account_id = M.account_id AND N.timestamp = M.max_ts" %>%
            sprintf(TO_DAY) %>%
            D.SQL
    }

    applyPortfolioSizing <- function(dat_signals) {
        instrument_ids <- unique(dat_signals$instrument_id)
        cor_matrix <- T.calcHistoricalCorrelationsMatrix(instrument_ids = instrument_ids, shrinkage = 0)

        dat_for_sizing <- dat_signals %>%
            select(instrument_id, buy_sell, notional)

        dat_sized <- V.portfolioSizing(
            dat_for_sizing,
            cor_matrix = cor_matrix,
            risk_per_bet_pct = risk_per_bet_pct,
            max_daily_risk_pct = max_daily_risk_pct,
            aum_total = sum(nav_data$nav_usd),
            correlation_adjustment = correlation_adjustment
        )

        list(
            dat_sized = dat_signals %>%
                mutate(
                    weight = dat_sized$weight,
                    sized_notional = dat_sized$sized_notional,
                    n_effective = dat_sized$n_effective[1]
                ),
            cor_matrix = cor_matrix,
            n_effective = dat_sized$n_effective[1]
        )
    }

    addInstrumentDetails <- function(dat_orders) {
        dat_orders %>%
            left_join(
                INSTRUMENTS %>%
                    select(instrument_id, pair, ticker, asset_class, conid_spot, trade_instrument_type),
                by = "instrument_id"
            ) %>%
            left_join(
                FUTURES_ACTIVE %>%
                    select(future_id, conid_active = conid),
                by = c("instrument_id" = "future_id")
            ) %>%
            left_join(
                FUTURES_EXPIRY %>%
                    select(conid, expiry),
                by = c("conid_active" = "conid")
            ) %>%
            left_join(TICK_SIZES, by = "instrument_id") %>%
            mutate(
                future_id = case_when(
                    asset_class %in% c("index", "bond", "metal") ~ instrument_id,
                    TRUE ~ NA_integer_
                ),
                conid = case_when(
                    !is.na(conid_active) ~ conid_active,
                    TRUE ~ conid_spot
                )
            ) %>%
            select(-conid_spot, -conid_active)
    }

    calculateTargetsAndStops <- function(dat_orders) {
        dat_orders %>%
            mutate(
                date_entry = TO_DAY,
                date_exit_latest = TO_DAY + 7
            )
    }

    scaleOrdersByAccountNAV <- function(dat_orders, account_id_this, nav_this) {
        total_nav <- sum(nav_data$nav_usd)
        nav_ratio <- nav_this / total_nav

        dat_orders %>%
            mutate(
                account_id = account_id_this,
                size_to_do = round(sized_notional * nav_ratio, 0)
            ) %>%
            filter(size_to_do > 0)
    }

    formatForExecution <- function(dat_orders) {
        dat_orders %>%
            mutate(
                order_id = row_number(),
                ib_order_id = NA_integer_,
                contract = NA_character_,
                buy_sell_action = case_when(buy_sell == 1 ~ "BUY", TRUE ~ "SELL"),
                px_order = price,
                px_live = NA_real_,
                px_avg = NA_real_,
                initial_position = 0L,
                position = 0L,
                filled = 0L,
                remaining = size_to_do,
                status = "not yet started",
                ib_status = ""
            ) %>%
            select(
                order_id, ib_order_id, account_id, instrument_id, ticker,
                future_id, conid, contract, tick_size,
                buy_sell, buy_sell_action, size_to_do, px_order, px_live, px_avg,
                initial_position, position, filled, remaining, status, ib_status
            )
    }

    exportOrdersCSV <- function(dat_orders, account_id_this) {
        date_str <- format(TO_DAY, "%Y%m%d")
        hour_str <- format(Sys.time(), "%H")

        dir_path <- paste0(
            export_path, "Orders/Combined/",
            format(TO_DAY, "%Y-%m"), "/",
            format(TO_DAY, "%Y-%m-%d"), "/"
        )

        if (!dir.exists(dir_path)) {
            dir.create(dir_path, recursive = TRUE)
        }

        file_name <- sprintf("combined_orders_%s_%s-%s.csv", account_id_this, date_str, hour_str)
        file_path <- paste0(dir_path, file_name)

        U.write.csv(dat_orders, file_path)
        U.printBanner(sprintf("Exported orders to: %s", file_path), FALSE)

        file_path
    }

    ####################################################################################################
    ### Script
    ####################################################################################################

    dat_signals <- loadSignals() %>%
        filter(
            signal_ok == TRUE,
            pair %in% tradable_instruments,
            buy_sell != 0
        ) %>%
        left_join(select(INSTRUMENTS, pair, instrument_id), by = "pair")

    if (nrow(dat_signals) == 0) {
        warning("No valid signals to generate orders")
        return(list(
            orders_all = tibble(),
            orders_by_account = list(),
            nav_data = tibble(),
            sizing_metadata = list()
        ))
    }

    nav_data <<- getLatestNAV() %>%
        filter(account_id %in% account_ids)

    if (nrow(nav_data) == 0) {
        stop("No NAV data found for specified accounts")
    }

    sizing_result <- applyPortfolioSizing(dat_signals)
    dat_sized <- sizing_result$dat_sized

    dat_orders_base <- dat_sized %>%
        addInstrumentDetails %>%
        calculateTargetsAndStops

    orders_by_account <- list()
    exported_files <- c()

    for (acc_id in account_ids) {
        nav_this <- filter(nav_data, account_id == acc_id)$nav_usd
        if (length(nav_this) == 0) {
            warning(sprintf("No NAV found for account %s, skipping", acc_id))
            next
        }

        dat_acc <- dat_orders_base %>%
            scaleOrdersByAccountNAV(acc_id, nav_this) %>%
            formatForExecution

        orders_by_account[[as.character(acc_id)]] <- dat_acc

        if (export_csv && nrow(dat_acc) > 0) {
            file_path <- exportOrdersCSV(dat_acc, acc_id)
            exported_files <- c(exported_files, file_path)
        }
    }

    orders_all <- bind_rows(orders_by_account)

    U.printBanner(sprintf(
        "Generated %d orders across %d accounts (N_eff = %.2f)",
        nrow(orders_all),
        length(orders_by_account),
        sizing_result$n_effective
    ))

    list(
        orders_all = orders_all,
        orders_by_account = orders_by_account,
        nav_data = nav_data,
        sizing_metadata = list(
            n_effective = sizing_result$n_effective,
            total_risk_pct = min(sizing_result$n_effective * risk_per_bet_pct, max_daily_risk_pct),
            correlation_matrix = sizing_result$cor_matrix
        ),
        exported_files = exported_files,
        signals_used = dat_orders_base %>%
            select(
                strategy_id, instrument_id, pair, ticker, asset_class,
                price, predict, target, stop, tp_pct,
                buy_sell, notional, weight, sized_notional, n_effective,
                date_entry, date_exit_latest
            )
    )
}
B.matchLegsToTrades <-
function(
    lookback_days = 7,
    price_tolerance_pct = 0.01,
    timestamp_tolerance_minutes = 60
) {
    ####################################################################################################
    ### Script description:
    ### Matches execution legs (from book_trade_leg) to trades.
    ### Determines if each leg is: ENTRY, TARGET, STOP, or MATURITY exit.
    ### Returns a summary for manual review before confirmation.
    ###
    ### Match types:
    ###   ENTRY: leg matches signal instrument_id + buy_sell, timestamp within tolerance
    ###   TARGET: closes live trade, price within tolerance of target
    ###   STOP: closes live trade, price within tolerance of stop
    ###   MATURITY: closes live trade, date >= date_exit_latest (7 days after entry)
    ####################################################################################################

    ####################################################################################################
    ### Script variables
    ####################################################################################################

    ####################################################################################################
    ### Sub routines
    ####################################################################################################

    getUnmatchedLegs <- function() {
        "SELECT L.*, I.pair, I.asset_class
        FROM book_trade_leg L
        LEFT JOIN (
            SELECT instrument_id, pair, asset_class,
                CASE WHEN asset_class IN ('fx_dm', 'fx_em') THEN instrument_id ELSE conid_spot END AS identifier
            FROM instrument
        ) I ON L.identifier = I.identifier
        WHERE L.leg_id NOT IN (SELECT leg_id FROM book_trade_map)
        AND L.timestamp >= '%s'" %>%
            sprintf(TO_DAY - lookback_days) %>%
            D.SQL %>%
            mutate(
                timestamp = as.POSIXct(timestamp, tz = TZ_LOCAL)
            )
    }

    getLiveTrades <- function() {
        "SELECT T.trade_id, T.strategy_id, T.date_entry, T.target_pct,
                M.leg_id AS entry_leg_id, L.identifier, L.buy_sell AS entry_buy_sell,
                L.price AS entry_price, L.size AS entry_size, L.account_id,
                I.pair, I.asset_class
        FROM book_trade T
        JOIN book_trade_map M ON M.trade_id = T.trade_id AND M.trade_category_id = 1
        JOIN book_trade_leg L ON L.leg_id = M.leg_id
        LEFT JOIN (
            SELECT instrument_id, pair, asset_class,
                CASE WHEN asset_class IN ('fx_dm', 'fx_em') THEN instrument_id ELSE conid_spot END AS identifier
            FROM instrument
        ) I ON L.identifier = I.identifier
        WHERE T.trade_outcome_id = 0 AND T.date_exit IS NULL" %>%
            D.SQL %>%
            mutate(
                date_entry = as.Date(date_entry),
                date_exit_latest = date_entry + 7,
                target_pct = target_pct / 100,
                target_price = entry_price * (1 + entry_buy_sell * target_pct),
                stop_price = 2 * entry_price - target_price
            )
    }

    getRecentSignals <- function() {
        instruments_with_identifier <- INSTRUMENTS %>%
            mutate(
                identifier = case_when(
                    asset_class %in% c("fx_dm", "fx_em") ~ instrument_id,
                    TRUE ~ conid_spot
                )
            ) %>%
            select(instrument_id, pair, asset_class, identifier)

        "SELECT strategy_id, instrument_id, timestamp, timestamp_px,
                close AS price, t_up, t_dn, outcome_id
        FROM live_predict
        WHERE date >= '%s'
        AND use_weights = 0" %>%
            sprintf(YESTERDAY) %>%
            D.SQL %>%
            left_join(TRADE_OUTCOMES, by = "outcome_id") %>%
            rename(predict = outcome) %>%
            mutate(
                timestamp = as.POSIXct(timestamp, tz = TZ_LOCAL),
                buy_sell = (predict == "up") - (predict == "down"),
                target = price + buy_sell * (t_up - price),
                stop = 2 * price - target,
                tp_pct = t_up / price - 1
            ) %>%
            left_join(instruments_with_identifier, by = "instrument_id")
    }

    matchLegToEntry <- function(leg, signals) {
        matching_signals <- signals %>%
            filter(
                identifier == leg$identifier,
                buy_sell == leg$buy_sell,
                abs(difftime(timestamp, leg$timestamp, units = "mins")) <= timestamp_tolerance_minutes
            ) %>%
            mutate(
                time_diff_minutes = abs(as.numeric(difftime(timestamp, leg$timestamp, units = "mins")))
            ) %>%
            arrange(time_diff_minutes)

        if (nrow(matching_signals) == 0) {
            return(NULL)
        }

        best_match <- matching_signals[1, ]

        confidence <- case_when(
            best_match$time_diff_minutes <= 5 ~ "HIGH",
            best_match$time_diff_minutes <= 15 ~ "MEDIUM",
            best_match$time_diff_minutes <= 30 ~ "LOW",
            TRUE ~ "VERY_LOW"
        )

        tibble(
            match_type = "ENTRY",
            suggested_trade_id = NA_integer_,
            suggested_strategy_id = best_match$strategy_id,
            confidence = confidence,
            price_diff_pct = NA_real_,
            time_diff_minutes = best_match$time_diff_minutes,
            expected_target = best_match$target,
            expected_stop = best_match$stop,
            tp_pct = best_match$tp_pct
        )
    }

    matchLegToExit <- function(leg, live_trades) {
        matching_trades <- live_trades %>%
            filter(
                identifier == leg$identifier,
                entry_buy_sell == -leg$buy_sell,
                account_id == leg$account_id
            )

        if (nrow(matching_trades) == 0) {
            return(NULL)
        }

        results <- matching_trades %>%
            mutate(
                price_diff_from_target = abs(leg$price - target_price) / entry_price,
                price_diff_from_stop = abs(leg$price - stop_price) / entry_price,
                is_past_maturity = as.Date(leg$timestamp) >= date_exit_latest
            )

        best_match <- results %>%
            mutate(
                exit_type = case_when(
                    price_diff_from_target <= price_tolerance_pct ~ "TARGET",
                    price_diff_from_stop <= price_tolerance_pct ~ "STOP",
                    is_past_maturity ~ "MATURITY",
                    TRUE ~ "UNKNOWN"
                ),
                price_diff_pct = case_when(
                    exit_type == "TARGET" ~ price_diff_from_target,
                    exit_type == "STOP" ~ price_diff_from_stop,
                    TRUE ~ pmin(price_diff_from_target, price_diff_from_stop)
                )
            ) %>%
            filter(exit_type != "UNKNOWN" | price_diff_pct <= 0.05) %>%
            arrange(price_diff_pct) %>%
            head(1)

        if (nrow(best_match) == 0) {
            return(NULL)
        }

        confidence <- case_when(
            best_match$exit_type == "MATURITY" ~ "HIGH",
            best_match$price_diff_pct <= 0.0025 ~ "HIGH",
            best_match$price_diff_pct <= 0.005 ~ "MEDIUM",
            best_match$price_diff_pct <= 0.01 ~ "LOW",
            TRUE ~ "VERY_LOW"
        )

        tibble(
            match_type = best_match$exit_type,
            suggested_trade_id = best_match$trade_id,
            suggested_strategy_id = best_match$strategy_id,
            confidence = confidence,
            price_diff_pct = best_match$price_diff_pct * 100,
            time_diff_minutes = NA_real_,
            expected_target = best_match$target_price,
            expected_stop = best_match$stop_price,
            tp_pct = best_match$target_pct
        )
    }

    buildInstruction <- function(match_row) {
        if (match_row$match_type == "ENTRY") {
            sprintf(
                "B.createNewTradeIDFromLegs('%s', %d, %.4f, c(%d))",
                as.character(as.Date(match_row$timestamp)),
                match_row$suggested_strategy_id,
                match_row$tp_pct,
                match_row$leg_id
            )
        } else if (match_row$match_type %in% c("TARGET", "STOP", "MATURITY")) {
            exit_type <- tolower(match_row$match_type)
            if (exit_type == "maturity") exit_type <- "exit_maturity"
            sprintf(
                "B.closeTradeFromLegs(%d, '%s', '%s', c(%d))",
                match_row$suggested_trade_id,
                exit_type,
                as.character(as.Date(match_row$timestamp)),
                match_row$leg_id
            )
        } else {
            "# UNKNOWN - manual review required"
        }
    }

    ####################################################################################################
    ### Script
    ####################################################################################################

    unmatched_legs <- getUnmatchedLegs()

    if (nrow(unmatched_legs) == 0) {
        U.printBanner("No unmatched legs found")
        return(tibble(
            leg_id = integer(),
            account_id = integer(),
            pair = character(),
            timestamp = as.POSIXct(character()),
            buy_sell = integer(),
            size = numeric(),
            price = numeric(),
            match_type = character(),
            confidence = character(),
            suggested_trade_id = integer(),
            suggested_strategy_id = integer(),
            price_diff_pct = numeric(),
            time_diff_minutes = numeric(),
            expected_target = numeric(),
            expected_stop = numeric(),
            instruction = character()
        ))
    }

    live_trades <- getLiveTrades()
    recent_signals <- getRecentSignals()

    U.printBanner(sprintf(
        "Matching %d unmatched legs against %d live trades and recent signals",
        nrow(unmatched_legs),
        nrow(live_trades)
    ))

    results <- list()

    for (i in 1:nrow(unmatched_legs)) {
        leg <- unmatched_legs[i, ]

        exit_match <- matchLegToExit(leg, live_trades)
        if (!is.null(exit_match)) {
            match_info <- exit_match
        } else {
            entry_match <- matchLegToEntry(leg, recent_signals)
            if (!is.null(entry_match)) {
                match_info <- entry_match
            } else {
                match_info <- tibble(
                    match_type = "UNKNOWN",
                    suggested_trade_id = NA_integer_,
                    suggested_strategy_id = NA_integer_,
                    confidence = "NONE",
                    price_diff_pct = NA_real_,
                    time_diff_minutes = NA_real_,
                    expected_target = NA_real_,
                    expected_stop = NA_real_,
                    tp_pct = NA_real_
                )
            }
        }

        results[[i]] <- tibble(
            leg_id = leg$leg_id,
            account_id = leg$account_id,
            pair = leg$pair,
            timestamp = leg$timestamp,
            buy_sell = leg$buy_sell,
            size = leg$size,
            price = leg$price
        ) %>%
            bind_cols(match_info)
    }

    match_summary <- bind_rows(results) %>%
        rowwise() %>%
        mutate(instruction = buildInstruction(cur_data())) %>%
        ungroup() %>%
        arrange(
            factor(confidence, levels = c("HIGH", "MEDIUM", "LOW", "VERY_LOW", "NONE")),
            match_type,
            timestamp
        )

    summary_counts <- match_summary %>%
        group_by(match_type, confidence) %>%
        summarise(n = n(), .groups = "drop")

    U.printBanner("Match Summary:")
    print(summary_counts)

    match_summary
}
B.confirmLegMatch <-
function(
    match_summary,
    confirm_types = c("HIGH"),
    dry_run = TRUE
) {
    ####################################################################################################
    ### Script description:
    ### Executes leg-to-trade matches after user review.
    ### For ENTRY matches: creates new trades via B.createNewTradeIDFromLegs()
    ### For EXIT matches: closes trades via B.closeTradeFromLegs()
    ###
    ### Parameters:
    ###   match_summary: output from B.matchLegsToTrades()
    ###   confirm_types: which confidence levels to auto-confirm (default: only "HIGH")
    ###   dry_run: if TRUE, only show what would happen (default: TRUE)
    ###
    ### Returns:
    ###   Summary of actions taken (or would be taken if dry_run=TRUE)
    ####################################################################################################

    ####################################################################################################
    ### Script variables
    ####################################################################################################

    ####################################################################################################
    ### Sub routines
    ####################################################################################################

    confirmEntry <- function(match_row) {
        trade_date <- as.character(as.Date(match_row$timestamp))
        strategy_id <- match_row$suggested_strategy_id
        tp_pct <- match_row$tp_pct
        leg_id <- match_row$leg_id

        if (dry_run) {
            return(tibble(
                action = "CREATE_TRADE",
                leg_id = leg_id,
                trade_id = NA_integer_,
                status = "DRY_RUN",
                message = sprintf(
                    "Would create trade: strategy=%d, tp_pct=%.4f, leg=%d",
                    strategy_id, tp_pct, leg_id
                )
            ))
        }

        tryCatch({
            new_trade_id <- B.createNewTradeIDFromLegs(
                trade_date = trade_date,
                strategy_id = strategy_id,
                tp_pct = tp_pct,
                leg_id_list = c(leg_id)
            )

            tibble(
                action = "CREATE_TRADE",
                leg_id = leg_id,
                trade_id = new_trade_id,
                status = "SUCCESS",
                message = sprintf("Created trade_id=%d", new_trade_id)
            )
        }, error = function(e) {
            tibble(
                action = "CREATE_TRADE",
                leg_id = leg_id,
                trade_id = NA_integer_,
                status = "ERROR",
                message = as.character(e$message)
            )
        })
    }

    confirmExit <- function(match_row) {
        trade_id <- match_row$suggested_trade_id
        exit_type <- tolower(match_row$match_type)
        if (exit_type == "maturity") exit_type <- "exit_maturity"
        exit_date <- as.character(as.Date(match_row$timestamp))
        leg_id <- match_row$leg_id

        if (dry_run) {
            return(tibble(
                action = "CLOSE_TRADE",
                leg_id = leg_id,
                trade_id = trade_id,
                status = "DRY_RUN",
                message = sprintf(
                    "Would close trade_id=%d with exit_type='%s', leg=%d",
                    trade_id, exit_type, leg_id
                )
            ))
        }

        tryCatch({
            B.closeTradeFromLegs(
                trade_id = trade_id,
                exit_type = exit_type,
                exit_date = exit_date,
                leg_id_list = c(leg_id)
            )

            tibble(
                action = "CLOSE_TRADE",
                leg_id = leg_id,
                trade_id = trade_id,
                status = "SUCCESS",
                message = sprintf("Closed trade_id=%d with exit_type='%s'", trade_id, exit_type)
            )
        }, error = function(e) {
            tibble(
                action = "CLOSE_TRADE",
                leg_id = leg_id,
                trade_id = trade_id,
                status = "ERROR",
                message = as.character(e$message)
            )
        })
    }

    ####################################################################################################
    ### Script
    ####################################################################################################

    to_confirm <- match_summary %>%
        filter(
            confidence %in% confirm_types,
            match_type != "UNKNOWN"
        )

    if (nrow(to_confirm) == 0) {
        U.printBanner("No matches to confirm with specified confidence levels")
        return(tibble(
            action = character(),
            leg_id = integer(),
            trade_id = integer(),
            status = character(),
            message = character()
        ))
    }

    U.printBanner(sprintf(
        "%s: Processing %d matches with confidence in [%s]",
        ifelse(dry_run, "DRY RUN", "EXECUTING"),
        nrow(to_confirm),
        paste(confirm_types, collapse = ", ")
    ))

    entries <- to_confirm %>% filter(match_type == "ENTRY")
    exits <- to_confirm %>% filter(match_type %in% c("TARGET", "STOP", "MATURITY"))

    results <- list()

    if (nrow(entries) > 0) {
        U.printBanner(sprintf("Processing %d ENTRY matches", nrow(entries)), FALSE)
        for (i in 1:nrow(entries)) {
            results[[length(results) + 1]] <- confirmEntry(entries[i, ])
        }
    }

    if (nrow(exits) > 0) {
        U.printBanner(sprintf("Processing %d EXIT matches", nrow(exits)), FALSE)
        for (i in 1:nrow(exits)) {
            results[[length(results) + 1]] <- confirmExit(exits[i, ])
        }
    }

    result_summary <- bind_rows(results)

    U.printBanner("Confirmation Results:")
    print(result_summary)

    if (dry_run) {
        U.printBanner("This was a DRY RUN. To execute, call with dry_run=FALSE")
    }

    result_summary
}
B.confirmSingleLeg <-
function(
    leg_id,
    match_type,
    trade_id = NULL,
    strategy_id = NULL,
    tp_pct = NULL,
    dry_run = TRUE
) {
    ####################################################################################################
    ### Script description:
    ### Manually confirm a single leg match when automatic matching fails or needs override.
    ###
    ### For ENTRY: requires strategy_id and tp_pct
    ### For TARGET/STOP/MATURITY: requires trade_id
    ####################################################################################################

    ####################################################################################################
    ### Script
    ####################################################################################################

    leg <- "SELECT * FROM book_trade_leg WHERE leg_id = %d" %>%
        sprintf(leg_id) %>%
        D.SQL

    if (nrow(leg) == 0) {
        stop(sprintf("Leg %d not found", leg_id))
    }

    match_type <- toupper(match_type)

    if (match_type == "ENTRY") {
        if (is.null(strategy_id) || is.null(tp_pct)) {
            stop("ENTRY match requires strategy_id and tp_pct")
        }

        if (dry_run) {
            U.printBanner(sprintf(
                "DRY RUN: Would create trade for leg %d (strategy=%d, tp_pct=%.4f)",
                leg_id, strategy_id, tp_pct
            ))
            return(invisible(NULL))
        }

        trade_date <- as.character(as.Date(leg$timestamp))
        new_trade_id <- B.createNewTradeIDFromLegs(
            trade_date = trade_date,
            strategy_id = strategy_id,
            tp_pct = tp_pct,
            leg_id_list = c(leg_id)
        )
        U.printBanner(sprintf("Created trade_id=%d for leg %d", new_trade_id, leg_id))
        return(new_trade_id)

    } else if (match_type %in% c("TARGET", "STOP", "MATURITY")) {
        if (is.null(trade_id)) {
            stop(sprintf("%s match requires trade_id", match_type))
        }

        exit_type <- tolower(match_type)
        if (exit_type == "maturity") exit_type <- "exit_maturity"

        if (dry_run) {
            U.printBanner(sprintf(
                "DRY RUN: Would close trade %d with %s for leg %d",
                trade_id, exit_type, leg_id
            ))
            return(invisible(NULL))
        }

        exit_date <- as.character(as.Date(leg$timestamp))
        B.closeTradeFromLegs(
            trade_id = trade_id,
            exit_type = exit_type,
            exit_date = exit_date,
            leg_id_list = c(leg_id)
        )
        U.printBanner(sprintf("Closed trade_id=%d with %s for leg %d", trade_id, exit_type, leg_id))
        return(trade_id)

    } else {
        stop(sprintf("Unknown match_type: %s. Use ENTRY, TARGET, STOP, or MATURITY", match_type))
    }
}
