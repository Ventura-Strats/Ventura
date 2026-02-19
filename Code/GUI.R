G.Book.FX.Table.fx_position <-
function(dat_trd) {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    dat_fx_usd <- "SELECT P.asset_id, L.price AS fx
            FROM (
            	SELECT asset_id, MAX(timestamp) AS timestamp
                FROM live_px_fx_book
                GROUP BY asset_id
                ) P
            LEFT JOIN live_px_fx_book L
            ON L.asset_id = P.asset_id AND L.timestamp = P.timestamp" %>%
        D.select %>%
        left_join(ASSETS, by = "asset_id") %>%
        rename(CCY = code) %>%
        select(CCY, fx)
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    getPositionsFromTrades <- function() {
        dat <- dat_trd$trd_live %>% 
            filter(instrument_type == "FX") %>%
            mutate(
                account_id = paste0("Expected_", account_id),
                ccy_1 = substr(pair, 1, 3),
                ccy_2 = substr(pair, 4, 6),
                notional_1 = buy_sell * size,
                notional_2 = -notional_1 * price_entry
            )
        
        dat_1 <- dat %>% 
            rename(CCY = ccy_1, notional = notional_1) %>%
            group_by(CCY, account_id) %>%
            summarize(notional = sum(notional)) %>% 
            ungroup
        
        dat_2 <- dat %>% 
            rename(CCY = ccy_2, notional = notional_2) %>%
            group_by(CCY, account_id) %>%
            summarize(notional = sum(notional)) %>% 
            ungroup 

        rbind(dat_1, dat_2) %>%
            left_join(dat_fx_usd, by = "CCY") %>%
            mutate(notional = notional * fx) %>%
            group_by(CCY, account_id) %>% 
            summarize(position = sum(notional)) %>%
            ungroup %>% 
            spread(account_id, position) %>%
            mutate(
                Total_Expected = rowSums(select(., starts_with("Expected_")))
            ) 
    }
    
    getPositionsFromBook <- function() {
        "SELECT B.account_id, B.asset_id, B.position
        FROM (
            SELECT asset_id, MAX(timestamp) AS timestamp
            FROM book_live_position_fx
            GROUP BY asset_id
        ) J
        LEFT JOIN book_live_position_fx B
        ON B.asset_id = J.asset_id AND B.timestamp = J.timestamp" %>%
            D.select  %>% 
            left_join(ASSETS, by = "asset_id") %>%
            rename(CCY = code) %>%
            left_join(dat_fx_usd, by = "CCY") %>%
            mutate(
                account_id = paste0("Actual_", account_id),
                position = position * fx
            ) %>%
            spread(account_id, position, fill = 0) %>%
            select(CCY, starts_with("Actual_")) %>% 
            mutate(Total_Actual = rowSums(select(., starts_with("Actual_")))) 
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_from_trd <- getPositionsFromTrades()
    dat_book <- getPositionsFromBook()
    nb_accounts <- ncol(select(dat_book, starts_with("Actual_")))

    dat <- select(dat_from_trd, CCY) %>% 
        rbind(select(dat_book, CCY)) %>%
        unique %>%
        filter(CCY != "USD") %>%
        arrange(CCY) %>%
        left_join(dat_from_trd, by = "CCY") %>%
        left_join(dat_book, by = "CCY") %>%
        U.dfReplaceNAColumnsWithZero(colnames(.)) 
    
    res <- select(dat, CCY) 
    for (i in 1:nb_accounts) {
        res <- cbind(res, select(dat, ends_with(as.character(i))))
        res[[paste0("Diff_", i)]] <- res[[paste0("Actual_", i)]] - res[[paste0("Expected_", i)]]
    }
    
    res <- res %>% 
        cbind(select(dat, "Total_Expected", "Total_Actual")) %>%
        mutate(Diff_Total = Total_Actual - Total_Expected)
    
    formats_list <- {}
    for (i in 2:ncol(res)) {
        formats_list[[colnames(res)[i]]] <- "#,###"
    }
    
    res %>%
        gvisTable(formats = formats_list)
}
G.Book.NAV.data.nav <-
function (start_date = "2021-01-01") 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    BACKTEST_RATE <- log(1 + 3.87650005113879)
    
    WIRE_TRANSFERS <- "SELECT * FROM book_wire_transfer" %>%
        D.SQL %>%
        mutate(
            timestamp = as.POSIXct(timestamp),
            date = as.Date(timestamp)
        ) %>%
        left_join(CURRENCIES, by = "ccy_id") %>%
        rename(code = ccy) %>%
        left_join(ASSETS, by = "code")
    
    FX_VS_USD = "SELECT * FROM histo_fx_close_vs_usd WHERE date >= '%s'" %>%
        sprintf(min(WIRE_TRANSFERS$date)) %>%
        D.select
    
    WIRE_TRANSFERS <- WIRE_TRANSFERS %>%
        left_join(FX_VS_USD, by = c("asset_id", "date")) %>%
        mutate(amount = amount * fx) %>%
        select(timestamp, amount) %>%
        rename(date = timestamp, transfer = amount)
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_plot <- "SELECT account_id, timestamp AS date, nav_usd FROM book_nav WHERE date >= '%s'" %>% 
        sprintf(as.Date(start_date)-1) %>%
        D.SQL %>% 
        mutate(
            account_id = paste0("A", account_id),
            date = as.POSIXct(date),
            #    date = date - second(date)
        ) %>% 
        filter(date >= start_date)
    
    start_time <- as.POSIXct(min(dat_plot$date))
    
    dat_plot %>% 
        spread(account_id, nav_usd) %>% 
        na.omit %>% 
        mutate(nav = A1+A2) %>% 
        as_tibble %>% 
        select(date, nav) %>% 
        left_join(WIRE_TRANSFERS, by = "date") %>%
        U.dfReplaceNAColumnsWithZero("transfer") %>%
        mutate(
            rtn = (nav - transfer) / lag(nav, 1, default = first(nav)) - 1,
            book = 100*cumprod(1+rtn)
        ) %>%
        select(date, book) %>%
        gather(nav, value, -date)
   
}
G.Book.NAV.plot <-
function (dat_plot, start_date) 
{
    ####################################################################################################
    ### Script
    ####################################################################################################

    dat_plot %>%
        filter(date >= start_date) %>%
        mutate(value = 100*value / first(value)) %>%
        ggplot(aes(x = date, y = value)) + 
        geom_step(color = "lightseagreen") + 
        scale_x_datetime() +
        scale_y_log10(n.breaks = 15) +
        theme(
            legend.position="None",
            axis.text=element_text(size=12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
        ) + ggtitle("NAV")
}
G.Book.Trades_Closed.Plot.grossPnLDistribution <-
function (dat_trd, date_start) 
{
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_trd$trd_matched %>%
        filter(date_entry >= date_start) %>%
        mutate(
            gross_pnl = (price_exit - price_entry) / (target - price_entry),
            size_pnl = gross_pnl * size
            ) %>%
        group_by(trade_id) %>%
        summarize(
            gross_pnl = sum(size_pnl) / sum(size)
        ) %>%
        ungroup %>%
        ggplot(aes(x=gross_pnl)) +
        geom_histogram(fill = "lightseagreen", bins = 50) + 
        aes(y=stat(count)/sum(stat(count))) + 
        scale_y_continuous(labels = scales::percent) + 
        scale_x_continuous(limits = c(-2.5, 2.5)) + 
        labs(
            title= "Gross PnL distribution",
            x = "PnL per trade (+1 = hit target, -1 = stopped)"
        ) + 
        theme(
            axis.title.y = element_blank(),
            axis.text=element_text(size=12),
            axis.title = element_text(size=12)
        )
}
G.Book.Trades_Closed.Table.monthly_stats <-
function (dat_trd, date_start) 
{
    
    print(date_start)
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_1 <- dat_trd$trd_matched %>% 
        filter(date_entry >= date_start) %>%
        group_by(trade_id) %>%
        summarize(date_entry = first(date_entry), trade_status = first(trade_status)) %>%
        ungroup %>%
        mutate(month = date_entry - day(date_entry) + 1) %>%
        select(month, trade_status) %>%
        table %>% 
        data.frame %>%
        spread(trade_status, Freq) %>%
        select(month, stop, flat, target) %>%
        mutate(
            Nb_Trades = stop + flat + target,
            stop = stop / Nb_Trades,
            flat = flat / Nb_Trades,
            target = target / Nb_Trades
            ) %>%
        rename(Month = month, Stop = stop, Flat = flat, Target = target) %>%
        arrange(Month) %>%
        mutate(Month = substr(Month, 1, 7))
    
    dat_1_total <- dat_trd$trd_matched %>% 
        filter(date_entry >= date_start) %>%
        group_by(trade_id) %>%
        summarize(date_entry = first(date_entry), trade_status = first(trade_status)) %>%
        ungroup %>%
        mutate(month = "Total") %>%
        select(month, trade_status) %>%
        table %>% 
        data.frame %>%
        spread(trade_status, Freq) %>%
        select(month, stop, flat, target) %>%
        mutate(
            Nb_Trades = stop + flat + target,
            stop = stop / Nb_Trades,
            flat = flat / Nb_Trades,
            target = target / Nb_Trades
        ) %>%
        rename(Month = month, Stop = stop, Flat = flat, Target = target) 
    
    dat_1 <- rbind(dat_1, dat_1_total) %>%
        select(Month, Nb_Trades, Stop, Flat, Target)

    dat_2 <- dat_trd$trd_matched %>% 
        filter(date_entry >= date_start) %>%
        mutate(p1 = price_entry * size, p2 = price_exit * size) %>%
        group_by(trade_id) %>%
        summarize(
            date_entry = first(date_entry),
            target = first(target),
            price_entry = sum(p1) / sum(size),
            price_exit = sum(p2) / sum(size)
            ) %>%
        ungroup %>%
        mutate(
            month = date_entry - day(date_entry) + 1,
            gross_pnl = (price_exit - price_entry) / (target - price_entry)
            ) %>%
        select(month, gross_pnl) %>%
        group_by(month) %>%
        summarize(gross_pnl = mean(gross_pnl)) %>%
        ungroup %>%
        rename(Month = month, Gross_PnL = gross_pnl) %>%
        arrange(Month) %>%
        mutate(Month = substr(Month, 1, 7)) 
    
    dat_2_total <- dat_trd$trd_matched %>% 
        filter(date_entry >= date_start) %>%
        mutate(p1 = price_entry * size, p2 = price_exit * size) %>%
        group_by(trade_id) %>%
        summarize(
            date_entry = first(date_entry),
            target = first(target),
            price_entry = sum(p1) / sum(size),
            price_exit = sum(p2) / sum(size)
        ) %>%
        ungroup %>%
        mutate(
            month = "Total",
            gross_pnl = (price_exit - price_entry) / (target - price_entry)
        ) %>%
        select(month, gross_pnl) %>%
        group_by(month) %>%
        summarize(gross_pnl = mean(gross_pnl)) %>%
        ungroup %>%
        rename(Month = month, Gross_PnL = gross_pnl)
    
    dat_2 <- rbind(dat_2, dat_2_total)
    
    dat_1 %>%
        left_join(dat_2, by = "Month") %>%
        gvisTable(formats = list(Stop = "#.#%", Flat = "#.#%", Target = "#.#%"))
    
}
G.Book.Trades_Closed.Table.trades_closed <-
function(dat_trd, date_start) {
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_trd$trd_matched_grouped %>%
        filter(date_entry >= date_start) %>%
        select(
            trade_id, strategy_id, pair, instrument_type, date_entry, date_exit, buy_sell, size, 
            price_entry, price_exit, target_pct, target, stop, trade_status
        ) %>%
        arrange(-trade_id) %>%
        mutate(
            trade_status = case_when(
                trade_status == "flat" ~ "Exit Maturity",
                TRUE ~ trade_status
            ),
            trade_status = str_to_title(trade_status),
            Gross_PnL = (price_exit - price_entry) / (target - price_entry)
        ) %>%
        rename(
            Trade = trade_id,
            Strategy = strategy_id,
            Pair = pair,
            Type = instrument_type,
            Date_Entry = date_entry,
            Date_Exit = date_exit,
            Buy_Sell = buy_sell,
            Size = size,
            Price_Entry = price_entry,
            Price_Exit = price_exit,
            Target_Pct = target_pct,
            Target = target,
            Stop = stop,
            Outcome = trade_status
        ) %>%
        datatable(
            rownames = FALSE,
            options = list(
                paging = FALSE,
                info = FALSE,
                columnDefs = list(list(className = 'dt-right', targets = "_all"))
            )) %>% 
        #formatStyle("Gross_PnL", backgroundColor = styleInterval(pnl_breaks, pnl_colors)) %>%
    #    formatStyle("Model", backgroundColor = styleEqual(FALSE, "lightcoral")) %>%
        formatPercentage("Target_Pct", 2) %>%
        formatPercentage("Gross_PnL", 1) %>%
        formatRound(c("Price_Entry", "Price_Exit"), digits = 5) %>%
        formatRound("Size", digits = 1)
    
        #gvisTable(
        #    formats = list(
        #        Target_Pct = "#.###%",
        #        Gross_PnL = "#.#%",
        #        Price_Entry = "#,###.######",
        #        Price_Exit = "#,###.######",
        #        Size = "#,###"
        #    )
        #)
}
G.Book.Trades_Live.Table.trades_live <-
function(dat_trd, output = "screen") {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    px_live <- D.SQL("SELECT instrument_id, close AS price_instrument FROM live_px")
    px_live_fut <- "SELECT J.conid, L.price AS price_future
        FROM (
        	SELECT conid, MAX(timestamp) AS timestamp
            FROM live_px_future
            WHERE timestamp >= '%s'
            GROUP BY conid
        ) J
        LEFT JOIN live_px_future L
        ON L.conid = J.conid AND L.timestamp = J.timestamp" %>%
        sprintf(YESTERDAY) %>%
        D.SQL
    
    fx_vs_usd <- "SELECT * FROM histo_fx_close_vs_usd WHERE date >= '%s'" %>%
        sprintf(min(dat_trd$trd_live_grouped$date_entry)) %>% 
        D.select %>%
        rename(date_entry = date)
    
    path_px_position <- paste0(DIRECTORY_DATA_HD, "Account_Data/Px_Position/")
    
    FUTURE_SIZE <- D.loadTableLocal("future_contract") %>% 
        select(instrument_id, notional)
    
    FUTURE_PAIR <- D.loadTableLocal("future_contract") %>%
        select(instrument_id, ib_symbol) %>%
        left_join(INSTRUMENTS, by = "instrument_id") %>%
        rename(symbol = ib_symbol) %>%
        select(symbol, pair)
    
    FUTURE_CCY <- D.loadTableLocal("future_contract") %>%
        left_join(CURRENCIES, by = "ccy_id") %>%
        select(future_id, instrument_id, ccy) %>%
        rename(code = ccy) %>%
        left_join(select(ASSETS, asset_id, code), by = "code") %>%
        rename(asset_id_fut = asset_id) %>%
        select(-code) %>%
        left_join(select(INSTRUMENTS, instrument_id, ccy), by = "instrument_id") %>%
        rename(code = ccy) %>%
        left_join(select(ASSETS, asset_id, code), by = "code") %>%
        rename(asset_id_inst = asset_id) %>%
        select(-code, -future_id) 
    
    ETF <- D.loadTableLocal("ETF") %>%
        left_join(select(INSTRUMENTS, instrument_id, pair), by = "instrument_id") %>%
        rename(pair_etf = pair)
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    getLatestPriceFromBookPosition_Try <- function() {
        dat_1 <- U.try(U.read.csv)(paste0(path_px_position, "px_position_1_last.csv"))
        dat_2 <- NULL # the python retriever doesn't work for this account need to fix
        #U.try(U.read.csv)(paste0(path_px_position, "px_position_2_last.csv"))
        
        rbind(dat_1, dat_2) %>%
            left_join(FUTURE_PAIR, by = "symbol") %>%
            left_join(select(ETF, conid, pair_etf), by = "conid") %>%
            mutate(
                instrument_type = case_when(
                    secType == "CASH" ~ "FX",
                    secType == "FUT" ~ "FUT",
                    secType == "STK" ~ "ETF",
                ),
                pair = case_when(
                    instrument_type == "FX" ~ paste0(symbol, currency),
                    instrument_type == "FUT" ~ pair,
                    instrument_type == "ETF" ~ pair_etf
                )
            ) %>%
            left_join(select(INSTRUMENTS, instrument_id, pair), by = "pair") %>%
            group_by(instrument_type, instrument_id, conid) %>% 
            summarize(price_book = mean(price)) %>% 
            ungroup
    }    
    getLatestPriceFromBookPosition <- function() 
        U.try(getLatestPriceFromBookPosition_Try)()
    
    ####################################################################################################
    ### Script
    ####################################################################################################

    px_live_book <- getLatestPriceFromBookPosition() 

    px_live_book_fx <- px_live_book %>%
        filter(instrument_type == "FX") %>%
        select(instrument_id, price_book)

    px_live <- px_live %>% 
        left_join(px_live_book_fx, by = "instrument_id") %>%
        mutate(
            price_instrument = case_when(
                !is.na(price_book) ~ price_book,
                TRUE ~ price_instrument
            )
        ) %>%
        select(-price_book)

    px_live_book_fut <- px_live_book %>%
        filter(instrument_type %in% c("FUT", "ETF")) %>%
        select(conid, price_book)

    px_live_fut <- px_live_fut %>% 
        left_join(px_live_book_fut, by = "conid") %>% 
        mutate(
            price_future = case_when(
                !is.na(price_book) ~ price_book,
                TRUE ~ price_future
            )
        ) %>%
        select(-price_book)

    px_live_book <- px_live_book %>%
        anti_join(px_live_fut, by = "conid") %>%
        rename(price_future = price_book) %>%
        select(conid, price_future)
    px_live_fut <- rbind(px_live_fut , px_live_book)

    ### ABSOLUMENT DEGUEU A REFAIRE COMPLETEMENT EN TRAITANT CHAQUE TRADE TYPE PROPREMENT
    
    dat <- dat_trd$trd_live_grouped %>%
        left_join(px_live, by = "instrument_id") %>%
        left_join(px_live_fut, by = "conid") %>%
        mutate(
            price_live = case_when(
                instrument_type %in% c("Future", "ETF") ~ price_future,
                instrument_type == "FX" ~ price_instrument,
                TRUE ~ NUM_NA
            ),
            code = substr(pair, 4, 6)
        ) %>% 
        left_join(FUTURE_CCY, by = "instrument_id") %>% 
        rename(asset_id = asset_id_fut) %>%
        left_join(fx_vs_usd, by = c("asset_id", "date_entry")) %>% 
        rename(fx_fut = fx) %>% 
        select(-asset_id) %>%
        rename(asset_id = asset_id_inst) %>% 
        left_join(fx_vs_usd, by = c("asset_id", "date_entry")) %>% 
        rename(fx_inst = fx) %>%
        select(-asset_id) %>% 
        left_join(ASSETS, by = "code") %>%
        left_join(fx_vs_usd, by = c("asset_id", "date_entry")) %>% 
        mutate(
            fx_inst = case_when(
                is.na(fx_inst) ~ fx,
                TRUE ~ fx_inst
            ),
            fx_fut = case_when(
                is.na(fx_fut) ~ 1,
                TRUE ~ fx_fut
            )
        ) %>%
        left_join(FUTURE_SIZE, by = "instrument_id") %>%
        mutate(
            notional_multiplier = case_when(
                instrument_type == "Future" ~ notional,
                instrument_type == "FX" ~ 1,
                instrument_type == "ETF" ~ 1,
                TRUE ~ NUM_NA
            ),
            fx_multiplier = case_when(
                instrument_type == "Future" ~ 1,
                instrument_type == "FX" ~ fx_inst,
                instrument_type == "ETF" ~ 1,
                TRUE ~ NUM_NA
            )
        ) %>%
        mutate(
            amount_ccy = size * notional_multiplier * price_entry,
            PnL_Target_USD = amount_ccy * fx * target_pct * fx_fut / fx_inst * fx_multiplier
        ) %>%
        select(
            trade_id, strategy_id, pair, instrument_type, expiry, 
            date_entry, date_exit, buy_sell, size, 
            price_entry, price_live, target_pct, target, stop, 
            PnL_Target_USD
        ) %>%
        arrange(date_exit, date_entry, strategy_id, pair) %>%
        mutate(
            Gross_PnL = (price_live - price_entry) / (target - price_entry),
            PnL_Target_USD = round(PnL_Target_USD, 0),
            PnL_Live_USD = round(Gross_PnL * PnL_Target_USD, 0)
        ) 
    
    dat_total <- head(dat, 1)
    for (j in 1:(ncol(dat_total))) {
        dat_total[,j] <- NA
    }
    
    dat_total$trade_id <- sprintf("Total (%s)", nrow(dat))
    dat_total$PnL_Live_USD <- sum(dat$PnL_Live_USD, na.rm=TRUE)
    
    dat <- rbind(dat, dat_total) 
    
    formatX <- function(x) { if (is.na(x)) CHAR_NA else format(x, big.mark = ",") }
    formatCol <- function(x_vector) U.sapply(x_vector, formatX)
    
    dat$size <- formatCol(dat$size)
    dat$price_entry <- formatCol(dat$price_entry)
    dat$price_live <- formatCol(dat$price_live)
    dat$target <- formatCol(dat$target)
    dat$stop <- formatCol(dat$stop)
    dat$PnL_Live_USD <- formatCol(dat$PnL_Live_USD)
    dat$PnL_Target_USD <- formatCol(dat$PnL_Target_USD)
    
    dat <- dat %>%
        rename(
            Trade = trade_id,
            Strategy = strategy_id,
            Pair = pair,
            Type = instrument_type,
            Expiry = expiry,
            Entry = date_entry,
            Latest_Exit = date_exit,
            Buy_Sell = buy_sell,
            Size = size,
            Price_Entry = price_entry,
            Price_Live = price_live,
            Target_Pct = target_pct,
            Target = target,
            Stop = stop
        ) 
    
    nb_breaks <- 8
    breaks_left <- seq(-1, -0.05, length.out = 1+nb_breaks)
    breaks_right <- seq(0.05, 1, length.out = 1+nb_breaks) 
    
    colors_left <- round(seq(0, 255, length.out = 2+nb_breaks), 0) %>%
        {paste0("rgb(255,", ., ",", ., ")")}
    
    colors_right <- round(seq(255, 0, length.out = 2+nb_breaks), 0) %>%
        {paste0("rgb(", ., ",", 255, ",", ., ")")}
    
    pnl_breaks <- c(breaks_left, breaks_right)
    pnl_colors <- c(colors_left, tail(colors_right, -1))
    
    if (output == "screen") {
        dat_account <- dat_trd$trd_live %>% 
            select(trade_id, account_id, size) %>% 
            spread(account_id, size) %>% 
            rename(
                Account_1 = "1",
                Trade = trade_id
            ) %>%
            mutate(Trade = as.character(Trade))
        if ("2" %in% colnames(dat_account)) {
            dat_account <- dat_account %>% 
                rename(Account_2 = "2")
        } 
        else {
            dat_account$Account_2 <- 0
        }

        dat <- dat %>% 
            left_join(dat_account, by="Trade") %>%
            select(
                Trade, Strategy, Pair, Type, Expiry, Entry, Latest_Exit, Buy_Sell, Size, 
                Account_1, Account_2, Price_Entry, Price_Live, 
                Target_Pct, Target, Stop, PnL_Target_USD, Gross_PnL, PnL_Live_USD
            )
        dat$Account_1 <- formatCol(dat$Account_1)
        dat$Account_2 <- formatCol(dat$Account_2)
        dat <- dat %>% 
            datatable(
                rownames = FALSE,
                options = list(
                    paging = FALSE,
                    info = FALSE,
                    columnDefs = list(list(className = 'dt-right', targets = "_all"))
                )) %>% 
            formatStyle("Gross_PnL", backgroundColor = styleInterval(pnl_breaks, pnl_colors)) %>%
            formatStyle("Latest_Exit", backgroundColor = styleEqual(TO_DAY, "lightcoral")) %>%
            formatPercentage("Target_Pct", 2) %>%
            formatPercentage("Gross_PnL", 0)
    }
    dat
}
G.Diagnostic.Instruments.Table.status <-
function () 
{
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    dat_status_technicals <- "SELECT instrument_id, MIN(date) as min_technicals_date
        FROM histo_technicals_dbl GROUP BY instrument_id" %>%
        D.select
    
    dat_status_px <- "SELECT instrument_id, MIN(date) as min_price_date
        FROM histo_px_daily GROUP BY instrument_id" %>%
        D.select
    
    dat <- "SELECT R.region, L.asset_class, I.ticker, CONCAT(A.code, C.ccy) AS pair, S.*, F.ib_symbol AS future, 
        X.expiry, P.timestamp AS future_px_time
    FROM status_instrument S 
    LEFT JOIN static_instrument I ON I.instrument_id = S.instrument_id
    LEFT JOIN static_asset_class L ON L.asset_class_id = I.asset_class_id
    LEFT JOIN static_currency C ON C.ccy_id = I.ccy_id
    LEFT JOIN static_asset A ON A.asset_id = I.asset_id
    LEFT JOIN static_market M ON M.market_id = I.market_id
    LEFT JOIN static_region R ON R.region_id = M.region_id
    LEFT JOIN static_future_contract F ON F.instrument_id = I.instrument_id
    LEFT JOIN static_future_active V ON V.future_id = F.future_id
    LEFT JOIN static_future_expiry X ON X.conid = V.conid
    LEFT JOIN live_px_future P ON P.conid = V.conid
    ORDER BY R.region, L.asset_class, M.market, I.instrument_id" %>% 
        D.SQL %>%
        left_join(dat_status_px, by="instrument_id") %>% 
        left_join(dat_status_technicals, by="instrument_id") %>% 
        select(-instrument_id) %>% 
        filter(asset_class != "yield")
    
    for (this_col in colnames(dat)) {
        if (all(is.na(dat[[this_col]]))) {
            dat[this_col] <- NULL
        }
    }
    dat %>%
        datatable(rownames = FALSE, options = list(paging = FALSE, info = FALSE))

}
G.Diagnostic.Jobs.Data.Activity <-
function (diagnostic_date) {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    path_python_log <- paste0(DIRECTORY_DATA_SD, "Log/Python")
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    getActivityFromDBLog <- function() {
        dat <- 
            "SELECT C.category, S.script, M.machine, L.run_id, L.start, L.end
            FROM status_script L 
            LEFT JOIN static_script S ON S.script_id = L.script_id 
            LEFT JOIN static_machine M ON M.machine_id = L.machine_id
            LEFT JOIN static_script_category C ON C.category_id = S.category_id
            WHERE L.start >= '%s' 
            AND L.end <= '%s' 
            AND L.end IS NOT NULL
            ORDER BY C.category, S.script DESC, L.start ASC" %>%
            sprintf(diagnostic_date - 1, diagnostic_date + 2) %>% 
            D.SQL
        
        dat$start <- as.POSIXct(dat$start, tz = TZ_LOCAL)
        dat$end <- as.POSIXct(dat$end, tz = TZ_LOCAL)
        
        dat
    }
    
    readPattern <- function(pattern, file_lines) {
        pattern <- paste0("### ", pattern, ": ")
        pos_pattern <- which(grepl(pattern, file_lines))[1]
        gsub(pattern, "", file_lines[pos_pattern]) %>%
            gsub(" ", "", .)
    }
    
    getPythonActivityFromOneLogFile_Try <- function(file_name) {
        res <- NULL
        pos_name_time_start_first <- gregexpr(".py1_", file_name, fixed = TRUE)[[1]][1] + 5
        if (pos_name_time_start_first == 4)
            pos_name_time_start_first <- gregexpr(".py2_", file_name, fixed = TRUE)[[1]][1] + 5
        if (pos_name_time_start_first == 4)
            pos_name_time_start_first <- gregexpr(".py_", file_name, fixed = TRUE)[[1]][1] + 4
        
        pos_name_time_start_last <- gregexpr(".txt", file_name, fixed = TRUE)[[1]][1] - 1
        
        run_start <- file_name %>%
            substr(pos_name_time_start_first, pos_name_time_start_last) %>% 
            as.POSIXct(format = "%Y-%m-%d_%H%M%S", tz = TZ_LOCAL)
        
        run_end <- file_name %>% 
            file.mtime %>%
            as.POSIXct(tz = TZ_LOCAL)
        print(c(run_start, run_end));
        if ((run_start >= diagnostic_date - 1) & (run_end <= diagnostic_date + 2)) {
            dat <- readLines(file_name)
            script <- readPattern("Script Name", dat)
            machine <- readPattern("This Computer", dat)
            res <- data.frame(script, machine, run_start, run_end, stringsAsFactors = FALSE)
        }
        res;
    }
    getPythonActivityFromOneLogFile <- function(file_name) 
        U.try(getPythonActivityFromOneLogFile_Try, NULL)(file_name)
    
    getPythonActivityFromLogFiles <- function(path_file) {
        path_file %>%
            list.files(pattern = ".py", full.names = TRUE) %>%
            lapply(getPythonActivityFromOneLogFile) %>% 
            bind_rows
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    
    dat_R <- getActivityFromDBLog() 
  #  dat_P_F <- getPythonActivityFromLogFiles(path_python_fx);
    dat_P_F <- NULL
    activity_data <- rbind(dat_R, dat_P_F) %>%
        arrange(category, script, machine, run_id)
    activity_data$category <- as.factor(activity_data$category)
    activity_data$script <- as.factor(activity_data$script)
    activity_data$machine <- as.factor(activity_data$machine)
    activity_data %>%
        as_tibble
}
G.Diagnostic.Jobs.Plot.Utilization <-
function (activity_data, diagnostic_date) {
    
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    start_time <- as.POSIXct(paste0(diagnostic_date, " 00:00:00")) 
    end_time <- as.POSIXct(paste0(diagnostic_date + 1, " 00:00:00")) 
    
    shutdown_time_start <- diagnostic_date %>%
        paste0(" 01:10:00") %>%
        as.POSIXct(tz = TZ_ASIA) %>%
        format(tz = TZ_LOCAL) %>%
        as.POSIXct
    shutdown_time_end <- shutdown_time_start + 30 * 60
    
    ib_reboot_time <- diagnostic_date %>%
        paste0(" 06:58:00") %>%
        as.POSIXct(tz = TZ_ASIA) %>%
        format(tz = TZ_LOCAL) %>%
        as.POSIXct
    
    execution_times <- D.loadTableLocal("schedule_execution") %>%
        mutate(
            execution_time = as.POSIXct(paste0(TO_DAY, " ", execution_time), tz = TZ_LOCAL)
            ) %>%
        .$execution_time
    
    ip_address <- U.readIPAddress()
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    activity_data <- activity_data %>%
        filter(
            start <= end_time,
            end >= start_time
        )
    
    pos_not_0 <- which(activity_data$run_id != 0)
    activity_data$run_id[which(activity_data$run_id == 0)] <- INT_NA
    activity_data$run_id[pos_not_0] <- U.right(paste0("0", activity_data$run_id[pos_not_0]), 2)
    
    activity_data <- activity_data %>% 
        unite("script", c("category", "script", "run_id"), na.rm = TRUE) %>%
        mutate(script = factor(script))

    script_levels <- rev(levels(activity_data$script))
    
    activity_data <- activity_data %>%
        mutate(
            end = pmax(end, start + 30), #otherwise doesn't display anything
            start = pmax(start_time, start),
            end = pmin(end_time, end)
        )

    activity_data$start[which(activity_data$start < start_time)] <- start_time
    activity_data$end[which(activity_data$end > end_time)] <- end_time

    activity_data %>%
        ggplot(aes(x = start, y = script, col = machine)) + 
        geom_segment(
            aes(xend = end, yend = script), 
            size = 8
        ) +
        scale_x_datetime(
            limits=c(start_time, end_time),
            breaks = date_breaks("1 hour"), 
            date_labels = "%Hh", 
            minor_breaks = NULL,
            expand = c(0, 0)
        ) +
        scale_y_discrete(name="", limits = script_levels) +
        theme(
            legend.position="bottom", 
            legend.text=element_text(size=13),
            axis.text=element_text(size=13),
            axis.title.x = element_blank()
        ) +
        annotate(
            "rect", 
            xmin = shutdown_time_start, 
            xmax = shutdown_time_end, 
            alpha = 0.2, 
            ymin = -Inf, 
            ymax = Inf
        ) + 
        geom_vline(xintercept = execution_times, linetype="dotted", color = "red") +
        geom_vline(xintercept = ib_reboot_time, linetype="dotted", color = "blue") + 
        ggtitle(ip_address)
    
}
G.Instrument.Plot.technicalChart <-
function (instrument, date_start, date_end, strat_id) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    this_instrument_id <- A.getInstrumentId(instrument)
    strat_id <- as.integer(strat_id)
    date_start <- as.Date(date_start)
    date_end <- as.Date(date_end)
    
    bb_width <- 2.5
    
    FEATURES <- D.loadTable("static_feature")
    
    feature_id_for_price_chart <- data.frame(feature = c(
        "open", "high", "low", "close", "vol_lt",
        "spline_fast", "spline_medium", "spline_slow",
        "bb_up", "bb_dn"
        )) %>%
        left_join(FEATURES, by = "feature") %>%
        select(feature_id) %>%
        U.vectorize
        
    feature_id_for_strategy <- 
        "SELECT feature_id 
        FROM strategy_feature 
        WHERE strategy_id = %s 
        AND market_configuration = 1" %>%
        sprintf(strat_id) %>%
        D.select %>%
        U.vectorize %>% 
        setdiff(filter(
            FEATURES, 
            feature %in% c("asset_class", "asset_class_id", "ccy_1_reference")
        )$feature_id)
    
    feature_id_list <- c(feature_id_for_price_chart, feature_id_for_strategy) %>%
        U.vectorizeUnique
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################

    ####
    # plot 1
    ####
    if (FALSE) {
        date_from <- as.Date(paste0(year_start, "-01-01"))
        date_to <- as.Date(paste0(year_end, "-12-31"))
        
        dat <- T.getHistoPx(instrument, date_from, date_to)
        
        year_start <- min(dat$year)
        year_end <- max(dat$year)
        duration_series <- (year_end - year_start)
        
        plot_breaks <- 1
        if (duration_series > 50) plot_breaks <- 2
        
        plot_breaks <- paste0(plot_breaks, " years")
        
        plot_1 <- dat %>% 
            ggplot(aes(x = date, y = close)) + 
            geom_line(color = "lightseagreen") + 
            geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
            scale_x_date(date_breaks = plot_breaks, date_labels = "%y") + 
            theme(
                legend.position = "bottom",
                axis.text=element_text(size = 10), 
                axis.title.y = element_blank(),
                axis.title.x = element_blank()
            ) +
            ggtitle(fx_pair)
        print(plot_1)
    }

    
    ######
    #####

    getHistoTechnicalsDB <- function(histo_or_live) {
        "SELECT T.date, T.feature_id, T.value
            FROM %s_technicals_dbl T
            LEFT JOIN static_feature F ON F.feature_id = T.feature_id
            WHERE T.instrument_id = %s
            AND T.date BETWEEN '%s' AND '%s'
            AND (T.feature_id IN (%s) OR LEFT(F.feature, 3) = 'lvl')" %>%
            sprintf(
                histo_or_live,
                this_instrument_id,
                date_start,
                date_end,
                paste(feature_id_list, collapse = ",")
            ) %>%
            D.SQL %>%
            left_join(FEATURES, by = "feature_id") %>%
            select(date, feature, value) %>%
            mutate(date = as.Date(date)) %>%
            arrange(date) %>%
            spread(feature, value)
    }
    
    prepareData <- function() {
        dat_histo <- getHistoTechnicalsDB("histo")
        dat_live <-  getHistoTechnicalsDB("live")

        dat_histo %>%
            anti_join(dat_live, by = "date") %>%
            rbind(dat_live) %>%
            arrange(date) %>%
            mutate(
                bb_up = spline_medium + bb_width * (bb_up - spline_medium),
                bb_dn = spline_medium + bb_width * (bb_dn - spline_medium)
            )
    }
    
    prepareTradingZones <- function(dat, y_min, y_max) {
        dat <- dat %>% 
            arrange(date) %>%
            tail(1);
        vol_lt <- dat$vol_lt[1]
        px <- dat$close[1]

        dat %>% 
            select(starts_with("lvl")) %>%
            gather(level, value) %>%
            filter(abs(value) < 100) %>%
            mutate(
                value = px * exp(value * vol_lt)
            ) %>% 
            arrange(value) %>%
            select(value) %>% 
            filter(value >= y_min, value <= y_max) %>%
            U.vectorize
    }
    
    prepareMainPlotPriceSplineBB <- function(dat) {
        dat <- filter(dat, date >= date_start, date <= date_end)
        y_max <- max(dat$high);
        y_min <- min(dat$low);
        min_max <- y_max - y_min
        y_max <- y_max + min_max / 20
        y_min <- y_min - min_max / 20

        dat_levels <- prepareTradingZones(dat, y_min, y_max)
        
        dat %>% 
            ggplot(aes(x=date, y=close)) +
            geom_candlestick(aes(x=date, open=open, high=high, low=low, close=close)) +
            geom_line(aes(y=spline_fast), color ="purple", linetype="solid", size=0.75, alpha=0.75) +
            geom_line(aes(y=spline_medium), color="green", linetype="solid", size=0.75, alpha=0.75) +
            geom_line(aes(y=spline_slow), color="orange", linetype="solid", size=0.75, alpha=0.75) + 
            geom_ribbon(
                aes(x = date, ymin=bb_dn, ymax=bb_up), 
                fill = "grey60",
                alpha = 0.25
            ) +
            geom_hline(yintercept = dat_levels, color = "grey50", alpha = 0.25) +
            theme(
                legend.position = "none",
                axis.text=element_text(size=10), 
                axis.title.y = element_blank(),
                axis.title.x = element_blank()
            ) 

    }
    

    
    prepareFeaturePlot <- function(this_feature_id) {
        feature <- filter(FEATURES, feature_id == this_feature_id)$feature
        
        dat <- dat_tech %>%
            rename(used_feature = feature) %>%
            select(date, used_feature)
        
        y_max <- max(dat$used_feature)
        y_min <- min(dat$used_feature)
        min_max <- y_max - y_min
        y_max <- y_max + min_max / 20
        y_min <- y_min - min_max / 20
        
        dat %>%
            ggplot(aes(x = date, y = used_feature)) +
            geom_line(size = 0.5, color = "red") + 
            geom_hline(yintercept = 0, size = 0.25) +
        #    scale_y_continuous(limits = c(y_min, y_max)) +
            ggtitle(feature) +
            theme(
                legend.position = "none",
                axis.text.y=element_text(size=10), 
                axis.text.x=element_blank(),
                axis.title.y = element_blank(),
                axis.title.x = element_blank(),
                plot.title = element_text(size = 10, margin = margin(b = -10))
            ) 
    }

    plotStackedPlots <- function(plot_main, plots_features) {
        chart_main <- plot_main %>% 
            ggplot_build %>% 
            ggplot_gtable;

        charts_features <- plots_features %>%
            lapply(function(this_plot) this_plot %>% ggplot_build %>% ggplot_gtable)


        chart_combined <- gtable:::rbind_gtable(chart_main, charts_features[[1]], "first")

        for (i in 2:length(plots_features)) {
            chart_combined <- gtable:::rbind_gtable(chart_combined, charts_features[[i]], "first")
        }

        panels <- chart_combined$layout$t[grep("panel", chart_combined$layout$name)]

        chart_combined$heights[panels[1]] <- unit(1.5*length(plots_features), "null")
        for (i in 1:length(plots_features)) {
            chart_combined$heights[panels[i+1]] <- unit(1, "null")
        }

        grid.newpage()
        grid.draw(chart_combined)
    }
    

    ####################################################################################################
    ### Script
    ####################################################################################################
 #   dat <- prepareData();
 #   plot_1 <- preparePlotPriceMMBB(dat);
 #   plot_2 <- preparePlotMACD(dat);
 #   plotStackedPlots_2(plot_1, plot_2);
    dat_tech <- prepareData()
    plot_main <- prepareMainPlotPriceSplineBB(dat_tech)
    plots_features <- lapply(feature_id_for_strategy, prepareFeaturePlot)
    plotStackedPlots(plot_main, plots_features)
}
G.Predict.Data.dat_predict <-
function () 
{
    ####################################################################################################
    ### Sub routines 
    ####################################################################################################
    
    readDBNoWeights <- function() {
        D.SQL("SELECT * FROM live_predict WHERE use_weights = 0") %>%
            as_tibble %>%
            mutate(
                date = as.Date(date),
                timestamp = as.POSIXct(timestamp, tz = TZ_LOCAL),
                timestamp_px = as.POSIXct(timestamp_px, tz = TZ_LOCAL),
                strategy_id = as.integer(strategy_id),
                instrument_id = as.integer(instrument_id)
            ) %>%
            select(-use_weights)
    }
    
    readDBWithWeights <- function() {
        D.SQL("SELECT * FROM live_predict WHERE use_weights = 1") %>%
            as_tibble %>%
            mutate(
                date = as.Date(date),
                timestamp = as.POSIXct(timestamp, tz = TZ_LOCAL),
                timestamp_px = as.POSIXct(timestamp_px, tz = TZ_LOCAL),
                strategy_id = as.integer(strategy_id),
                instrument_id = as.integer(instrument_id)
            ) %>%
            select(
                strategy_id, instrument_id, date, timestamp, timestamp_px,
                outcome_id, proba_up, proba_flat, proba_down, score
            ) %>%
            rename(
                outcome_id_w = outcome_id, 
                proba_up_w = proba_up, 
                proba_flat_w = proba_flat, 
                proba_down_w = proba_down
            )
    }
    
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    
    dat_no_weights <- readDBNoWeights()
    dat_weights <- readDBWithWeights()
    
    dat_no_weights %>% 
        left_join(
            dat_weights, 
            by = c("strategy_id", "instrument_id", "date", "timestamp", "timestamp_px", "score")
            )
    
}
G.Predict.Data.predict <-
function () 
{
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    REGIONS <- D.loadTableLocal("region")
    MARKETS <- D.loadTableLocal("market")
    
    PROBAS <- D.loadTableLocal("probability_threshold") %>% 
        rename(threshold = proba_threshold)
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    readDBNoWeights <- function() {
        "SELECT * FROM live_predict WHERE use_weights = 0 AND date >= '%s'" %>%
            sprintf(YESTERDAY) %>%
            D.SQL %>%
            as_tibble %>%
            mutate(
                date = as.Date(date),
                timestamp = as.POSIXct(timestamp, tz = TZ_LOCAL),
                timestamp_px = as.POSIXct(timestamp_px, tz = TZ_LOCAL),
                strategy_id = as.integer(strategy_id),
                instrument_id = as.integer(instrument_id)
            ) %>%
            select(-use_weights)
    }
    
    readDBWithWeights <- function() {
        "SELECT * FROM live_predict WHERE use_weights = 1 AND date >= '%s'" %>%
            sprintf(YESTERDAY) %>%
            D.SQL %>%
            as_tibble %>%
            mutate(
                date = as.Date(date),
                timestamp = as.POSIXct(timestamp, tz = TZ_LOCAL),
                timestamp_px = as.POSIXct(timestamp_px, tz = TZ_LOCAL),
                strategy_id = as.integer(strategy_id),
                instrument_id = as.integer(instrument_id)
            ) %>%
            select(
                strategy_id, instrument_id, date, timestamp, timestamp_px,
                outcome_id, proba_up, proba_flat, proba_down, score
            ) %>%
            rename(
                outcome_id_w = outcome_id, 
                proba_up_w = proba_up, 
                proba_flat_w = proba_flat, 
                proba_down_w = proba_down
            )
    }
    
    loadAllFiles <- function() {
        dat_no_weights <- readDBNoWeights()
        dat_weights <- readDBWithWeights()
        
        dat_no_weights %>% 
            left_join(
                dat_weights, 
                by = c("strategy_id", "instrument_id", "date", "timestamp", "timestamp_px", "score")
            )
    }
    
    formatTable <- function(dat) {
        
        fx_spot_vs_usd <- 
            "SELECT H.*
            FROM (
                SELECT asset_id, MAX(date) as date
                FROM histo_fx_close_vs_usd
                GROUP BY asset_id
            ) D 
            LEFT JOIN histo_fx_close_vs_usd H ON D.asset_id = H.asset_id AND D.date = H.date" %>%
            D.select %>%
            left_join(ASSETS, by = "asset_id") %>%
            select(code, fx)
        
        dat %>%
            left_join(
                select(INSTRUMENTS, instrument_id, asset_class, pair, ticker, market_id), 
                by = "instrument_id"
            ) %>%
            filter(
                instrument_id %in% filter(INSTRUMENTS, use_for_trading == 1)$instrument_id
            ) %>%
            left_join(MARKETS, by = "market_id") %>%
            left_join(REGIONS, by = "region_id") %>%
            left_join(PROBAS, by = "strategy_id") %>%
            left_join(TRADE_OUTCOMES, by = "outcome_id") %>%
            left_join(
                rename(TRADE_OUTCOMES, outcome_w = outcome, outcome_id_w = outcome_id),
                by = "outcome_id_w"
            ) %>%
            mutate(code = substr(pair, 1, 3)) %>% 
            left_join(fx_spot_vs_usd, by = "code") %>% 
            rename(
                price = close,
                predict = outcome,
                predict_w = outcome_w
            ) %>%
            mutate(
                models_agree = (predict == predict_w),
                buy_sell = (predict == "up") - (predict == "down"),
                target = case_when(
                    predict == "flat" ~ t_up, 
                    TRUE ~ price + buy_sell * (t_up - price)
                ),
                stop = 2 * price - target,
                pnl_tgt_ccy_1 = 1000 / fx, 
                tp_pct = t_up / price - 1, 
                notional = pnl_tgt_ccy_1 / tp_pct, 
                time_prd = format(timestamp, "%b%d %H:%M", tz = TZ_LOCAL),
                time_px = format(timestamp_px, "%b%d %H:%M", tz = TZ_LOCAL),
                proba_signal = case_when(
                    predict == "up" ~ proba_up,
                    predict == "down" ~ proba_down,
                    predict == "flat" ~ proba_flat
                ),
                proba_antisignal = case_when(
                    predict == "up" ~ proba_down,
                    predict == "down" ~ proba_up,
                    predict == "flat" ~ NUM_NA
                ),
                proba_spread = proba_signal - proba_antisignal,
                proba_signal_w = case_when(
                    predict_w == "up" ~ proba_up_w,
                    predict_w == "down" ~ proba_down_w,
                    predict_w == "flat" ~ proba_flat_w
                ),
                proba_antisignal_w = case_when(
                    predict_w == "up" ~ proba_down_w,
                    predict_w == "down" ~ proba_up_w,
                    predict_w == "flat" ~ NUM_NA
                ),
                proba_spread_w = proba_signal_w - proba_antisignal_w,
            ) %>%
            mutate(
                proba_ok = pmax(proba_spread, proba_spread_w) >= threshold,
                score_ok = score >= 10,
                signal_ok = case_when(
                    !score_ok ~ as.logical(NA),
                    score_ok ~ models_agree & proba_ok
                )
            ) %>%
            arrange(-signal_ok, -score, region_id, asset_class, strategy_id, pair) %>%
            select(
                region, asset_class,
                strategy_id, pair, ticker, date, time_prd, time_px, 
                price, target, stop, tp_pct, score,
                predict, predict_w, models_agree,
                proba_signal, proba_signal_w,
                proba_spread, proba_spread_w,
                threshold,
                proba_antisignal,
                proba_ok,
                score_ok,
                signal_ok, notional
            ) %>% 
            filter(!(strategy_id %in% c(4,5)))
    }
   
    ####################################################################################################
    ### Script
    ####################################################################################################
    loadAllFiles() %>%
        formatTable
}
G.Predict.Diagnostic.Table.modelDiagnostic <-
function (dat_predict = NULL, output = "screen") 
{
    ####################################################################################################
    ### Script Variables
    ####################################################################################################

    SCORE_MIN_FOR_DISPLAY <- 0
    
    REGIONS <- D.loadTableLocal("region")
    MARKETS <- D.loadTableLocal("market")
    
    PROBAS <- D.loadTableLocal("probability_threshold") %>% 
        rename(threshold = proba_threshold)
    
    file_name <- paste0(DIRECTORY_DATA_HD, "Model_Diagnostic/model_diagnostic_%s_latest.csv")
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    loadAllFiles <- function() {
        if (is.null(dat_predict)) {
            dat_predict <- G.Predict.Data.dat_predict()
        }
        dat_predict
    }
    
    formatTable <- function(dat) {
        
        fx_spot_vs_usd <- 
            "SELECT H.*
            FROM (
                SELECT asset_id, MAX(date) as date
                FROM histo_fx_close_vs_usd
                GROUP BY asset_id
            ) D 
            LEFT JOIN histo_fx_close_vs_usd H ON D.asset_id = H.asset_id AND D.date = H.date" %>%
            D.select %>%
            left_join(ASSETS, by = "asset_id") %>%
            select(code, fx)
        
        dat_predict %>%
            left_join(
                select(INSTRUMENTS, instrument_id, asset_class, pair, market_id), 
                by = "instrument_id"
            ) %>%
            left_join(MARKETS, by = "market_id") %>%
            left_join(REGIONS, by = "region_id") %>%
            left_join(PROBAS, by = "strategy_id") %>%
            left_join(TRADE_OUTCOMES, by = "outcome_id") %>%
            left_join(
                rename(TRADE_OUTCOMES, outcome_w = outcome, outcome_id_w = outcome_id),
                by = "outcome_id_w"
            ) %>%
            mutate(code = substr(pair, 1, 3)) %>% 
            left_join(fx_spot_vs_usd, by = "code") %>% 
            arrange(-score, region_id, asset_class, strategy_id, pair) %>%
            rename(
                price = close,
                predict = outcome,
                predict_w = outcome_w
            ) %>%
            mutate(
                models_agree = (predict == predict_w),
                buy_sell = (predict == "up") - (predict == "down"),
                target = case_when(
                    predict == "flat" ~ t_up, 
                    TRUE ~ price + buy_sell * (t_up - price)
                ),
                stop = 2 * price - target,
                pnl_tgt_ccy_1 = 1000 / fx, 
                tp_pct = t_up / price - 1, 
                Notional_for_1k_PnL = pnl_tgt_ccy_1 / tp_pct, 
                timestamp = format(timestamp, "%b%d %H:%M", tz = TZ_LOCAL),
                timestamp_px = format(timestamp_px, "%b%d %H:%M", tz = TZ_LOCAL),
                proba_signal = case_when(
                    predict == "up" ~ proba_up,
                    predict == "down" ~ proba_down,
                    predict == "flat" ~ proba_flat
                ),
                proba_antisignal = case_when(
                    predict == "up" ~ proba_down,
                    predict == "down" ~ proba_up,
                    predict == "flat" ~ NUM_NA
                ),
                proba_spread = proba_signal - proba_antisignal,
                proba_signal_w = case_when(
                    predict_w == "up" ~ proba_up_w,
                    predict_w == "down" ~ proba_down_w,
                    predict_w == "flat" ~ proba_flat_w
                ),
                proba_antisignal_w = case_when(
                    predict_w == "up" ~ proba_down_w,
                    predict_w == "down" ~ proba_up_w,
                    predict_w == "flat" ~ NUM_NA
                ),
                proba_spread_w = proba_signal_w - proba_antisignal_w,
            ) %>%
            mutate(
                proba_ok = pmax(proba_spread, proba_spread_w) >= threshold,
                score_ok = score >= 10,
                signal_ok = case_when(
                    !score_ok ~ as.logical(NA),
                    score_ok ~ models_agree & proba_ok
                )
            ) %>%
            select(
                region, asset_class,
                strategy_id, pair, date, timestamp_px, price, score,
                predict, predict_w, models_agree,
                proba_signal, proba_signal_w,
                proba_spread, proba_spread_w,
                threshold,
                proba_antisignal,
                proba_ok,
                score_ok,
                signal_ok
            )
    }
    
    prepareDataTableForDisplay <- function(dat) {
        columns_to_hide <- c("models_agree", "proba_ok", "score_ok")
        
        columns_id_to_hide <- which(colnames(dat) %in% columns_to_hide) - 1
        
        if (output == "screen") {
            dat <- dat %>%
                datatable(
                    rownames = FALSE,
                    options = list(
                        #     paging = FALSE,
                        #     info = FALSE,
                        pageLength = 100,
                        columnDefs = list(
                            list(className = 'dt-right', targets = "_all"),
                            list(targets = columns_id_to_hide, visible = FALSE)
                        )
                    )
                ) %>% 
                #formatStyle("Gross_PnL", backgroundColor = styleInterval(pnl_breaks, pnl_colors)) %>%
                formatStyle("signal_ok", backgroundColor = styleEqual(FALSE, "lightcoral")) %>%
                formatStyle("signal_ok", backgroundColor = styleEqual(TRUE, "lightseagreen")) %>%
                formatStyle(
                    c("predict", "predict_w"), "models_agree", 
                    backgroundColor = styleEqual(TRUE, "lightseagreen")
                ) %>%
                formatStyle(
                    c("predict", "predict_w"), "models_agree", 
                    backgroundColor = styleEqual(FALSE, "lightcoral")
                ) %>%
                formatStyle(
                    c("proba_spread", "proba_w_spread"), "proba_ok", 
                    backgroundColor = styleEqual(TRUE, "lightseagreen")
                ) %>%
                formatStyle(
                    c("proba_spread", "proba_w_spread"), "proba_ok", 
                    backgroundColor = styleEqual(FALSE, "lightcoral")
                ) %>%
                formatStyle(
                    "score", "score_ok", 
                    backgroundColor = styleEqual(TRUE, "lightseagreen")
                ) %>%
                formatStyle(
                    "score", "score_ok", 
                    backgroundColor = styleEqual(FALSE, "lightcoral")
                ) %>%
                formatPercentage(
                    c("proba", "proba_w", "proba_spread", 
                      "proba_w_spread", "proba_antisignal", "threshold"), 
                    2) %>%
                formatRound(c("price"), digits = 5)
            
        }
        else {
            dat <- dat %>% 
                select(-columns_to_hide)
        }
        
        dat
    }
    
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    loadAllFiles() %>%
        filter(score >= SCORE_MIN_FOR_DISPLAY) %>%
        formatTable %>%
        prepareDataTableForDisplay
}
G.Predict.Table.predict <-
function (dat_predict, output = "screen") 
{
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    SCORE_MIN_FOR_DISPLAY <- 0
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    prepareDataTableForDisplay <- function(dat) {
        
        dat <- dat %>%
            select(-date, -time_prd, -notional) %>%
            rename(
                strategy = strategy_id,
                proba = proba_signal,
                proba_w = proba_signal_w,
                spread = proba_spread,
                spread_w = proba_spread_w,
                antisignal = proba_antisignal,
                class = asset_class
            )
        
        columns_to_hide <- c("models_agree", "proba_ok", "score_ok")
        
        columns_id_to_hide <- which(colnames(dat) %in% columns_to_hide) - 1
        
        if (output == "screen") {
            dat <- dat %>%
                datatable(
                    rownames = FALSE,
                    options = list(
                        #     paging = FALSE,
                        #     info = FALSE,
                        pageLength = 100,
                        columnDefs = list(
                            list(className = 'dt-right', targets = "_all"),
                            list(targets = columns_id_to_hide, visible = FALSE)
                        )
                    )
                ) %>% 
                #formatStyle("Gross_PnL", backgroundColor = styleInterval(pnl_breaks, pnl_colors)) %>%
                formatStyle("signal_ok", backgroundColor = styleEqual(FALSE, "lightcoral")) %>%
                formatStyle("signal_ok", backgroundColor = styleEqual(TRUE, "lightseagreen")) %>%
                formatStyle(
                    c("predict", "predict_w"), "models_agree", 
                    backgroundColor = styleEqual(TRUE, "lightseagreen")
                ) %>%
                formatStyle(
                    c("predict", "predict_w"), "models_agree", 
                    backgroundColor = styleEqual(FALSE, "lightcoral")
                ) %>%
                formatStyle(
                    c("spread", "spread_w", "threshold"), "proba_ok", 
                    backgroundColor = styleEqual(TRUE, "lightseagreen")
                ) %>%
                formatStyle(
                    c("spread", "spread_w", "threshold"), "proba_ok", 
                    backgroundColor = styleEqual(FALSE, "lightcoral")
                ) %>%
                formatStyle(
                    "score", "score_ok", 
                    backgroundColor = styleEqual(TRUE, "lightseagreen")
                ) %>%
                formatStyle(
                    "score", "score_ok", 
                    backgroundColor = styleEqual(FALSE, "lightcoral")
                ) %>%
                formatPercentage(
                    c("proba", "proba_w", "spread", 
                      "spread_w", "antisignal", "threshold"), 
                    2) %>%
                formatRound(c("price", "target", "stop"), digits = 5) %>%
                formatRound(c("score"), digits = 3) %>%
                formatPercentage(c("tp_pct"), digits = 3)
            
        }
        else {
            dat <- dat %>% 
                select(-columns_to_hide)
        }
        
        dat
    }
    
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_predict %>%
        filter(score >= SCORE_MIN_FOR_DISPLAY) %>%
        prepareDataTableForDisplay
}
G.Predict.Table.predict_old <-
function (dat_predict, view_what, strat_id = NULL) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    customGreen0 = "#DeF7E9"
    customGreen = "#71CA97"
    REGIONS <- D.loadTable("static_region")
    MARKETS <- D.loadTable("static_market")
    #  LIVE_PX <- D.SQL("SELECT * FROM live_px") %>% 
    #      rename(px_timestamp = timestamp)
    FEATURES <- D.loadTable("static_feature")
    
    strat_id <- as.integer(strat_id)
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    viewByBestScore <- function() {
        dat_predict %>% 
            arrange(instrument_id, -score, strategy_id) %>% 
            ungroup %>% 
            left_join(INSTRUMENTS, by = "instrument_id") %>% 
            left_join(TRADE_OUTCOMES, by = "outcome_id") %>%
            left_join(MARKETS, by = "market") %>%
            left_join(REGIONS, by = "region_id") %>%
            filter(use_for_trading == 1) %>%
            mutate(
                buy_sell = (outcome == "up") - (outcome == "down"),
                target = case_when(
                    outcome == "flat" ~ t_up, 
                    TRUE ~ close + buy_sell * (t_up - close)
                ),
                stop = 2 * close - target,
                target_pct = t_up / close - 1
            ) %>%
            rename(
                Pair = pair,
                Ticker = ticker,
                Strategy = strategy_id,
                Price = close,
                Date = date,
                Market = market,
                Time_Predict = timestamp,
                Time_Price = timestamp_px,
                Predict = outcome,
                Target = target,
                Stop = stop,
                Target_Pct = target_pct,
                Score = score,
                Region = region
            ) %>% 
            arrange(-Score, region_id, Pair) %>% 
            select(
                Region, Strategy, Pair, Score, Predict, Price, Target, Stop, 
                Target_Pct, Date, Time_Predict, Time_Price
            ) %>%
            head(100) %>%
            gvisTable(formats = list(Target_Pct = "#.##%"))
    }
    
    viewByInstrument <- function() {
        interval_score <- c(9, 9.95, 9.999, 10)
        i_color <- round(seq(255, 40, length.out = length(interval_score) + 1), 0)
        interval_colors <- paste0("rgb(", i_color, ", 255,", i_color, ")")
        
        strategy_id_columns <- dat_predict %>%
            select(strategy_id) %>%
            unique %>%
            arrange(strategy_id) %>%
            U.vectorize %>%
            paste0(paste0("0", .)) %>%
            U.right(2)
        
        predict_columns <- paste0("predict_", strategy_id_columns)
        score_columns <- paste0("score_", strategy_id_columns)
        columns_to_hide <- which(colnames(dat_predict) %in% score_columns)
        
        dat <- dat_predict %>% 
            left_join(INSTRUMENTS, by = "instrument_id") %>% 
            left_join(TRADE_OUTCOMES, by = "outcome_id") %>%
            left_join(MARKETS, by = "market") %>%
            left_join(REGIONS, by = "region_id") %>%
            filter(use_for_trading == 1) %>%
            select(region_id, region, pair, strategy_id, outcome) %>%
            mutate(strategy_id = paste0("predict_", U.right(paste0("0", strategy_id), 2))) %>%
            spread(strategy_id, outcome) %>%
            left_join(
                dat_predict %>% 
                    left_join(INSTRUMENTS, by = "instrument_id") %>% 
                    select(pair, strategy_id, score) %>%
                    mutate(strategy_id = paste0("score_", U.right(paste0("0", strategy_id), 2))) %>%
                    spread(strategy_id, score),
                by = "pair"
            ) %>%
            arrange(region_id, pair) %>%
            select(-region_id)
        columns_to_hide <- which(colnames(dat) %in% score_columns)
        dat <- dat %>%
            datatable(
                #  rownames = FALSE,
                options = list(
                    paging = FALSE,
                    info = FALSE,
                    columnDefs = list(list(targets = columns_to_hide, visible = FALSE))
                )
            )
        for (strategy_id in strategy_id_columns) {
            dat <- dat %>%
                formatStyle(
                    paste0("predict_", strategy_id),
                    paste0("score_", strategy_id),
                    backgroundColor = styleInterval(interval_score, interval_colors)
                )
        }
        dat
    }
    
    viewByStrategy <- function() {
        nb_score <- "SELECT COUNT(*) FROM strategy_criteria WHERE strategy_id = %s" %>%
            sprintf(strat_id) %>%
            D.select %>% 
            U.vectorize;
        
        score_columns_to_keep <- paste0("score_", 1:nb_score)
        
        features_for_market_configuration <- 
            "SELECT DISTINCT feature_id
            FROM strategy_feature
            WHERE strategy_id = %s  AND market_configuration = 1" %>%
            sprintf(strat_id) %>%
            D.select %>% 
            left_join(FEATURES, by = "feature_id") %>%
            select(feature) %>% 
            U.vectorize %>%
            gsub("asset_class_id", "asset_class")
        
        dat_technicals <- T.getTechnicalsLive() %>% 
            select(c("instrument_id", features_for_market_configuration))
        
        dat_predict %>% 
            filter(strategy_id == strat_id) %>%
            left_join(INSTRUMENTS, by = "instrument_id") %>% 
            left_join(TRADE_OUTCOMES, by = "outcome_id") %>%
            left_join(MARKETS, by = "market") %>%
            left_join(REGIONS, by = "region_id") %>%
            filter(use_for_trading == 1) %>%
            rename(
                Pair = pair,
                Ticker = ticker,
                Price = close,
                Date = date,
                Predict = outcome,
                Score = score,
                Region = region
            ) %>%
            arrange(-Score, Region) %>%
            select(Region, Pair, Price, Predict, Score, all_of(score_columns_to_keep)) %>%
            gvisTable
    }
    
    viewByTrades <- function() {
        fx_spot_vs_usd <- 
            "SELECT H.*
            FROM (
                SELECT asset_id, MAX(date) as date
                FROM histo_fx_close_vs_usd
                GROUP BY asset_id
            ) D 
            LEFT JOIN histo_fx_close_vs_usd H ON D.asset_id = H.asset_id AND D.date = H.date" %>%
            D.select %>%
            left_join(ASSETS, by = "asset_id") %>%
            select(code, fx)
        
        dat_predict %>%
            left_join(INSTRUMENTS, by = "instrument_id") %>% 
            left_join(TRADE_OUTCOMES, by = "outcome_id") %>%
            left_join(
                rename(TRADE_OUTCOMES, outcome_id_w = outcome_id, outcome_w = outcome), 
                by = "outcome_id_w"
            ) %>%
            left_join(MARKETS, by = "market") %>%
            left_join(REGIONS, by = "region_id") %>%
            filter(
                score == 10,
                use_for_trading == 1
            ) %>%
            mutate(code = substr(pair, 1, 3)) %>% 
            left_join(fx_spot_vs_usd, by = "code") %>% 
            rename(
                predict = outcome, 
                predict_w = outcome_w, 
                px = close
            ) %>%
            mutate(
                models_agree = (predict == predict_w),
                buy_sell = (predict == "up") - (predict == "down"),
                target = case_when(
                    predict == "flat" ~ t_up, 
                    TRUE ~ px + buy_sell * (t_up - px)
                ),
                stop = 2 * px - target,
                pnl_tgt_ccy_1 = 1000 / fx, 
                tp_pct = t_up / px - 1, 
                Notional_for_1k_PnL = pnl_tgt_ccy_1 / tp_pct, 
                timestamp = format(timestamp, "%b%d %H:%M", tz = TZ_LOCAL),
                timestamp_px = format(timestamp_px, "%b%d %H:%M", tz = TZ_LOCAL),
                proba_signal = case_when(
                    predict == "up" ~ proba_up,
                    predict == "down" ~ proba_down,
                    predict == "flat" ~ proba_flat
                ),
                proba_antisignal = case_when(
                    predict == "up" ~ proba_down,
                    predict == "down" ~ proba_up,
                    predict == "flat" ~ NUM_NA
                ),
                proba_spread = proba_signal - proba_antisignal,
                proba_signal_w = case_when(
                    predict_w == "up" ~ proba_up_w,
                    predict_w == "down" ~ proba_down_w,
                    predict_w == "flat" ~ proba_flat_w
                ),
                proba_antisignal_w = case_when(
                    predict_w == "up" ~ proba_down_w,
                    predict_w == "down" ~ proba_up_w,
                    predict_w == "flat" ~ NUM_NA
                ),
                proba_spread_w = proba_signal_w - proba_antisignal_w,
            ) %>% 
            arrange(region_id, pair, strategy_id) %>%
            rename(
                Ticker = ticker,
                Asset_Class = asset_class,
                Pair = pair,
                Strategy = strategy_id,
                Price = px,
                Date = date,
                Market = market,
                Time_Predict = timestamp,
                Time_Price = timestamp_px,
                Predict = predict,
                Predict_W = predict_w,
                Target = target,
                Stop = stop,
                Target_Pct = tp_pct,
                Region = region,
                Buy_Sell = buy_sell,
                Proba = proba_signal,
                Proba_Spread = proba_spread,
                Proba_W = proba_signal_w,
                Proba_Spread_W = proba_spread_w,
                Models_Agree = models_agree
            ) %>%
            select(
                Region, Strategy, Pair, Ticker, Price, Predict,
                Target, Stop, Target_Pct, 
                Proba, Proba_Spread,
                Notional_for_1k_PnL, Date, Time_Predict, Time_Price,
                Predict_W,
                Models_Agree,
                Proba_W,
                Proba_Spread_W
            ) 
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    switch(
        view_what,
        "Score" = viewByBestScore(), 
        "Instrument" = viewByInstrument(), 
        "Strategy" = viewByStrategy(),
        "Trades" = viewByTrades()
    )
}
G.save <-
function() {
    All_Objects <- ls(".GlobalEnv", pattern = "G.");
    All_Objects <- All_Objects[substr(All_Objects,1,2) == "G."];
    dump(All_Objects, paste0(DIRECTORY_CODE_HD, "Code/GUI.R"));
}
G.Scenario.Data.dat_scenario <-
function () 
{
    ####################################################################################################
    ### Script
    ####################################################################################################
    bumps <- D.loadTableLocal("scenario_bump") %>% select(bump)
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    structureTableWithAllPossibleBumps <- function(i) {
        data.frame(pair_strategy[i,], bumps)
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_base <- "SELECT P.instrument_id, P.strategy_id, B.bump, T.outcome
    FROM scenario_predict P
    LEFT JOIN static_scenario_bump B ON B.bump_id = P.bump_id
    LEFT JOIN static_trade_outcome T ON T.outcome_id = P.outcome_id
    WHERE P.score = 10
    AND P.timestamp_scenario >= '2020-11-03'" %>% 
        D.SQL %>% 
        left_join(select(INSTRUMENTS, pair, instrument_id), by = "instrument_id") %>%
        select(-instrument_id) %>%
        mutate(
            predict = (outcome == "up") - (outcome == "down") 
        ) %>%
        select(-outcome)
    pair_strategy <- dat_base %>% select(pair, strategy_id) %>% unique
    dat <- 1:nrow(pair_strategy) %>%
        lapply(structureTableWithAllPossibleBumps) %>%
        bind_rows %>% 
        left_join(dat_base, by = c("pair", "strategy_id", "bump")) %>%
        #   mutate(bump = paste0("b",bump)) %>%
        spread(bump, predict) %>% 
        arrange(pair, strategy_id)# %>%
    bump_col_names <- colnames(dat)[-(1:2)]

    dat <- dat %>% 
        datatable(
            rownames = FALSE,
            options = list(info = FALSE, paging = FALSE)
            )
    
    bump_col <- "0"
    dat <- dat %>%
        formatStyle(
            bump_col,
            backgroundColor = "lavender",
            color = "lavender"
        )
    
    for (bump_col in bump_col_names) {
        dat <- dat %>%
            formatStyle(
                bump_col,
                backgroundColor = styleEqual(c(-1,0, 1), c("red", "lightgrey", "green")),
                color = styleEqual(c(-1,0, 1), c("red", "lightgrey", "green"))
            )
        
    }
    dat
}
G.Scenario.Table.instrument <-
function (fx_pair)
{
    ####################################################################################################
    ### Script
    ####################################################################################################
    "SELECT S.timestamp_scenario, S.instrument_id, S.strategy_id, B.bump, S.price, S.score, 
        T.outcome AS predict
        FROM scenario_predict S
        LEFT JOIN static_scenario_bump B ON B.bump_id = S.bump_id
        LEFT JOIN static_trade_outcome T ON T.outcome_id = S.outcome_id
        WHERE instrument_id = %s" %>% 
        sprintf(A.getInstrumentId(fx_pair)) %>% 
        D.SQL %>%
        arrange(instrument_id, strategy_id, bump, timestamp_scenario) %>%
        group_by(instrument_id, strategy_id, bump) %>%
        summarize_each(last) %>% 
        ungroup %>%
        select(-timestamp_scenario) %>%
        left_join(INSTRUMENTS, by = "instrument_id") %>%
        select(pair, strategy_id, bump, price, score, predict) %>% 
        mutate(
         #   strategy_id = paste0("strategy_", U.right(paste0("0",strategy_id), 2)),
            predict = case_when((score < 10) ~ CHAR_NA, TRUE ~ predict),
        ) %>%
        select(-score) %>%
        spread(strategy_id, predict) %>% 
        select(which(U.vectorize(summarise_each(., function(this_col) !all(is.na(this_col)))))) %>%
        gvisTable(formats = list(price = U.formatPrice(.$price[ceiling(nrow(.) / 2)])))
}
G.Trades.Table.correlations <-
function(dat_predict) {
    ####################################################################################################
    ### Script description:
    ### Returns trade-adjusted correlation matrix for signals, labeled by pair
    ####################################################################################################

    dat_signals <- dat_predict %>%
        left_join(select(INSTRUMENTS, pair, instrument_id), by = "pair") %>%
        mutate(buy_sell = (predict == "up") - (predict == "down")) %>%
        filter(signal_ok)

    if (nrow(dat_signals) == 0) {
        return(data.frame(Message = "No signals"))
    }

    # Get unique instruments and their directions
    dat_unique <- dat_signals %>%
        distinct(instrument_id, pair, buy_sell)

    if (nrow(dat_unique) < 2) {
        return(data.frame(Message = "Need at least 2 instruments for correlation matrix"))
    }

    # Compute asset correlation matrix
    cor_matrix <- T.calcHistoricalCorrelationsMatrix(
        instrument_ids = dat_unique$instrument_id,
        shrinkage = 0
    )

    # Build trade correlation matrix (direction-adjusted)
    n <- nrow(dat_unique)
    trade_cor <- matrix(0, nrow = n, ncol = n)

    for (i in 1:n) {
        for (j in 1:n) {
            inst_i <- dat_unique$instrument_id[i]
            inst_j <- dat_unique$instrument_id[j]
            dir_i <- dat_unique$buy_sell[i]
            dir_j <- dat_unique$buy_sell[j]

            idx_i <- which(as.integer(colnames(cor_matrix)) == inst_i)
            idx_j <- which(as.integer(colnames(cor_matrix)) == inst_j)

            if (length(idx_i) == 1 && length(idx_j) == 1) {
                trade_cor[i, j] <- dir_i * dir_j * cor_matrix[idx_i, idx_j]
            }
        }
    }

    # Format as data frame with pair names
    trade_cor_df <- as.data.frame(round(trade_cor, 2))
    colnames(trade_cor_df) <- dat_unique$pair
    trade_cor_df <- cbind(Pair = dat_unique$pair, trade_cor_df)
    trade_cor_df
}
G.Trades.Table.predict <-
function (dat_predict, aum_total = 1e6, risk_per_bet_pct = 0.5, max_daily_risk_pct = 5, correlation_adjustment = 0)
{
    ####################################################################################################
    ### Script Variables
    ####################################################################################################

    tradable_instruments <- c(
        'A50CNY', 'ASXAUD', 'AUDCAD', 
        'AUDCHF', 'AUDJPY', 'AUDNZD', 'AUDUSD', 
        'CADJPY', 'CHFJPY', 'CHFSEK', 'DAXEUR', 
        'DJIUSD', 'EURAUD', 'EURCAD', 'EURCHF', 'EURCZK', 'EURGBP', 'EURHUF', 
        'EURJPY', 'EURNOK', 'EURNZD', 'EURPLN', 'EURSEK', 'EURUSD', 
        'FTSGBP', 'GBPAUD', 'GBPCAD', 'GBPCHF', 
        'GBPJPY', 'GBPNZD', 'GBPSEK', 'GBPUSD', 'HSIHKD', 'IBXEUR',
        'KSPKRW', 'MIBEUR', 
        'NDXUSD', 'NKYJPY', 'NZDCAD', 'NZDCHF', 'NZDJPY', 'NZDUSD', 
        'PX1EUR', 'RUTUSD', 'SEKJPY', 'SMICHF', 'SPXUSD', 'SSECNY', 
        'STXEUR', 'TPXJPY', 'TSXCAD',
        'USDBRL', 'USDCAD', 'USDCHF', 'USDCLP', 
        'USDINR', 'USDJPY', 'USDMXN', 'USDNOK', 'USDSEK', 
        'USDSGD', 'USDZAR', 'XAGUSD', 'XAUUSD', 
        'CHFNOK', 'GBPNOK', 'GBPPLN', 'NOKSEK', 
        'EEMUSD')
    
    dat_signals <- dat_predict %>%
        left_join(select(INSTRUMENTS, pair, asset, instrument_id), by = "pair") %>%
        filter(pair %in% tradable_instruments) %>%
        mutate(
            asset = case_when(
                asset_class %in% c("index", "bond") ~ asset,
                TRUE ~ pair
            ),
            buy_sell = (predict == "up") - (predict == "down")
        ) %>%
        filter(signal_ok)

    if (nrow(dat_signals) == 0) {
        return(
            dat_signals %>%
                select(
                    region, asset_class, strategy_id, ticker, asset,
                    price, predict, target, stop, tp_pct, notional
                ) %>%
                mutate(sized_notional = numeric(0), weight_pct = numeric(0), n_eff = numeric(0)) %>%
                rename(
                    Region = region, Asset_Class = asset_class, Strategy = strategy_id,
                    Ticker = ticker, Name = asset, Price = price, Predict = predict,
                    Target = target, Stop = stop, Target_Pct = tp_pct,
                    Notional_for_1k_PnL = notional, Sized_Notional = sized_notional,
                    Weight_Pct = weight_pct, N_Eff = n_eff
                ) %>%
                rhandsontable(rowHeaders = NULL)
        )
    }

    ####################################################################################################
    ### Compute correlation matrix and portfolio sizing
    ####################################################################################################

    instrument_ids <- unique(dat_signals$instrument_id)
    cor_matrix <- T.calcHistoricalCorrelationsMatrix(instrument_ids = instrument_ids, shrinkage = 0)

    dat_sized <- V.portfolioSizing(
        dat_signals %>% select(instrument_id, buy_sell, notional),
        cor_matrix = cor_matrix,
        risk_per_bet_pct = risk_per_bet_pct,
        max_daily_risk_pct = max_daily_risk_pct,
        aum_total = aum_total,
        correlation_adjustment = correlation_adjustment
    )

    ####################################################################################################
    ### Format output table
    ####################################################################################################

    n_effective <- dat_sized$n_effective[1]

    dat_signals %>%
        mutate(
            weight = dat_sized$weight,
            sized_notional = dat_sized$sized_notional,
            weight_pct = weight * 100,
            n_eff = n_effective
        ) %>%
        select(
            region, asset_class, strategy_id, ticker, asset,
            price, predict, target, stop, tp_pct, notional, weight_pct, sized_notional, n_eff
        ) %>%
        rename(
            Region = region,
            Asset_Class = asset_class,
            Strategy = strategy_id,
            Ticker = ticker,
            Name = asset,
            Price = price,
            Predict = predict,
            Target = target,
            Stop = stop,
            Target_Pct = tp_pct,
            Notional_for_1k_PnL = notional,
            Weight_Pct = weight_pct,
            Sized_Notional = sized_notional,
            N_Eff = n_eff
        ) %>%
        rhandsontable(rowHeaders = NULL) %>%
        hot_col(c("Price", "Target", "Stop", "Notional_for_1k_PnL", "Sized_Notional"), format = "0,000.000000") %>%
        hot_col("Target_Pct", format = "0.000%") %>%
        hot_col("Weight_Pct", format = "0.00") %>%
        hot_col("N_Eff", format = "0.00")
}
