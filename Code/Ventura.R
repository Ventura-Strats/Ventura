V.backtestModel <-
function (strat_id = 1, start_date = "1995-01-01", end_date = TO_DAY-7,
          use_weights_for_recent_data = FALSE, file_import_data_already_done = NULL
) 
{
    ####################################################################################################
    ### Script Parameters
    ####################################################################################################
    keep_features <- 120
    model_n_trees <- 1000
    use_best_features_at_that_time <- TRUE
    
    MIN_PROBA_SIGNAL <- 0 # 0.4
    MIN_PROBA_SIGNAL_PLUS_FLAT <- 0 # 0.75
    
    backtest_name <- paste0("strat_", strat_id)
    if (use_weights_for_recent_data) backtest_name <- paste0(backtest_name, "_weights")
    
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    file_name <- "%sBacktestings/backtest_%s_%s.csv" %>%
        sprintf(DIRECTORY_DATA_HD, backtest_name, U.dateTimeFormatForExport())
    file_name_r <- gsub(".csv", ".RData", file_name)
    
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
    
    FEATURES_TO_IGNORE <- ""
    
    INSTRUMENT_PAIR <- select(INSTRUMENTS, instrument_id, pair)
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    prepareDatesList <- function(dat_tech) {
        dat_tech %>%
            E.genericScoreFunction(strat_id) %>%
            filter(
                score_high_enough,
                date >= as.Date(start_date),
                date <= as.Date(end_date)
            ) %>%
            select(date) %>%
            unique %>%
            arrange(date) %>%
            .$date %>%
            as.character
    }
    
    prepareDatesListFeaturesSelection <- function(dates_list) {
        (1 + c(0,  which(diff(year(as.Date(dates_list))) != 0))) %>% 
            unique %>% 
            dates_list[.] %>%
            as.character %>%
            U.debug("dates_features_selection")
    }
    
    calcFeaturesToIgnore_Try <- function(features_list) {
        U.printBanner("First calibration with all features", FALSE)
        dat_trade_model <- E.trainModel(
            dat_tech, strat_id, 
            n_trees = model_n_trees,
            weights_with_time = use_weights_for_recent_data,
            use_limited_training_set = TRUE
        )
        U.printBanner("Variable importance", FALSE)
        var_imp <- varimp(dat_trade_model$model)
        var_imp <- var_imp[order(var_imp)]
        features_list <- head(names(var_imp), -keep_features)
        U.printBanner("Removing these features:", FALSE)
        print(features_list)
        rm(dat_trade_model)
        gc()
        features_list
    }
    
    findFeaturesToIgnore <- function(this_date, features_list) {
        if (
            use_best_features_at_that_time & 
            (this_date %in% dates_list_features_selection)
        ) {
            features_list <- U.try(calcFeaturesToIgnore_Try, features_list)(features_list)
        }
        features_list
    }
    
    printDateResult <- function(dat) {
        print(
            dat %>%
                left_join(INSTRUMENT_PAIR, by = "instrument_id") %>%
                #    filter(
                #        predict %in% c("up", "down"),
                #        proba_signal >= MIN_PROBA_SIGNAL,
                #        proba_signal_plus_flat  >= MIN_PROBA_SIGNAL_PLUS_FLAT
                #    ) %>%
                mutate(
                    buy_sell = (predict == "up") - (predict == "down"),
                    pnl = round(100 * buy_sell * (px_exit / close - 1), 1),
                    pnl_gross = round(buy_sell * (px_exit - close) / (t_up - close), 2)
                ) %>%
                rename(px_entry = close) %>%
                select(
                    pair, predict, starts_with("proba"), 
                    tgt, px_entry, t_up, t_dn, px_exit, pnl, pnl_gross
                ) %>%
                data.frame
        )
        dat
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_tech <- T.getTechnicals("train", strat_id, TRUE, TRUE) %>% 
        filter(
            date <= end_date,
            asset_class != "bond"
        ) 
    
    dates_list <- prepareDatesList(dat_tech) %>% U.debug("dates")
    
    res <- NULL
    if (!is.null(file_import_data_already_done)) {
        res <- "%sBacktestings/%s" %>%
            sprintf(DIRECTORY_DATA_HD, file_import_data_already_done) %>% 
            U.read.csv
        dates_list <- dates_list[dates_list > max(res$date)] %>% U.debug("dates using data already done")
    }
    
    dates_list_features_selection <- prepareDatesListFeaturesSelection(dates_list) 
    
    for (this_date in dates_list) {
        U.printBanner(this_date, TRUE)
        MAX_TRAIN_DATE <<- as.Date(this_date)
        
        FEATURES_TO_IGNORE <- findFeaturesToIgnore(this_date, FEATURES_TO_IGNORE)
        
        dat_trade_model <- dat_tech %>%
            E.trainModel(
                strat_id, 
                export_all_data = TRUE,
                n_trees = model_n_trees, 
                features_to_ignore = FEATURES_TO_IGNORE,
                weights_with_time = use_weights_for_recent_data,
                use_limited_training_set = TRUE
            )
        
        this_res <- dat_trade_model$test %>%
            filter(date == as.Date(this_date)) %>%
            E.modelPredict(dat_trade_model, TRUE) %>%
            U.data2Tibble %>%
            mutate(
                proba_signal = (predict == "up") * predict_proba_up +
                    (predict == "down") * predict_proba_down + 
                    (predict == "flat") * predict_proba_flat,
                proba_signal_plus_flat = (predict != "flat") * proba_signal + predict_proba_flat
            ) %>%
            printDateResult
        
        res <- rbind(res, this_res)
        U.write.csv(res, file_name)
        rm(dat_trade_model)
        gc()
    }
    
    res <- U.dataFrame(res)
    res$buy_sell <- 0
    res$buy_sell[which(res$predict == "up")] <- 1
    res$buy_sell[which(res$predict == "down")] <- -1
    
    res <- res %>% 
        left_join(INSTRUMENT_PAIR, by = "instrument_id") %>%
        mutate(
            rtn = px_exit / close - 1,
            pnl = rtn * buy_sell
        ) %>% 
        U.data2Tibble
    
    dat_1 <- res %>% 
        filter(
            buy_sell != 0, 
            !is.na(buy_sell), 
            tgt != "unknown", 
            !is.na(pnl)
        ) %>%
        mutate(
            year = year(date),
            win = (pnl >= 0) + 0,
            pnl_positive = pnl * win,
            pnl_negative = pnl * (1-win),
            gross_pnl = buy_sell * (px_exit - close) / (t_up - close)
        ) %>%
        group_by(year) %>%
        summarize(
            N = n(),
            n_days_with_trade = length(unique(date)),
            pnl_per_trade = round(100*mean(pnl), 2),
            gross_pnl = round(mean(gross_pnl), 2),
            stdev = round(100*sd(pnl), 2),
            duration_days = round(mean(duration), 2),
            win_ratio_N = round(100 * sum(win) / N, 2),
            win_ratio_pnl = round(-mean(pnl_positive) / mean(pnl_negative), 2),
            pnl_annualized = pnl_per_trade * 260 / duration_days
        ) %>% 
        ungroup
    
    dat_2 <- res %>% 
        filter(
            buy_sell != 0, 
            !is.na(buy_sell), 
            tgt != "unknown", 
            !is.na(pnl)
        ) %>%
        mutate(
            year = "Total",
            win = (pnl >= 0) + 0,
            pnl_positive = pnl * win,
            pnl_negative = pnl * (1-win),
            gross_pnl = buy_sell * (px_exit - close) / (t_up - close)
        ) %>% 
        group_by(year) %>%
        summarize(
            N = n(),
            n_days_with_trade = length(unique(date)),
            pnl_per_trade = round(100*mean(pnl), 2),
            gross_pnl = round(mean(gross_pnl), 2),
            stdev = round(100*sd(pnl), 2),
            duration_days = round(mean(duration), 2),
            win_ratio_N = round(100 * sum(win) / N, 2),
            win_ratio_pnl = round(-mean(pnl_positive) / mean(pnl_negative), 2),
            pnl_annualized = pnl_per_trade * 260 / duration_days
        ) %>% 
        ungroup
    
    colnames(dat_2) <- colnames(dat_1)
    
    dat_1 <- rbind(dat_1, dat_2)
    
    dat_1$win_ratio_pnl[which(is.nan(dat_1$win_ratio_pnl))] <- NA
    dat_1$win_ratio_pnl[which(is.infinite(dat_1$win_ratio_pnl))] <- NA
    dat_1$win_ratio_N[which(is.nan(dat_1$win_ratio_pnl))] <- NA
    dat_1$win_ratio_N[which(is.infinite(dat_1$win_ratio_pnl))] <- NA
    
    dat_3 <- res %>% 
        group_by(predict, tgt) %>%
        summarize(N = n()) %>% 
        ungroup %>%
        spread(tgt, N, fill = 0)
    
    
    dat_4 <- res %>% 
        filter(
            buy_sell != 0, 
            !is.na(buy_sell), 
            tgt != "unknown", 
            !is.na(pnl)
        ) %>%
        mutate(
            year = paste0("Y",year(date))
        ) %>%
        group_by(pair, year) %>%
        summarize(N = n()) %>%
        ungroup %>%
        spread(year, N, fill = 0) %>%
        mutate(Total = reduce(select(., -pair), `+`)) %>%
        arrange(-Total)
    
    dat_5 <- res %>% 
        filter(
            buy_sell != 0, 
            !is.na(buy_sell), 
            tgt != "unknown", 
            !is.na(pnl)
        ) %>%
        mutate(gross_pnl = buy_sell * (px_exit - close) / (t_up - close)) %>% 
        group_by(date) %>%
        summarize(
            N = n(), 
            gross_pnl = mean(gross_pnl), 
            nav_mult = 1 + 0.01 * gross_pnl * pmin(N, 10)
        ) %>% 
        ungroup %>%
        mutate(nav =  100*cumprod(nav_mult)-1) %>% 
        mutate(year = year(date)) %>%
        mutate(nav_prev = lag(nav, 1, default = 100)) %>%
        group_by(year) %>%
        summarize(
            N = sum(N),
            nav = last(nav),
            rtn = round(100 * (nav / first(nav_prev) - 1), 2)
        ) %>% 
        data.frame %>%
        mutate(nav = round(nav, 2))
    
    
    U.printBanner("Performance summary", FALSE)
    print(dat_1 %>% data.frame)
    print(dat_3)
    print(dat_4)
    print(dat_5)
    
    res <- list(
        res = res, 
        stats_1 = dat_1,
        stats_2 = dat_3,
        stats_3 = dat_4,
        stats_4 = dat_5
    )
    save(res, file = file_name_r)
    
    MAX_TRAIN_DATE <<- YESTERDAY
    
    res
}
V.backtestProbabilityAnalysis <-
function (strat_id_list,
          THRESHOLD_PROBA_SIGNAL = 0.425,
          THRESHOLD_PROBA_DIFF = 0.06,
          THRESHOLD_PROBA_ANTISIGNAL = 0.25, 
          dat_input = NULL) 
{
    ####################################################################################################
    ### Script parameters
    ####################################################################################################
    keep_only_tradable <- TRUE
    compare_with_weighted_calibration <- TRUE
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    path_backtest <- paste0(DIRECTORY_DATA_HD, "/Backtestings/")
    file_name_strat <- paste0(path_backtest, "full_backtest_strat_%s.csv")
    file_name_strat_w <- gsub(".csv", "_weights.csv", file_name_strat)
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    readStrategyData <- function(strat_id) {
        dat <- dat_input
        if (is.null(dat_input)) {
            dat_w <- file_name_strat_w %>%
                sprintf(strat_id) %>% 
                U.read.csv %>%
                mutate(
                    proba_antisignal_w = 1 - proba_signal_plus_flat,
                    proba_diff_w = proba_signal - proba_antisignal_w
                ) %>%
                rename(
                    predict_w = predict,
                    proba_signal_w = proba_signal
                ) %>%
                select(date, instrument_id, predict_w, proba_signal_w, proba_diff_w, proba_antisignal_w)
            
            dat <- file_name_strat %>%
                sprintf(strat_id) %>%
                U.read.csv %>%
                left_join(dat_w, by = c("date", "instrument_id")) %>%
                filter(predict %in% c("up", "down")) %>%
                mutate(
                    strategy_id = strat_id,
                    buy_sell = (predict == "up") - (predict == "down"),
                    gross_pnl = buy_sell * (px_exit - close) / (t_up - close),
                    proba_antisignal = 1 - proba_signal_plus_flat,
                    proba_diff = proba_signal - proba_antisignal
                ) 
        }
        
        dat %>%
            filter(
                strategy_id == strat_id,
                predict %in% c("up", "down"),
                predict_w %in% c("up", "down")
            ) %>%
            mutate(year = year(date)) %>%
            select(
                strategy_id, date, year, instrument_id, 
                predict, predict_w, 
                buy_sell, tgt,
                proba_signal, proba_diff, proba_antisignal,
                proba_signal_w, proba_diff_w, proba_antisignal_w,
                gross_pnl
            )
    }
    
    filterProbabilities <- function(dat) {
        dat %>% 
            left_join(select(INSTRUMENTS, instrument_id, use_for_trading_ib), by = "instrument_id") %>%
            filter(
                (use_for_trading_ib == 1) | !keep_only_tradable,
                (predict == predict_w) | !compare_with_weighted_calibration,
                (pmax(proba_signal, proba_signal_w) >= THRESHOLD_PROBA_SIGNAL) |
                    (pmax(proba_diff, proba_diff_w) >= THRESHOLD_PROBA_DIFF) | 
                    (pmin(proba_antisignal, proba_antisignal_w) <= THRESHOLD_PROBA_ANTISIGNAL)
            ) %>% 
            select(-use_for_trading_ib)
    }
    
    calcQuickPnL <- function(dat) {
        
        start_date <- as.Date(paste0(min(dat$year), "-01-01"))
        end_date <- as.Date(paste0(max(dat$year), "-12-31"))
        n_years <- U.yearFrac(start_date, end_date)
        
        date_list <- data.frame(date = seq(start_date, end_date, 1)) %>% U.data2Tibble
        
        dat_0 <- dat %>% 
            group_by(date) %>%
            summarize(
                N = n(), 
                gross_pnl = mean(gross_pnl), 
                nav_mult = 1 + 0.01 * gross_pnl * pmin(N, 10)
            ) %>%
            ungroup
        
        dat_0 <- date_list %>% 
            left_join(dat_0, by = "date") %>%
            U.dfReplaceNAColumnsWithZero("N") 
        
        dat_0$nav_mult[which(is.na(dat_0$nav_mult))] <- 1
        
        dat_0 <- dat_0 %>%
            mutate(
                nav = 100*cumprod(nav_mult)-1,
                year = year(date),
                nav_prev = lag(nav, 1, default = 100),
                max_so_far = cummax(nav),
                drawdown = -(nav / max_so_far - 1),
                max_drawdown = cummax(drawdown),
                quinquennat = 5 * round((year - 2.49) / 5, 0),
                rtn_day = log(nav/nav_prev)
            ) 
        
        dat_1 <- dat_0 %>%
            group_by(quinquennat) %>%
            summarize(
                stdev = sd(gross_pnl, na.rm=TRUE),
                gross_pnl = mean(gross_pnl, na.rm=TRUE),
                nb_years = U.yearFrac(first(date), last(date)),
                N_per_year = sum(N) / nb_years ,
                cagr = 100*((last(nav) / first(nav_prev))^(1/nb_years) - 1),
                max_drawdown = 100 * last(max_drawdown),
                vol = sqrt(365) * sd(rtn_day)
            ) %>% 
            ungroup %>%
            select(quinquennat, N_per_year, gross_pnl, stdev, cagr, vol, max_drawdown)
        
        dat_2 <- dat_0 %>%
            mutate(quinquennat = "Total") %>%
            group_by(quinquennat) %>%
            summarize(
                stdev = sd(gross_pnl, na.rm=TRUE),
                gross_pnl = mean(gross_pnl, na.rm=TRUE),
                nb_years = U.yearFrac(first(date), last(date)),
                N_per_year = sum(N) / nb_years ,
                cagr = 100*((last(nav) / first(nav_prev))^(1/nb_years) - 1),
                max_drawdown = 100 * last(max_drawdown),
                vol = sqrt(365) * sd(rtn_day)
            ) %>% 
            ungroup %>%
            select(quinquennat, N_per_year, gross_pnl, stdev, cagr, vol, max_drawdown)
        rbind(dat_1, dat_2)
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    strat_id_list %>%
        lapply(readStrategyData) %>%
        bind_rows %>%
        filterProbabilities %>%
        calcQuickPnL
    
}
V.backtestProbabilityAnalysisPlotCAGR <-
function (strat_id_list, dat_input, x1 = 0.425, x2 = 0.25) 
{
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    n0 <- V.backtestProbabilityAnalysis(strat_id_list, 1, 0, 0, dat_input) %>%
        filter(quinquennat == "Total") %>% 
        .$N_per_year
    print(n0)
    n1 <- c(0.95, 0.9, 0.85, 0.8, 0.75) * n0

    dat <- seq(0, 0.25, 0.001) %>%
        lapply(
            function(x) 
                V.backtestProbabilityAnalysis(strat_id_list, x1, x, x2, dat_input) %>%
                mutate(proba = x)
            ) %>%
        bind_rows %>%
        filter(quinquennat == "Total")
    

    pos_n1 <- U.sapply(n1, function(x) which(dat$N_per_year <= x)[1])
    p1 <- dat$proba[pos_n1]
    dat %>%
        ggplot(aes(x=proba, y = cagr, colour = N_per_year)) + 
        geom_line() + 
        geom_vline(xintercept = p1, colour = "red", linetype="dotted") + 
        theme(legend.position = "bottom") + 
        scale_y_continuous(limits = c(120, 150))
    
}
V.backtestProbabilityDensity <-
function (strat_id_list, PROBA_STEP = 0.025, dat_input = NULL, this_quinquennat = "Total", 
          up_down = c("up", "down"), date_min = "2000-01-01") 
{
    ####################################################################################################
    ### Script parameters
    ####################################################################################################
    keep_only_tradable <- TRUE
    compare_with_weighted_calibration <- TRUE
    PROBA_DIFF <- seq(0, 0.3 - PROBA_STEP, PROBA_STEP)
    
    limited_training_set <- c(
        'A50CNY', 'AEXEUR', 'ASXAUD', 'ATXEUR', 'AUDCAD', 
        'AUDCHF', 'AUDJPY', 'AUDNZD', 'AUDUSD', 'BELEUR', 'BMVMXN', 'BVPBRL', 
        'CADJPY', 'CHFJPY', 'CHFSEK', 'CSICNY', 'DAXEUR', 
        'DJIUSD', 'DJNNZD', 'EURAUD', 'EURCAD', 'EURCHF', 'EURCZK', 'EURDKK', 'EURGBP', 'EURHUF', 
        'EURJPY', 'EURNOK', 'EURNZD', 'EURPLN', 'EURSEK', 'EURUSD', 
        'FTSGBP', 'GBPAUD', 'GBPCAD', 'GBPCHF', 
        'GBPJPY', 'GBPNZD', 'GBPSEK', 'GBPUSD', 'HSIHKD', 'IBXEUR',
        'JPYKRW', 'KSPKRW', 'MIBEUR', 'NDQUSD', 
        'NDXUSD', 'NKYJPY', 'NZDCAD', 'NZDCHF', 'NZDJPY', 'NZDUSD', 'OMHEUR', 
        'OMXSEK', 'PSIEUR', 'PX1EUR', 'RUTUSD', 'SEKJPY', 'SMICHF', 'SPXUSD', 'SSECNY', 
        'STISGD', 'STXEUR', 'SZSCNY', 'TAIILS', 'TPXJPY', 'TSXCAD',
        'USDBRL', 'USDCAD', 'USDCHF', 'USDCLP', 'USDIDR', 'USDILS', 
        'USDINR', 'USDJPY', 'USDKRW', 'USDMXN', 'USDNOK', 'USDPHP', 'USDRUB', 'USDSEK', 
        'USDSGD', 'USDTRY', 'USDTWD', 'USDZAR', 'XAGUSD', 'XAUUSD', 'XPDUSD', 
        'XPTUSD', 'CHFNOK', 'GBPNOK', 'GBPPLN', 'EURRUB', 'NOKSEK', 
        'EEMUSD')
    
    limited_instrument_set <- A.getInstrumentId(limited_training_set)
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    path_backtest <- paste0(DIRECTORY_DATA_HD, "/Backtestings/")
    file_name_strat <- paste0(path_backtest, "limited_backtest_strat_%s.csv")
    file_name_strat_w <- gsub(".csv", "_weights.csv", file_name_strat)
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    readStrategyData <- function(strat_id) {
        dat <- dat_input
        if (is.null(dat_input)) {
            dat_w <- file_name_strat_w %>%
                sprintf(strat_id) %>% 
                U.read.csv %>%
                mutate(
                    proba_antisignal_w = 1 - proba_signal_plus_flat,
                    proba_diff_w = proba_signal - proba_antisignal_w
                ) %>%
                rename(
                    predict_w = predict,
                    proba_signal_w = proba_signal
                ) %>%
                select(date, instrument_id, predict_w, proba_signal_w, proba_diff_w, proba_antisignal_w)
            
            dat <- file_name_strat %>%
                sprintf(strat_id) %>%
                U.read.csv %>%
                left_join(dat_w, by = c("date", "instrument_id")) %>%
                filter(predict %in% c("up", "down")) %>%
                mutate(
                    strategy_id = strat_id,
                    buy_sell = (predict == "up") - (predict == "down"),
                    gross_pnl = buy_sell * (px_exit - close) / (t_up - close),
                    proba_antisignal = 1 - proba_signal_plus_flat,
                    proba_diff = proba_signal - proba_antisignal
                ) 
        }
        
        dat %>%
            filter(
                instrument_id %in% limited_instrument_set,
                date >= date_min,
                strategy_id == strat_id,
                predict %in% up_down,
                predict_w %in% up_down,
            ) %>%
            mutate(year = year(date)) %>%
            select(
                strategy_id, date, year, instrument_id, 
                predict, predict_w, 
                buy_sell, tgt,
                proba_signal, proba_diff, proba_antisignal,
                proba_signal_w, proba_diff_w, proba_antisignal_w,
                gross_pnl
            )
    }
    
    filterProbabilities <- function(p, dat) {
        dat %>% 
            left_join(select(INSTRUMENTS, instrument_id, use_for_trading_ib), by = "instrument_id") %>%
            filter(
                (use_for_trading_ib == 1) | !keep_only_tradable,
                (predict == predict_w) | !compare_with_weighted_calibration,
                (pmax(proba_diff, proba_diff_w) >= p),
                (pmax(proba_diff, proba_diff_w) < p + PROBA_STEP),
            ) %>% 
            select(-use_for_trading_ib)
    }
    
    calcPnLDensityForProba <- function(p, dat) {
        dat <- filterProbabilities(p, dat)
        start_date <- as.Date(paste0(min(dat$year), "-01-01"))
        end_date <- as.Date(paste0(max(dat$year), "-12-31"))
        n_years <- U.yearFrac(start_date, end_date)
        
        date_list <- data.frame(date = seq(start_date, end_date, 1)) %>% U.data2Tibble
        
        dat_0 <- dat %>% 
            group_by(date) %>%
            summarize(
                N = n(), 
                gross_pnl = mean(gross_pnl), 
                nav_mult = 1 + 0.01 * gross_pnl * pmin(N, 10)
            ) %>%
            ungroup
        
        dat_0 <- date_list %>% 
            left_join(dat_0, by = "date") %>%
            U.dfReplaceNAColumnsWithZero("N") 
        
        dat_0$nav_mult[which(is.na(dat_0$nav_mult))] <- 1
        
        dat_0 <- dat_0 %>%
            mutate(
                nav = 100*cumprod(nav_mult)-1,
                year = year(date),
                nav_prev = lag(nav, 1, default = 100),
                max_so_far = cummax(nav),
                drawdown = -(nav / max_so_far - 1),
                max_drawdown = cummax(drawdown),
                quinquennat = 5 * round((year - 2.49) / 5, 0),
                rtn_day = log(nav/nav_prev)
            ) 
        
        dat_1 <- dat_0 %>%
            group_by(quinquennat) %>%
            summarize(
                proba = p,
                stdev = sd(gross_pnl, na.rm=TRUE),
                gross_pnl = mean(gross_pnl, na.rm=TRUE),
                nb_years = U.yearFrac(first(date), last(date)),
                N_per_year = sum(N) / nb_years ,
                cagr = 100*((last(nav) / first(nav_prev))^(1/nb_years) - 1),
                max_drawdown = 100 * last(max_drawdown),
                vol = sqrt(365) * sd(rtn_day)
            ) %>% 
            ungroup %>%
            select(proba, quinquennat, N_per_year, gross_pnl, stdev, cagr, vol, max_drawdown)
        
        dat_2 <- dat_0 %>%
            mutate(quinquennat = "Total") %>%
            group_by(quinquennat) %>%
            summarize(
                proba = p,
                stdev = sd(gross_pnl, na.rm=TRUE),
                gross_pnl = mean(gross_pnl, na.rm=TRUE),
                nb_years = U.yearFrac(first(date), last(date)),
                N_per_year = sum(N) / nb_years ,
                cagr = 100*((last(nav) / first(nav_prev))^(1/nb_years) - 1),
                max_drawdown = 100 * last(max_drawdown),
                vol = sqrt(365) * sd(rtn_day)
            ) %>% 
            ungroup %>%
            select(proba, quinquennat, N_per_year, gross_pnl, stdev, cagr, vol, max_drawdown)
        rbind(dat_1, dat_2)
    }
    
    calcPnLDensity <- function(dat) {
        PROBA_DIFF %>% 
            lapply(function(p) calcPnLDensityForProba(p, dat)) %>% 
            bind_rows
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    df <- strat_id_list %>%
        lapply(readStrategyData) %>%
        bind_rows %>%
        calcPnLDensity
    
    print(
        df %>%
            filter(quinquennat == this_quinquennat) %>% 
            mutate(proba = proba + 0.5 * PROBA_STEP) %>%
            select(-quinquennat) %>% 
            ggplot(aes(x=proba, y = gross_pnl)) + 
            geom_col(aes(fill= N_per_year)) + 
            theme(legend.position = "bottom")
    )
    df
}
V.buildTradesLifeCycle <-
function () 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    path_base <- paste0(DIRECTORY_DATA_HD, "Git/Ventura/trades_")
    path_trd_new <- paste0(path_base, "new")
    path_trd_ledger <- paste0(DIRECTORY_CODE_HD, "Code")
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    retrieveAllNewTrades <- function() {
        path_trd_new %>%
            list.files(pattern="csv", full.names=TRUE, recursive=TRUE) %>%
            lapply(U.read.csv) %>%
            bind_rows %>%
            mutate(
                buy_sell = case_when(
                    predict == "up" ~ 1,
                    predict == "down" ~ -1,
                    predict == "flat" ~ 0
                ),
                target = price_entry * (1 + buy_sell*tp_pct),
                stop = price_entry * (1 - buy_sell*tp_pct)
            )
    }
    
    readFullLedger <- function() {
        dat <- path_trd_ledger %>%
            paste0("/trades_ventura.csv") %>%
            U.read.csv
        rows_remove <- which(
            is.na(dat$price_status) | 
                is.na(dat$trade_status) | 
                (dat$trade_status == "live")
        );
        dat <- dat[-rows_remove,]
        rownames(dat) <- NULL
        dat
    }
    
    matchTrades <- function(dat_new, dat_ledger) {
        dat_matched <- dat_new %>% 
            semi_join(dat_ledger, by = "trade_id")
        dat_ledger %>% 
            filter(trade_id %in% dat_matched$trade_id)
    }
    
    unmatchedTrades <- function(dat_new, dat_ledger) {
        dat_new %>% 
            anti_join(dat_ledger, by = "trade_id") %>%
            arrange(trade_id)
    }
    
    getHistoPxVsUSD <- function(date_from) {
        "SELECT A.code AS ccy, F.date, F.fx
        FROM histo_fx_close_vs_usd F
        LEFT JOIN static_asset A ON A.asset_id = F.asset_id
        WHERE date >= '%s'" %>%
            sprintf(date_from) %>%
            D.select
    }
    
    fillTradeStatus <- function(dat_trd) {
        date_start <- min(dat_trd$date_trade)
        dat_fx <- getHistoPxVsUSD(date_start)
        dat_histo_ohlc <- T.getHistoPx(date_from = date_start)
        1:nrow(dat_trd) %>% 
            lapply(function(i) T.calcTradingStrategyOneTrade(dat_trd[i,], dat_fx, dat_histo_ohlc)) %>%
            bind_rows
    }
    
    reorderColumns <- function(dat) {
        dat$last_modification <- format(Sys.time(), "%Y-%m-%d %H:%M:%S HKT")
        dat %>% 
            select(
                trade_id, strategy, ticker, 
                date_trade, predict, price_entry, buy_sell, target, stop_loss,
                trade_status, price_status, date_exit, price_exit, duration_bizdays, date_exit_latest,
                range_dn, range_up,
                tp_pct, notional_for_1k_pnl, 
                pnl_ccy_2_pct, pnl_ccy_2_for_notional,
                fx_ccy_2_usd_exit, pnl_usd_for_notional,
                last_modification   
            ) %>%
            mutate(last_modification = as.POSIXct(last_modification))
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    trd_new <- retrieveAllNewTrades()
    trd_ledger <- readFullLedger() %>%
        reorderColumns
    trd_matched <- matchTrades(trd_new, trd_ledger) %>%
        reorderColumns
    
    trd_unmatched <- unmatchedTrades(trd_new, trd_ledger) %>%
        fillTradeStatus %>%
        reorderColumns
    
    trd_date_exit <- YESTERDAY
    
    trd_ledger <- rbind(trd_matched,trd_unmatched) %>% 
        arrange(trade_id) %>%
        reorderColumns
    file_name <- paste0(path_trd_ledger, "/trades_ventura.csv")
    U.write.csv(trd_ledger, file_name)
    if (FALSE) {
        
        git_comment <- "Updating Full Trades Ledger Ventura %s" %>%
            sprintf(format(trd_date_exit, "%Y-%m-%d"));
        print(git_comment)
        
        git_command <- "cd %s && git add %s && git commit -m '%s' && git push origin master" %>%
            sprintf(path_trd_ledger, file_name, git_comment);
        print(git_command)
        
        trd_ledger %>% U.debug("Full Ledger");
        U.printBanner("Uploading now", FALSE);
        system(git_command)
        U.printBanner("Upload finished", FALSE);
        
        file_name <- "%s/%s_trades_ventura_exit.csv" %>%
            sprintf(path_trd_exit, format(trd_date_exit, "%Y%m%d"));
        trd_exit <- trd_ledger %>% 
            filter(date_exit == trd_date_exit);
        U.write.csv(trd_exit, file_name);
        
        git_comment <- "Adding Exit Trades Ventura %s" %>%
            sprintf(format(trd_date_exit, "%Y-%m-%d"));
        print(git_comment)
        
        git_command <- "cd %s && git add %s && git commit -m '%s' && git push origin master" %>%
            sprintf(path_trd_exit, file_name, git_comment);
        print(git_command)
        
        
        trd_exit %>% U.debug("Trades Exit");
        U.printBanner("Uploading now", FALSE);
        system(git_command)
        U.printBanner("Upload finished", FALSE);
    }
    
    trd_ledger
}
V.calcPnLVenturaLiveGithub <-
function () 
{
    time_start <- Sys.time()
    ####################################################################################################
    ### Script Parameters
    ####################################################################################################
    #   leverage <- 1;
    
    start_date <- as.Date("2020-10-01")
    end_date <- YESTERDAY + 1
    
    pnl_tp_usd <- 0.01
    initial_nav <- 100
    
    flat_fee_per_trade <- 0.5 / 10000
    
    capital_maximum_allocation_per_day <- 0.1
    max_capital_per_trade_vs_nav = 5
    
    transaction_cost_vs_target_threshold <- 0.005
    stop_loss_slippage_vs_bid_offer <- 1
    
    cost_of_carry_pct_per_annum <- 0.05
    
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    
    path_backtest <- paste0(DIRECTORY_CODE_HD, "Code/")
    file_names <- "trades_ventura.csv"
    
    file_names <- paste0(path_backtest, file_names)
    cols_to_keep <- c(
        "trade_id", "strategy", "ticker", "date_trade", "predict", "range_up",  
        "trade_status", "price_entry", "price_exit", "date_exit", "duration_bizdays", 
        "notional_for_1k_pnl", "pnl_ccy_2_pct", "pnl_ccy_2_for_notional", 
        "fx_ccy_2_usd_exit"
    )
    
    nb_strategies <- 14

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    getHistoPxVsUSD <- function() {
        "SELECT A.code AS ccy, F.date, F.fx
        FROM histo_fx_close_vs_usd F
        LEFT JOIN static_asset A ON A.asset_id = F.asset_id
        WHERE date >= '%s'" %>%
            sprintf(start_date) %>%
            D.select
    }
    
    getHistoPrices <- function() {
        T.getHistoPx(date_from = start_date)
    }
    
    readFiles <- function() {
        file_names %>% 
            U.read.csv %>%
            select(cols_to_keep) %>%
            distinct(ticker, date_trade, .keep_all = TRUE) %>% 
            left_join(select(INSTRUMENTS, instrument_id, pair, ticker), by = "ticker") %>%
            filter(
                !is.na(instrument_id),
                predict %in% c("up", "down")
            ) %>%
            rename(
                duration = duration_bizdays,
                date_entry = date_trade,
                t_up = "range_up",
                px_entry = price_entry,
                px_exit = price_exit,
                notional = notional_for_1k_pnl
            ) %>%
            mutate(
                ccy_1 = substr(pair, 1, 3),
                ccy_2 = substr(pair, 4, 6),
                buy_sell = case_when(
                    predict == "up" ~ 1,
                    predict == "down" ~ -1,
                    predict == "flat" ~ 0
                ),
                tgt = case_when(
                    (trade_status == "target") & (buy_sell == 1) ~ "up",
                    (trade_status == "target") & (buy_sell == -1) ~ "down",
                    (trade_status == "stop") & (buy_sell == 1) ~ "down",
                    (trade_status == "stop") & (buy_sell == -1) ~ "up",
                    (trade_status == "exit_maturity") ~ "flat",
                    TRUE ~ "flat"
                ),
                date_exit = case_when(
                    is.na(date_exit) ~ end_date,
                    TRUE ~ date_exit
                )
            ) %>%
            select(
                trade_id, strategy, instrument_id, pair, ccy_1, ccy_2, date_entry, buy_sell, 
                notional,
                px_entry, date_exit, px_exit, t_up, duration, trade_status, tgt
            ) %>%
            left_join(
                select(dat_histo, instrument_id, date, close) %>%
                    rename(date_exit = date), 
                by = c("instrument_id", "date_exit")
            ) %>%
            mutate(
                px_exit = case_when(
                    is.na(px_exit) ~ close,
                    TRUE ~ px_exit
                )
            ) %>%
            select(-close)
    }
    
    addNbTradesPerDay <- function(dat_backtest) {
        dat_per_day <- dat_backtest %>% 
            group_by(date_entry) %>% 
            summarize(n_trades_today = n()) %>%
            ungroup;
        dat_backtest %>% 
            left_join(dat_per_day, by = "date_entry");
    }
    
    calcIsTradeDoable <- function(dat_trd) {
        dat_doable <- select(ASSETS_IDX, pair, tradable_long, tradable_short) %>%
            rbind(
                ASSETS_FXEM %>%
                    select(ticker, ib_tradable) %>%
                    rename(pair = ticker, tradable_long = ib_tradable) %>%
                    mutate(tradable_short = tradable_long)
            ) %>%
            rbind(
                ASSETS_FXDM %>%
                    select(ticker) %>%
                    rename(pair = ticker) %>%
                    mutate(tradable_long = TRUE, tradable_short = TRUE)
            ) %>%
            rbind(
                ASSETS_MTL %>%
                    select(ticker, ib_tradable) %>%
                    rename(pair = ticker, tradable_long = ib_tradable) %>%
                    mutate(tradable_short = tradable_long)
            );
        
        dat_trd <- dat_trd %>%
            left_join(dat_doable, by = "pair")
        dat_trd$trade_doable <- TRUE
        dat_trd$trade_doable[which((dat_trd$buy_sell == 1) & !dat_trd$tradable_long)] <- FALSE
        dat_trd$trade_doable[which((dat_trd$buy_sell == -1) & !dat_trd$tradable_short)] <- FALSE
        dat_trd %>% 
            select(-tradable_long, -tradable_short)
    }
    
    calcIsTransactionCostCheapEnough <- function(dat_trd) {
        dat_trd %>% 
            left_join(fees_per_asset, by = "pair") %>% 
            mutate(
                pnl_target = abs(t_up / px_entry - 1),
                transaction_cost_pct = (brokerage_per_100k + bid_offer_cost_per_100k) / 100000,
                
                transaction_cost_vs_target = transaction_cost_pct / pnl_target,
                transaction_cost_cheap_enough = 
                    (transaction_cost_vs_target <= transaction_cost_vs_target_threshold),
            ) %>%
            select(-pnl_target, -transaction_cost_vs_target, -transaction_cost_pct, 
                   -brokerage_per_100k,-bid_offer_cost_per_100k, -cost_of_carry_pct_per_annum)
    }
    
    decideIfWeActuallyTradeIt_Try <- function(dat_trd) {
        dat_trd %>% 
            T.addAssetClass %>% 
            calcIsTradeDoable %>% 
            calcIsTransactionCostCheapEnough  %>% 
            mutate(
                #        trade_doable = TRUE,
                transaction_cost_cheap_enough = TRUE
            ) %>%
            filter(trade_doable, transaction_cost_cheap_enough);
        
    }
    decideIfWeActuallyTradeIt <- function(dat_trd) 
        U.tryNull(decideIfWeActuallyTradeIt_Try, dat_trd)
    
    prepareTradesList <- function(dat_backtest, dat_fx) {
        dat_backtest %>% 
            filter(buy_sell %in% c(1, -1)) %>% 
            left_join(
                rename(dat_fx, ccy_1 = ccy, date_entry = date), 
                by = c("ccy_1", "date_entry")
            ) %>% 
            mutate(
                fx = na.locf0(fx, fromLast = TRUE),
                tp_pct = t_up / px_entry - 1,
                pnl_tgt_ccy_1 = pnl_tp_usd / fx, 
                notional_ccy_1 = buy_sell * pnl_tgt_ccy_1 / tp_pct, 
                notional_ccy_1 = pmin(notional_ccy_1, max_capital_per_trade_vs_nav),
                notional_ccy_2 = -notional_ccy_1 * px_entry,
                trd_outcome = case_when(
                    ((buy_sell == 1) & (tgt == "up")) ~ "target",
                    ((buy_sell == -1) & (tgt == "down")) ~ "target",
                    ((buy_sell == 1) & (tgt == "down")) ~ "stop",
                    ((buy_sell == -1) & (tgt == "up")) ~ "stop",
                    (tgt == "flat") ~ "matured",
                    TRUE ~ "undetermined"
                )
            ) %>% 
            #decideIfWeActuallyTradeIt %>%  
            arrange(date_entry, pair, date_exit) %>%  
            select(
                trade_id,
                strategy,
                pair,
                ccy_1,
                ccy_2,
                date_entry,
                buy_sell,
                px_entry,
                date_exit,
                px_exit,
                notional_ccy_1,
                notional_ccy_2,
                trd_outcome
            ) 
    }
    
    preparePnLTable <- function() {
        pnl <- data.frame(
            date = date_list,
            nav_morning = NUM_NA,
            nav_evening = NUM_NA,
            risk_morning = 0, risk_evening = 0,
            pnl = 0, pnl_new = 0, pnl_live = 0, pnl_exit = 0,
            n_new = 0, n_live = 0, n_exit = 0, 
            risk_new = 0, risk_live = 0, risk_exit = 0,
            fees = 0, fees_new = 0, fees_live = 0, fees_exit = 0,
            max_so_far = 0, drawdown = 0, max_drawdown = 0
        ) %>%
            U.dataFrame;
        for (i in 1:nb_strategies) {
            pnl_i <- data.frame(
                date = date_list,
                nav_morning = 0, nav_evening = 0, 
                pnl = 0, pnl_new = 0, pnl_live = 0, pnl_exit = 0,
                n_new = 0, n_live = 0, n_exit = 0, 
                fees = 0, fees_new = 0, fees_live = 0, fees_exit = 0
            ) %>%
                U.dataFrame %>%
                select(-date)
            colnames(pnl_i) <- paste0(colnames(pnl_i), "_", i)
            pnl <- cbind(pnl, pnl_i)
        }
        pnl$nav_morning[1] <- initial_nav
        pnl$nav_evening[1] <- initial_nav
        for (j in 1:nb_strategies) {
            pnl[1, paste0("nav_morning_", j)] <- initial_nav
            pnl[1, paste0("nav_evening_", j)] <- initial_nav
        }
        pnl
    }
    
    initializeDailyMorningValues <- function(i, pnl) {
        pnl$nav_morning[i] <- pnl$nav_evening[i-1]
        pnl$risk_morning[i] <- pnl$risk_evening[i-1]
        
        for (j in 1:nb_strategies) {
            pnl[i,paste0("nav_morning_", j)] <- pnl[i-1, paste0("nav_evening_", j)]
        }
        pnl
    }
    
    preparePxYesterday_Try <- function(px_i) {
        px_i %>% 
            rename(px_t_1 = px_t)
    }
    preparePxYesterday <- function(px_i)
        U.tryNull(preparePxYesterday_Try, px_i)
    
    preparePxToday <- function(date_i) {
        dat_px %>%
            filter(date == date_i) %>%
            select(-date) %>%
            rename(px_t = fx)
    }
    
    findNotionalsForExistingTrade <- function(i, trd_live, pnl_per_day) {
        this_trd <- trd_live[i,]
        this_trd_date_entry <- this_trd$date_entry
        date_tag <- paste0("date_", format(this_trd_date_entry, "%Y%m%d"))
        trd_new_that_day <- pnl_per_day[[date_tag]]$trd_new %>%
            select(strategy, pair, buy_sell, notional_ccy_1, notional_ccy_2)
        this_trd %>%
            left_join(trd_new_that_day, by = c("strategy", "pair", "buy_sell")) %>%
            head(1) %>%
            select(strategy, pair, date_entry, buy_sell, notional_ccy_1, notional_ccy_2)
    }
    
    calcPnLTradesLive <- function(pnl, date_i, px_i, px_i_1, nav_morning, pnl_per_day) {
        trd_live <- dat_trd %>% 
            filter(
                date_entry < date_i, 
                date_exit >= date_i
            ) %>%
            select(-notional_ccy_1, -notional_ccy_2)
        
        if (U.dfContainsData(trd_live)) {
            
            findNotionalsForExistingTrade_local <- function(i) 
                findNotionalsForExistingTrade(i, trd_live, pnl_per_day)
            
            trd_notionals <- 1:nrow(trd_live) %>%
                lapply(findNotionalsForExistingTrade_local) %>%
                bind_rows
            
            trd_live <- trd_live %>%
                left_join(rename(px_i, ccy_1 = ccy, px_1_t = px_t), by = "ccy_1") %>% 
                left_join(rename(px_i, ccy_2 = ccy, px_2_t = px_t), by = "ccy_2") %>% 
                left_join(rename(px_i_1, ccy_1 = ccy, px_1_t_1 = px_t_1), by = "ccy_1") %>% 
                left_join(rename(px_i_1, ccy_2 = ccy, px_2_t_1 = px_t_1), by = "ccy_2") %>% 
                left_join(trd_notionals, by = c("strategy", "pair", "date_entry", "buy_sell")) %>%
            #    left_join(fees_per_asset, by = "pair") %>%
                mutate(
                    npv_usd_1_t = notional_ccy_1 * px_1_t,
                    npv_usd_1_t_1 = notional_ccy_1 * px_1_t_1,
                    npv_usd_2_t = notional_ccy_2 * px_2_t,
                    npv_usd_2_t_1 = notional_ccy_2 * px_2_t_1,
                    npv_usd_t = npv_usd_1_t + npv_usd_2_t,
                    npv_usd_t_1 = npv_usd_1_t_1 + npv_usd_2_t_1,
                    pnl_usd = npv_usd_t - npv_usd_t_1,
                    fees_carry = cost_of_carry_pct_per_annum * npv_usd_1_t / 365,
                    fees = fees_carry
                )
            
            for (j in 1:nb_strategies) {
                pnl[i,paste0("n_live_", j)] <- nrow(filter(trd_live, strategy == j))
                pnl[i,paste0("pnl_live_", j)] <- sum(filter(trd_live, strategy == j)$pnl_usd)
                pnl[i,paste0("fees_live_", j)] <- 0
            }
            
            pnl$n_live[i] <- nrow(trd_live)
            pnl$pnl_live[i] <- sum(trd_live$pnl_usd)
            pnl$risk_live[i] <- 0
            pnl$fees_live[i] <- sum(trd_live$fees)
        }
        list(pnl = pnl, trd_live = trd_live)
    }
    
    calcPnLTradesExit <- function(pnl, date_i, px_i, px_i_1, nav_morning, pnl_per_day) {
        trd_exit <- dat_trd %>% 
            filter(
                date_entry < date_i,
                date_exit == date_i
            ) %>%
            select(-notional_ccy_1, -notional_ccy_2)
        
        if (U.dfContainsData(trd_exit)) {
            findNotionalsForExistingTrade_local <- function(i) 
                findNotionalsForExistingTrade(i, trd_exit, pnl_per_day)
            
            trd_notionals <- 1:nrow(trd_exit) %>%
                lapply(findNotionalsForExistingTrade_local) %>%
                bind_rows;
            
            trd_exit <- trd_exit %>%
                left_join(rename(px_i, ccy_1 = ccy, px_1_t = px_t), by = "ccy_1") %>% 
                left_join(rename(px_i, ccy_2 = ccy, px_2_t = px_t), by = "ccy_2") %>% 
                left_join(trd_notionals, by = c("strategy", "pair", "date_entry", "buy_sell")) %>%
            #    left_join(fees_per_asset, by = "pair") %>%
                mutate(
                    npv_usd_1_t = notional_ccy_1 * px_1_t,
                    npv_usd_2_t = notional_ccy_2 * px_2_t,
                    npv_usd_1_exit = notional_ccy_1 * px_1_t,
                    npv_usd_2_exit = notional_ccy_2 / px_exit * px_1_t,
                    npv_usd_exit = (notional_ccy_1 + notional_ccy_2 * px_exit) * px_1_t,
                    pnl_usd = (npv_usd_2_exit - npv_usd_2_t),
                    fees_brokerage = 0, #,brokerage_per_100k * abs(npv_usd_1_t) / 100000,
                    fees_bid_offer = 0, #bid_offer_cost_per_100k * abs(npv_usd_1_t) / 100000,
                    fees_stop_loss = 0, #stop_loss_slippage_vs_bid_offer * fees_bid_offer * 
                        #(trd_outcome == "stop"),
                    flat_fee = flat_fee_per_trade * abs(npv_usd_1_t),
                    fees = fees_brokerage + fees_bid_offer + fees_stop_loss + flat_fee
                ) 
            
            for (j in 1:nb_strategies) {
                pnl[i,paste0("n_exit_", j)] <- nrow(filter(trd_exit, strategy == j))
                pnl[i,paste0("pnl_exit_", j)] <- sum(filter(trd_exit, strategy == j)$pnl_usd)
                pnl[i,paste0("fees_exit_", j)] <- sum(filter(trd_exit, strategy == j)$fees)
            }
            
            pnl$n_exit[i] <- nrow(trd_exit)
            pnl$pnl_exit[i] <- sum(trd_exit$pnl_usd)
            pnl$risk_exit[i] <- -pnl$n_exit[i] * pnl_tp_usd
            pnl$fees_exit[i] <- sum(trd_exit$fees)
            
        }
        list(pnl = pnl, trd_exit = trd_exit)
    }
    
    calcPnLTradesNew <- function(pnl, date_i, px_i, px_i_1, nav_morning) {
        trd_new <- dat_trd %>%
            filter(date_entry == date_i)
        
        if (U.dfContainsData(trd_new)) {
            
            nb_trades <- nrow(trd_new)    
            capital_allocation_ratio <- 100 * capital_maximum_allocation_per_day /
                pmax(100 * capital_maximum_allocation_per_day, nb_trades)
            
            trd_new <- trd_new %>%
                left_join(rename(px_i, ccy_1 = ccy, px_1_t = px_t), by = "ccy_1") %>% 
                left_join(rename(px_i, ccy_2 = ccy, px_2_t = px_t), by = "ccy_2") %>%
             #   left_join(fees_per_asset, by = "pair") %>%
                mutate(
                    notional_ccy_1 = notional_ccy_1 * nav_morning * capital_allocation_ratio,
                    notional_ccy_2 = notional_ccy_2 * nav_morning * capital_allocation_ratio,
                    npv_usd_1_t = notional_ccy_1 * px_1_t,
                    npv_usd_2_t = notional_ccy_2 * px_2_t,
                    pnl_usd = (npv_usd_1_t + npv_usd_2_t),
                    fees_brokerage = 0,#brokerage_per_100k * abs(npv_usd_1_t) / 100000,
                    fees_bid_offer = 0,#bid_offer_cost_per_100k * abs(npv_usd_1_t) / 100000,
                    flat_fee = flat_fee_per_trade * abs(npv_usd_1_t),
                    fees = fees_brokerage + fees_bid_offer + flat_fee
                ) 
            
            for (j in 1:nb_strategies) {
                pnl[i,paste0("n_new_", j)] <- nrow(filter(trd_new, strategy == j))
                pnl[i,paste0("pnl_new_", j)] <- sum(filter(trd_new, strategy == j)$pnl_usd)
                pnl[i,paste0("fees_new_", j)] <- sum(filter(trd_new, strategy == j)$fees)
            }
            
            pnl$n_new[i] <- nrow(trd_new)
            pnl$pnl_new[i] <- sum(trd_new$pnl_usd)
            
            pnl$risk_new[i] <- pnl$n_new[i] * pnl_tp_usd
            pnl$fees_new[i] <- sum(trd_new$fees)
        }
        list(pnl = pnl, trd_new = trd_new)
    }
    
    wrapUpDailyValues <- function(pnl) {
        for (j in 1:nb_strategies) {
            pnl[i,paste0("fees_", j)] <- 
                pnl[i,paste0("fees_new_", j)] + 
                pnl[i,paste0("fees_live_", j)] +
                pnl[i,paste0("fees_exit_", j)]
            
            pnl[i,paste0("pnl_", j)] <- 
                pnl[i,paste0("pnl_new_", j)] + 
                pnl[i,paste0("pnl_live_", j)] +
                pnl[i,paste0("pnl_exit_", j)] - 
                pnl[i,paste0("fees_", j)]
            
            pnl[i,paste0("nav_evening_", j)] <- 
                pnl[i,paste0("nav_morning_", j)] + pnl[i,paste0("pnl_", j)]
        }
        
        pnl$fees[i] <- pnl$fees_new[i] + pnl$fees_live[i] + pnl$fees_exit[i]
        
        pnl$risk_evening[i] <- pnl$risk_morning[i] + 
            pnl$risk_new[i] +
            pnl$risk_live[i] + 
            pnl$risk_exit[i]
        
        pnl$pnl[i] <- pnl$pnl_new[i] + pnl$pnl_live[i] + pnl$pnl_exit[i] - pnl$fees[i]
        
        pnl$nav_evening[i] <- pnl$nav_morning[i] + pnl$pnl[i]
        
        pnl
    }
    
    updatePnLPerDay <- function(pnl_per_day, trd_live, trd_new, trd_exit) {
        pnl_per_day[[paste0("date_", format(date_i, "%Y%m%d"))]] <- list(
            trd_live = dat_live$trd_live, trd_new = dat_new$trd_new, trd_exit = dat_exit$trd_exit
        )
        pnl_per_day
    }
    
    finalComputations <- function(pnl) {
        pnl %>%
            mutate(
                max_so_far = cummax(nav_evening),
                drawdown = (nav_evening < max_so_far) * (nav_evening / max_so_far - 1),
                max_drawdown = -cummin(drawdown)
            )
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_px <- getHistoPxVsUSD()
    dat_histo <- getHistoPrices()
    
    dat_backtest <- readFiles()
    
    min_date <- start_date
    date_list <- seq(min_date, end_date, 1)
    date_list_extra <- seq(min_date - 14, end_date, 1)
    
    pnl <- preparePnLTable()
    
    dat_trd <- prepareTradesList(dat_backtest, dat_px)
    
    px_i <- NULL
    pnl_per_day <- {}
    for (i in 2:nrow(pnl)) {
        date_i <- pnl$date[i]
        pnl <- initializeDailyMorningValues(i, pnl)
        px_i_1 <- preparePxYesterday(px_i)
        px_i <- preparePxToday(date_i)
        nav_morning <- pnl$nav_morning[i]
        dat_live <- calcPnLTradesLive(pnl, date_i, px_i, px_i_1, nav_morning, pnl_per_day)
        dat_exit <- calcPnLTradesExit(dat_live$pnl, date_i, px_i, px_i_1, nav_morning, pnl_per_day)
        dat_new <- calcPnLTradesNew(dat_exit$pnl, date_i, px_i, px_i_1, nav_morning)
        pnl <- wrapUpDailyValues(dat_new$pnl)
        pnl_per_day <- updatePnLPerDay(pnl_per_day)
    }
    
    pnl <- finalComputations(pnl)
    
    dat_summary <- pnl %>% 
        filter(date <= YESTERDAY) %>%
        mutate(month = substr(date, 1, 7)) %>%
        group_by(month) %>%
        mutate(
            max_so_far = cummax(nav_evening),
            drawdown = (nav_evening < max_so_far) * (nav_evening / max_so_far - 1),
            day_rtn = log(nav_evening / nav_morning)
        ) %>%
        summarize(
            nb_trades = sum(n_new),
            rtn = 100 * (last(nav_evening) / first(nav_morning) - 1),
            max_drawdown_pct = -100 * min(drawdown),
            lowest_ytd_pct = NUM_NA, #100 * (min(nav_evening) / first(nav_morning) - 1),
            volatility = 100 * sd(day_rtn) * sqrt(365)
        ) %>% 
        ungroup %>% 
        mutate(
            drawdown_ratio = NUM_NA, #rtn / max_drawdown, 
            sharpe = NUM_NA, #rtn / volatility
        ) 
    
    total_years = U.yearFrac(pnl$date[1], tail(pnl$date, 1))
    
    dat_summary_total <- pnl %>% 
        filter(date <= YESTERDAY) %>%
        mutate(
            month = "Total Annualized",
            max_so_far = cummax(nav_evening),
            drawdown = (nav_evening < max_so_far) * (nav_evening / max_so_far - 1),
            day_rtn = log(nav_evening / nav_morning)
        ) %>%
        summarize(
            month = last(month),
            nb_trades = sum(n_new),
            rtn = 100 * ((last(nav_evening) / first(nav_morning)) ^ (1 / total_years) - 1),
            max_drawdown_pct = -100 * min(drawdown),
            lowest_ytd_pct = 100 * (min(nav_evening) / first(nav_morning) - 1),
            volatility = 100 * sd(day_rtn) * sqrt(365)
        ) %>% 
        ungroup %>% 
        mutate(
            drawdown_ratio = rtn / max_drawdown_pct, 
            sharpe = rtn / volatility
        ) 
    
    dat_summary <- rbind(dat_summary, dat_summary_total)

    dat_pnl <- list(
        pnl = pnl, 
        pnl_per_day = pnl_per_day, 
        data = dat_backtest,
        summary = dat_summary
    )
    
    file_name <- paste0(DIRECTORY_DATA_HD, "Git_PnL/pnl_latest.RData")
    save(dat_pnl, file = file_name)
    file_name <- gsub("latest", format(YESTERDAY, "%Y-%m-%d"), file_name)
    save(dat_pnl, file = file_name)
    dat_pnl
    
}
V.genAutocorrelatedUniformDistribution <-
function (N = 1000, rho = cos(0.5)) 
{
    # ref
    # https://stats.stackexchange.com/questions/48086/algorithm-to-produce-autocorrelated-uniformly-distributed-number
    theta <- acos(rho)
    cos_t <- rho
    sin_t <- sin(theta)
    c_p_s <- cos_t + sin_t
    c_m_s <- cos_t - sin_t
    csc2t <- csc(2*theta)
    s2t <- sin(2*theta)
    
    X <- rep(0, N)
    epsilon <- runif(N)
    X[1] <- epsilon[1]
    sq3 <- sqrt(3)
    
    z1 <- sq3 * c_p_s
    z2 <- sq3 * c_m_s
    z3 <- -z2
    z4 <- -z1
    
    f1 <- function(z) {
        csc2t * (-3 - z**2 + 2 * sq3 * z * c_p_s + 9 * s2t) / 12
    }
    
    f2 <- function(z) {
        (3 + sq3 * z / cos_t) / 6
    }
    
    f3 <- function(z) {
        csc2t * (z**2 + 2 * sq3 * z * c_p_s + 3 * (1 +  s2t)) / 12
    }
    
    F <- function(z) {
        if (z >= z1)
            1
        else if (z >= z2)
            f1(z)
        else if (z >= z3)
            f2(z)
        else if (z >= z4)
            f3(z)
        else
            0
    }
    
    
    
    
    for (i in 2:N) {
        X[i] <- sq3 * (F(cos_t * X[i-1] + sin_t * epsilon[i]) - 0.5)
    }
  
    X 

}
V.getIBFutOptionPrice <-
function (ticker, expiry, strike, call_put) 
{
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    expiry <- as.Date(expiry)
    ####################################################################################################
    ### Sun routines
    ####################################################################################################
    getIBPrice_Try <- function(this_contract, this_connection) {
        reqMktData(this_connection, this_contract, tickGenerics = "", snapshot = TRUE)

    }
    getIBPrice <- function(this_contract, this_connection)
        U.try(getIBPrice_Try, NULL)(this_contract, this_connection)
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    ib_contract <- twsContract(
        conId = "",
        symbol = ticker,
        sectype = "FOP",
        exch = "GLOBEX",
        primary = "",
        currency = "USD",
        expiry = format(expiry, "%Y%m%d"),
        strike = as.character(strike),
        right = toupper(substr(call_put, 1, 1)),
        multiplier = "",
        combo_legs_desc = "",
        comboleg = "",
        include_expired = "",
        local = ""
    );
    ib_conn <- twsConnect(clientId = 5, port = 7498);

    ib_contract <- reqContractDetails(ib_conn, ib_contract);
    if (length(ib_contract) == 0) {
        ib_contract <- NULL;
    }
    else {
        ib_contract <- ib_contract[[1]]$contract;
    }
    ib_mkt_data <- NULL;
    if (!is.null(ib_contract)) {
        ib_mkt_data <- getIBPrice(ib_contract, ib_conn)    
    }
    
    twsDisconnect(ib_conn);
    ib_mkt_data;
}
V.modelPredict <-
function (
    strats_to_do = 1:length(VENTURA$strats), 
    live_or_close = "live", 
    predict_time = Sys.time(),
    wait_for_technicals = FALSE,
    pair_list = NULL,
    with_weights = FALSE
) 
{
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    file_path_model <- paste0(DIRECTORY_DATA_SD, "Models/model_%s.RData")
    if (with_weights) {
        file_path_model <- gsub("model_", "model_weights_", file_path_model)    
    }
    
    if (is.null(pair_list)) {
        pair_list <- filter(INSTRUMENTS, use_for_training + use_for_trading >= 1)$pair
    }
    instrument_id_list <- filter(INSTRUMENTS, pair %in% pair_list)$instrument_id
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    loadTechnicals <- function() {
        U.printBanner(sprintf("Importing technicals"))
        T.getTechnicalsLive() %>%
            filter(instrument_id %in% instrument_id_list)
    }
    
    waitForTechnicalsToFinishAfterLoadingFirstModel <- function(dat, i) {
        force(dat)
        if (i == strats_to_do[1]) {
         #   predict_time <- Sys.time()
         #   second(predict_time) <- 0
         #   minute(predict_time) <- U.mround(minute(predict_time)+1, 5)
         #   predict_time <- predict_time %>% 
         #       as.character %>% 
         #       as.POSIXct(tz=TZ_LOCAL)
         #   predict_time <<- predict_time
            if (wait_for_technicals) D.waitTillPreviousJobHasFinished("Live", 3, 20, 5, 7)
            dat_technicals <<- loadTechnicals()
        }
        dat
    }
    
    loadModel <- function(i) {
        load(sprintf(file_path_model, i))
        dat_trades_models
    }
    
    predictModel <- function(dat_model, dat_tech) {
        dat_predict <- E.modelPredict(dat_tech, dat_model, TRUE)
        colnames(dat_predict) <- gsub("predict_proba_", "proba_", colnames(dat_predict))
        dat_predict
    }
    
    scoreTrades_Try <- function(dat, i) {
        dat %>% 
            E.genericScoreFunction(i) %>% 
            T.addBollingerBandsFLS(1, "D") %>% 
            T.addTargetLevels(i)
    }
    scoreTrades <- function(dat, i) 
        U.try(scoreTrades_Try, NA)(dat, i)
    
    saveToDB <- function(dat, i) {
        print(dat %>% data.frame)
        #    D.SQL("DELETE FROM live_predict WHERE strategy_id = %s" %>% sprintf(i))
        U.printBanner("Saving to live_predict", FALSE)
        D.replaceDataIntoTable("live_predict", dat, FALSE)
        U.printBanner("Saving to archive_predict", FALSE)
        D.insertDataIntoTable("archive_predict", dat, FALSE)
        U.printBanner("Saving done", FALSE)
        NULL
    }
    
    doOneStrategy_Try <- function(i) {
        U.printBanner(sprintf("Starting Strategy %s", i))
        dat <- i %>% 
            U.printMilestone(sprintf("Importing pre-trained model strategy %s ...", i)) %>%
            loadModel 
        dat <- dat %>%
            U.printMilestone("Import model done...") %>% 
            waitForTechnicalsToFinishAfterLoadingFirstModel(i) %>%
            U.printMilestone("Starting model predict...") %>%
            predictModel(dat_technicals) 
        dat <- dat %>%
            U.printMilestone("Model prediction finished...") %>%
            scoreTrades(i) 
        dat <- dat %>%
            mutate(
                strategy_id = i,
                timestamp = predict_time,
                use_weights = as.integer(with_weights)
            ) %>%
            left_join(rename(TRADE_OUTCOMES, predict = outcome), by = "predict") %>% 
            filter(
                !is.na(instrument_id),
                !is.na(date),
                !is.na(outcome_id),
                !is.na(t_up),
                !is.na(t_dn)
            ) %>%
            select(
                instrument_id, strategy_id, use_weights, date, timestamp, timestamp_px, close, 
                outcome_id, 
                t_up, t_dn, score, starts_with("score_"), starts_with("proba"), -score_high_enough
            ) %>% 
            U.printMilestone("Predictions done for model %s, saving to DB now" %>% sprintf(i)) 
        
        dat <- dat %>%
            saveToDB(i)
        gc()
        U.printBanner(sprintf("Strategy %s - All done", i), FALSE);
        NULL
    }
    doOneStrategy <- function(i) U.tryNull(doOneStrategy_Try, i)
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    lapply(strats_to_do, doOneStrategy_Try)
}
V.modelPredictAnyTime <-
function (
    strats_to_do = 1:length(VENTURA$strats), 
    pair_list = NULL,
    with_weights = FALSE
) 
{
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    file_path_model <- paste0(DIRECTORY_DATA_SD, "Models/model_%s.RData")
    if (with_weights) {
        file_path_model <- gsub("model_", "model_weights_", file_path_model)    
    }
    
    if (is.null(pair_list)) {
        pair_list <- filter(INSTRUMENTS, use_for_training + use_for_trading >= 1)$pair
    }
    instrument_id_list <- filter(INSTRUMENTS, pair %in% pair_list)$instrument_id
    
    predict_time <- Sys.time()
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    loadTechnicals <- function() {
        U.printBanner(sprintf("Importing technicals"))
        T.getTechnicalsLive() %>%
            filter(instrument_id %in% instrument_id_list)
    }
    
    waitForTechnicalsToFinishAfterLoadingFirstModel <- function(dat, i) {
        force(dat)
        if (i == strats_to_do[1]) {
            dat_technicals <<- loadTechnicals()
        }
        dat
    }
    
    loadModel <- function(i) {
        load(sprintf(file_path_model, i))
        dat_trades_models
    }
    
    predictModel <- function(dat_model, dat_tech) {
        dat_predict <- E.modelPredict(dat_tech, dat_model, TRUE)
        colnames(dat_predict) <- gsub("predict_proba_", "proba_", colnames(dat_predict))
        dat_predict
    }
    
    scoreTrades_Try <- function(dat, i) {
        dat %>% 
            E.genericScoreFunction(i) %>% 
            T.addBollingerBandsFLS(1, "D") %>% 
            T.addTargetLevels(i)
    }
    scoreTrades <- function(dat, i) 
        U.try(scoreTrades_Try, NA)(dat, i)
    
    doOneStrategy_Try <- function(i) {
        U.printBanner(sprintf("Starting Strategy %s", i))
        dat <- i %>% 
            U.printMilestone(sprintf("Importing pre-trained model strategy %s ...", i)) %>%
            loadModel 
        dat <- dat %>%
            U.printMilestone("Import model done...") %>% 
            waitForTechnicalsToFinishAfterLoadingFirstModel(i) %>%
            U.printMilestone("Starting model predict...") %>%
            predictModel(dat_technicals) 
        dat <- dat %>%
            U.printMilestone("Model prediction finished...") %>%
            scoreTrades(i) 
        dat <- dat %>%
            mutate(
                strategy_id = i,
                timestamp = predict_time,
                use_weights = as.integer(with_weights)
            ) %>%
            left_join(rename(TRADE_OUTCOMES, predict = outcome), by = "predict") %>% 
            filter(
                !is.na(instrument_id),
                !is.na(date),
                !is.na(outcome_id),
                !is.na(t_up),
                !is.na(t_dn)
            ) %>%
            select(
                instrument_id, strategy_id, use_weights, date, timestamp, timestamp_px, close, 
                outcome_id, 
                t_up, t_dn, score, starts_with("score_"), starts_with("proba"), -score_high_enough
            ) %>% 
            U.printMilestone("Predictions done for model %s, saving to DB now" %>% sprintf(i)) 
        gc()
        U.printBanner(sprintf("Strategy %s - All done", i), FALSE);
        dat
    }
    doOneStrategy <- function(i) U.tryNull(doOneStrategy_Try, i)
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    lapply(strats_to_do, doOneStrategy_Try)
}
V.modelPredictScenario <-
function (strats_to_do = 1:length(VENTURA$strats), wait_for_previous_script = TRUE, bump_list = NULL) 
{
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    file_path_model <- paste0(DIRECTORY_DATA_SD, "Models/model_%s.RData")
    BUMPS <- D.loadTable("static_scenario_bump")
    predict_time <<- start_time - second(start_time)
    if (is.null(bump_list)) {
        bump_list <- BUMPS$bump
    }
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    loadModel <- function(i) {
        U.printBanner(sprintf("Importing pre-trained model strategy %s ...", i), FALSE)
        load(sprintf(file_path_model, i))
        U.printBanner("Import model done...", FALSE)
        dat_trades_models
    }

    scoreTrades_Try <- function(dat, i) {
        dat %>% 
            E.genericScoreFunction(i) %>% 
            T.addBollingerBandsFLS(1, "D") %>%
            T.addTargetLevels(i)
    }
    scoreTrades <- function(dat, i) 
        U.try(scoreTrades_Try, NA)(dat, i)

    saveToDB <- function(dat, scenario_bump_id, strat_id) {
        msg_save <- "Predictions done for model %s, bump %s, saving to DB now" %>% 
            sprintf(
                strat_id, 
                filter(BUMPS, bump_id == scenario_bump_id)$bump
            )
        U.printBanner(msg_save, FALSE)
        
        "DELETE FROM scenario_predict WHERE strategy_id = %s AND bump_id = %s" %>% 
            sprintf(strat_id, scenario_bump_id) %>%
            D.SQL
        D.insertDataIntoTable("scenario_predict", dat, FALSE)
        NULL
    }
    
    doOneScenario <- function(scenario_bump, strat_id, strat_model) {
        dat_technicals <- T.getTechnicalsLive(scenario_bump)
        scenario_bump_id <- filter(BUMPS, bump == scenario_bump)$bump_id

        dat <- E.modelPredict(dat_technicals, strat_model, FALSE) %>%
            scoreTrades(strat_id) %>%
            mutate(
                bump_id = scenario_bump_id,
                strategy_id = strat_id,
                timestamp_scenario = predict_time
            ) %>%
            left_join(rename(TRADE_OUTCOMES, predict = outcome), by = "predict") %>%
            filter(
                !is.na(instrument_id),
                !is.na(date),
                !is.na(outcome_id),
                !is.na(t_up),
                !is.na(t_dn)
            ) %>%
            rename(price = close) %>%
            select(
                timestamp_scenario, instrument_id, bump_id, strategy_id,
                price, score, outcome_id
            ) 
        dat %>% 
            saveToDB(scenario_bump_id, strat_id)
        gc()
    }
    
    doOneStrategy <- function(strat_id) {
        U.printBanner(sprintf("Starting Strategy %s", strat_id))
        strat_model <- loadModel(strat_id)
        if ((strat_id == 1) & wait_for_previous_script) {
            D.waitTillPreviousJobHasFinished("Calc_Bumped", 2, 20, 60, 15)
        }
        
        bump_list %>%
            lapply(
                function(scenario_bump) doOneScenario(scenario_bump, strat_id, strat_model)
            )
        
        rm(strat_model)
        gc()
        U.printBanner(sprintf("Strategy %s - All done", strat_id), FALSE)
        NULL
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    lapply(strats_to_do, doOneStrategy)
    
}
V.plotSpotAndFeature <-
function (
    fx_pair, year_start, duration = 1, view_feature, threshold_low = NULL, threshold_high = NULL,
    log_scale = TRUE) 
{
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    dat_feature <- D.loadTableLocal("feature") %>% 
        filter(feature == view_feature)
    
    int_or_dbl <- as.character(U.vectorize(dat_feature$int_or_dbl[1]))
    
    feature_tbl <- switch(
        int_or_dbl,
        "0" = "int",
        "1" = "dbl",
        NA) %>%
        paste0("histo_technicals_", .)
    
    feature_id <- dat_feature$feature_id
    
    date_start <- as.Date(paste0(year_start, "-01-01"))
    date_end <- as.Date(paste0(year_start + duration - 1, "-12-31"))
    
    instrument_id <- A.getInstrumentId(fx_pair)
    
    plotStackedPlots <- function(plot_1, plot_2) {
        chart_1 <- plot_1 %>% 
            ggplot_build %>% 
            ggplot_gtable;
        chart_2 <- plot_2 %>% 
            ggplot_build %>% 
            ggplot_gtable;
        
        chart_combined <- gtable:::rbind_gtable(chart_1, chart_2, "first")
        
        panels <- chart_combined$layout$t[grep("panel", chart_combined$layout$name)]
        
        chart_combined$heights[panels[1]] <- unit(4, "null")
        chart_combined$heights[panels[2]] <- unit(1, "null")
        
        grid.newpage()
        grid.draw(chart_combined)
    }
    ####################################################################################################
    ### Script 
    ####################################################################################################
    
    dat_px <- "SELECT date, close 
        FROM histo_px_daily 
        WHERE instrument_id = %s
        AND date >= '%s' 
        AND date <= '%s'" %>%
        sprintf(instrument_id, date_start, date_end) %>% 
        D.select
    
    dat_tech <- "SELECT date, value AS feature
        FROM %s
        WHERE instrument_id = %s
        AND feature_id = %s
        AND date >= '%s' 
        AND date <= '%s'" %>%
        sprintf(feature_tbl, instrument_id, feature_id, date_start, date_end) %>% 
        D.select 
    
    dat <- dat_px %>% 
        left_join(dat_tech, by = "date")
    
    dat_feature_0 <- NULL
    dat_feature_1 <- NULL
    dat_feature_2 <- NULL
    if (is.null(threshold_low) & is.null(threshold_high)) {
        dat_feature_0 <- filter(dat, date < date_start)
    }
    if (!is.null(threshold_low)) {
        dat_feature_1 <- filter(dat, feature > threshold_low)
    }
    if (!is.null(threshold_high)) {
        dat_feature_2 <- filter(dat, feature < threshold_high)
    }
    dat_feature <- rbind(dat_feature_0, dat_feature_1, dat_feature_2)
    
    y_min <- ifelse(log_scale, 0, -Inf)
    
    plot_1 <- dat %>% 
        ggplot(aes(x = date, y = close)) + 
        geom_rect(
            aes(ymin=y_min, ymax=Inf, xmin=date-0.5, xmax=lead(date, 1)-0.499, fill = feature), 
            alpha =0.5
        ) + 
        scale_fill_viridis_c() +
        geom_line() + 
        geom_point(data = dat_feature, color = "red") +
        scale_x_date(date_labels = "%b%y") + 
        theme(
            legend.position = "bottom",
            axis.text=element_text(size = 10), 
            axis.title.y = element_blank(),
            axis.title.x = element_blank()
        ) 
    if (log_scale) {
        plot_1 <- plot_1 + 
            scale_y_continuous(trans = 'log10');
    }
    
    plot_2 <- dat %>% 
        ggplot(aes(x = date, y = feature)) + 
        geom_line() + 
        scale_x_date(date_labels = "%b%y") + 
        theme(
            legend.position = "none",
            axis.text=element_text(size = 10), 
            axis.title.y = element_blank(),
            axis.title.x = element_blank()
        ) 
    
    plotStackedPlots(plot_1, plot_2)
}
V.plotVarImportance <-
function(dat_model) {
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    
    U.printBanner("Starting variable importance...", FALSE);

    var_imp <- varimp(dat_model);
    var_imp <- var_imp[order(var_imp)];
    
    var_imp_plot <- data.frame(
        feature = names(var_imp), 
        var_imp = U.vectorize(var_imp)
    ) %>% 
        ggplot(aes(x = reorder(feature, var_imp), y = var_imp)) + 
        geom_point(col = "lightseagreen") + 
        geom_segment(
            aes(x = feature, xend = feature, y = 0, yend = var_imp), 
            col = "lightseagreen"
        ) + 
        coord_flip() +            
        theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
        );
    print(var_imp_plot);
    
    var_imp;
}
V.pptCharts <-
function () 
{
    
    ####################################################################################################
    a.1 %>% 
        mutate(cagr = (nav / 100) ^ (1 / a.2) - 1) %>% 
        ggplot(aes(x = fee, y = cagr)) +
        geom_line(col = "lightseagreen") + 
        geom_vline(xintercept = 0, size = 0.2) + 
        geom_hline(yintercept = 0, size = 0.2) + 
        geom_hline(yintercept = 1.74790565, size = 0.5, color = "darkorange2") + # Gross
        geom_hline(yintercept = 1.54861759, size = 0.5, color = "royalblue3") + # After bro + carry
        geom_hline(yintercept = 1.110710386, size = 0.5, color = "deeppink3") + # Doable 
        annotate(
            "text", 
            x = c(17,17, 17), y = c(1.74790565 + 0.03, 1.54861759 + 0.03,1.110710386 + 0.03 ), 
            label = c("Gross", "After transaction costs, bid-offer, funding", "Trades doable on IB"), 
            color = c("darkorange2", "royalblue3", "deeppink3")
        ) +
        annotate(
            "segment", 
            x = 14.5, xend = 4.1, y = 0.85, yend = 1.110710386 - 0.025, 
            colour = "tomato4", 
            size=0.5, alpha=1, arrow=arrow()
        ) +
        geom_label(x = 17, y = 0.85, label = "Current setup ~ 4 bps/leg all-in on average", color = "tomato4") + 
        annotate(
            "segment", 
            x = 17, xend = 14.25, y = 0.45, yend = 0.02, 
            colour = "darkorchid4", 
            size=0.5, alpha=1, arrow=arrow()
        ) +
        annotate("text", x = 17, y = 0.5, label = "Large buffer", color = "darkorchid4") + 
        labs(
            title= "Return vs transaction cost",
            x = "All-in transaction cost per leg from gross mid (bps) incl. brokerage, bid-offer, funding, taxes etc)",
            y="Cumulative annualized return"
        ) + 
        scale_y_continuous(labels=percent) + 
        theme(
            axis.text=element_text(size=12),
            axis.title=element_text(size=12)
        )
    ####################################################################################################
    
    dat$data %>% 
        filter(
            date_entry >= "1995-01-01", 
            buy_sell %in% c(-1, 1)
        ) %>%
        mutate(
            year = year(date_entry),
            gross_pnl = buy_sell * (px_exit - px_entry) / (t_up - px_entry)
        ) %>%
        ggplot(aes(x=gross_pnl)) +
        geom_histogram(fill = "lightseagreen", bins = 25) + 
        aes(y=stat(count)/sum(stat(count))) + 
        scale_y_continuous(labels = scales::percent) + 
        scale_x_continuous(limits = c(-2, 2)) + 
        labs(
            title= "Gross PnL distribution",
            x = "PnL per trade (+1 = hit target, -1 = stopped)"
        ) + 
        theme(
            axis.title.y = element_blank(),
            axis.text=element_text(size=12),
            axis.title = element_text(size=12)
        )
    ####################################################################################################
    dat$pnl %>%
        filter(date >= "1995-01-01") %>%
        ggplot(aes(x=n_live)) +
        geom_histogram(fill = "lightseagreen", bins = 50) + 
        aes(y=stat(count)/sum(stat(count))) + 
        scale_y_continuous(labels = scales::percent, limits = c(NA, 0.1)) + 
        scale_x_continuous(limits = c(-1, 50)) + 
        labs(
            title= "Distribution of number of live trades on a day",
            x = "Nb open trades"
        ) + 
        theme(
            axis.title.y = element_blank(),
            axis.text=element_text(size=12),
            axis.title = element_text(size=12)
        ) + 
        annotate(
            "rect", 
            xmin = 30, xmax = 50, ymin = 0.08, ymax = 0.1, 
            alpha = 0.25,
        ) +
        annotate(
            "text", 
            x = 40, y = 0.09,
            label = "  <= 5 Live Trades:   43% of the time  \n<= 10 Live Trades: 72% of the time\n<= 20 Live Trades: 93% of the time", size = 8
        )
    ####################################################################################################
    a.1 %>% ggplot(aes(x = date, y = time_dbl)) + geom_line(color = "lightseagreen") + 
        scale_x_date(date_breaks = "years", date_labels = "%y") +
        theme(
            axis.title.x = element_blank(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12)
        ) +
    labs(
        title = "Time for NAV to double (Years)",
        y = "Doubling time"
    )
    ####################################################################################################
    a.1 <- dat$data %>% 
        filter(
            date_entry >= "1995-01-01", 
            buy_sell %in% c(-1, 1)
        ) %>%
        mutate(
            gross_pnl = buy_sell * (px_exit - px_entry) / (t_up - px_entry),
            win = (gross_pnl >= 0) + 0,
            pnl_win = win * gross_pnl,
            pnl_lose = (1-win) * gross_pnl
        )
    
    dat_pnl <- dat$pnl %>% 
        select(date) %>%
        filter(date >= "1997-01-01") %>%
        mutate(gross_pnl = NUM_NA, win_ratio = NUM_NA, profit_ratio = NUM_NA)
    
    for (i in 1:nrow(dat_pnl)) {
        rr <- a.1 %>%
            filter(
                date_entry >= dat_pnl$date[i] %m+% years(-2),
                date_entry < dat_pnl$date[i]
            ) %>%
            summarize(
                m = mean(gross_pnl),
                w = sum(win) / n(),
                p = -mean(pnl_win) / mean(pnl_lose)
            );
        dat_pnl$gross_pnl[i] <- U.vectorize(rr$m[1]);
        dat_pnl$win_ratio[i] <- U.vectorize(rr$w[1]);
        dat_pnl$profit_ratio[i] <- U.vectorize(rr$p[1]);
    }
    dat_pnl <- dat_pnl %>% U.data2Tibble;
    
    dat_pnl %>% 
        ggplot(aes(x = date, y = gross_pnl)) + 
        geom_step(col = "lightseagreen") + 
        geom_smooth(method="lm", se=FALSE, size = 0.2) +
        #  geom_hline(yintercept = 0, size = 0.2) + 
        scale_x_date(date_breaks = "years", date_labels = "%y") +
        scale_y_continuous(labels = percent, limits = c(0, 0.35)) +
        theme(
            axis.title.x = element_blank(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12)
        ) +
        labs(
            title = "Average gross PnL, last 2 years rolling\n (1 = Hit target, -1 = Stopped)",
            y = "Average gross trade PnL"
        )
    # (- average pnl when positive / average pnl when negative)
    ####################################################################################################
    "/home/fls/Mount/Glenorchy/FX/Backtestings/backtest_strat_14_minus120features_2020-08-26_20h12m53s.csv" %>% 
        U.read.csv %>%
        filter(
            predict %in% c("up", "down")
        ) %>%
        mutate(
            year = year(date),
            buy_sell = (predict == "up") - (predict == "down"),
            gross_pnl = buy_sell * (px_exit - close) / (t_up - close)
        ) %>%
        group_by(year) %>%
        summarize(N = n(), gross_pnl = mean(gross_pnl))
    ####################################################################################################
    a.1$pnl %>% 
        filter(date >= "1995-01-01") %>%
        mutate(
            quarter = 
                case_when(
                    month(date) %in% 1:3 ~ "Q1",
                    month(date) %in% 4:6 ~ "Q2",
                    month(date) %in% 7:9 ~ "Q3",
                    month(date) %in% 10:12 ~ "Q4"
                ),
            quarter = paste0(year(date), "-", quarter),
            
        ) %>%
        group_by(quarter) %>% 
        summarize(rtn = last(nav_evening) / first(nav_morning) - 1) %>%
        ungroup %>%
        ggplot(aes(x = rtn))  +
        geom_histogram(fill = "lightseagreen", bins = 20) + 
        aes(y=stat(count)/sum(stat(count))) + 
        geom_vline(xintercept = 0, size = 0.25, linetype = "dashed") +
        scale_x_continuous(labels = scales::percent, breaks=(-10:100)/5) +
        scale_y_continuous(labels = scales::percent) +
        labs(
            title= "Distribution of monthly P&L - SPX",
            x = "Month P&L (%)"
        ) + 
        theme(
            axis.title.y = element_blank(),
            axis.text=element_text(size=12),
            axis.title = element_text(size=12)
        )
    ####################################################################################################
    dat$data %>% 
        filter(
            date_entry >= "1995-01-01", 
            buy_sell %in% c(-1, 1)
        ) %>%
        mutate(
            year = year(date_entry),
            gross_pnl = buy_sell * (px_exit - px_entry) / (t_up - px_entry),
            win = (gross_pnl >= 0) + 0,
            pnl_win = win * gross_pnl,
            pnl_lose = (1-win) * gross_pnl,
            status = case_when(
                gross_pnl <= -1 ~ "stop",
                gross_pnl >= 1 ~ "target",
                TRUE ~ "middle"
            ),
            stop = (status == "stop") + 0,
            target = (status == "target") + 0,
            middle = (status == "middle") + 0
        ) %>%
        group_by(strategy) %>%
        summarize(
            nb_trd = n() / 25.5,
            duration = mean(duration),
            avg_gross_pnl = mean(gross_pnl),
            win_ratio = sum(win) / n(),
            profit_ratio = -mean(pnl_win) / mean(pnl_lose),
            stop = sum(stop) / n(),
            middle = sum(middle) / n(),
            target = sum(target) / n()
        ) %>% ungroup %>%
        U.write.csv("p2.csv")
    
    ####################################################################################################
    dat$pnl %>%
        filter(date >= "1995-01-01") %>%
        mutate(
            year = year(date),
            rtn = nav_evening / nav_morning - 1,
            rtn_log = log(nav_evening / nav_morning)
        ) %>%
        group_by(year) %>%
        mutate(
            max_so_far = cummax(nav_evening),
            drawdown = -(nav_evening < max_so_far) * (nav_evening / max_so_far - 1),
            max_drawdown = cummax(drawdown)
        ) %>%
        summarize(
            return = last(nav_evening) / first(nav_morning) - 1,
            volatility = sd(rtn_log) * sqrt(365),
            max_drawdown = max(max_drawdown),
            lowest_ytd = min(nav_evening) / first(nav_morning) - 1,
            sharpe = return / volatility,
            drawdown_ratio = return / max_drawdown
        ) %>%
        U.write.csv("p4.csv")
    
}
V.prepareExecutionSchedule <-
function () 
{
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    ####################################################################################################
    ### Script 
    ####################################################################################################
    dat <- 
        "SELECT M.execution_time_tz_close, T.time_zone 
        FROM static_instrument I
        LEFT JOIN static_market M ON M.market_id = I.market_id
        LEFT JOIN static_time_zone T ON T.time_zone_id = M.tz_close_id
        WHERE I.use_for_trading_ib = 1" %>%
        D.SQL
    
    dat$execution_time <- mapply(
        function(t, t_z) format(as.POSIXct(paste0(TO_DAY, " ", t), tz = t_z), tz = TZ_LOCAL), 
        dat$execution_time_tz_close, 
        dat$time_zone
    )
    
    dat %>% 
        select(execution_time) %>% 
        unique %>% 
        arrange(execution_time) %>%
        mutate(
            execution_time_id = 1:nrow(.),
            execution_time = format(as.POSIXct(execution_time, tz = tzhk), "%H:%M:%S", tz = TZ_LOCAL)
        ) %>%
        select(execution_time_id, execution_time)
}
V.prepareMorningModel <-
function (strat_id_list, use_weights) 
{
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    file_path_model <- paste0(DIRECTORY_DATA_SD, "Models/model_%s.RData")
    if (use_weights) {
        file_path_model <- gsub("/model_", "/model_weights_", file_path_model)    
    }

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    saveModel <- function(dat_trades_models, i) {
        U.printBanner("Model:", FALSE)
        print(dat_trades_models$model)
        U.printBanner("Saving model", FALSE)
        file_name <- sprintf(file_path_model, i)
        save(dat_trades_models, file = file_name)
        U.printBanner("Saving done...", FALSE)
        rm(dat_trades_models)
        gc()
    } 
    
    prepareModel_Try <- function(i) {
        U.printBanner(sprintf("Training model strategy %s ...", i))
        U.printBanner("Importing precalculated technicals", FALSE)
        dat_tech <- T.getTechnicals("train", i, TRUE)
        if (use_weights) {
            U.printBanner("Doing model with weights", FALSE)    
        }
        else {
            U.printBanner("Doing model without weights", FALSE)
        }
        
        dat_tech %>%
            E.trainModel(i, FALSE, weights_with_time = use_weights) %>%
            saveModel(i)
        gc()
        NULL
    }
    prepareModel <- function(i) 
        U.try(prepareModel_Try)(i)
   
    ####################################################################################################
    ### Script
    ####################################################################################################
    strat_id_list %>% 
        lapply(prepareModel)

}
V.quickBacktestAnalysis <-
function (strat_id_list) 
{
    ####################################################################################################
    ### Script parameters
    ####################################################################################################
    min_proba_diff <- 0.05# 0.06
    min_proba_signal <- 0.37#0.425
    min_proba_signal_plus_flat <- 0.75#0.75
    fees_per_trade <- 0/100000
    
    min_date <- as.Date("2000-01-01")
    
    limited_training_set <- c(
        'A50CNY', 'AEXEUR', 'ASXAUD', 'ATXEUR', 'AUDCAD', 
        'AUDCHF', 'AUDJPY', 'AUDNZD', 'AUDUSD', 'BELEUR', 'BMVMXN', 'BVPBRL', 
        'CADJPY', 'CHFJPY', 'CHFSEK', 'CSICNY', 'DAXEUR', 
        'DJIUSD', 'DJNNZD', 'EURAUD', 'EURCAD', 'EURCHF', 'EURCZK', 'EURDKK', 'EURGBP', 'EURHUF', 
        'EURJPY', 'EURNOK', 'EURNZD', 'EURPLN', 'EURSEK', 'EURUSD', 
        'FTSGBP', 'GBPAUD', 'GBPCAD', 'GBPCHF', 
        'GBPJPY', 'GBPNZD', 'GBPSEK', 'GBPUSD', 'HSIHKD', 'IBXEUR',
        'JPYKRW', 'KSPKRW', 'MIBEUR', 'NDQUSD', 
        'NDXUSD', 'NKYJPY', 'NZDCAD', 'NZDCHF', 'NZDJPY', 'NZDUSD', 'OMHEUR', 
        'OMXSEK', 'PSIEUR', 'PX1EUR', 'RUTUSD', 'SEKJPY', 'SMICHF', 'SPXUSD', 'SSECNY', 
        'STISGD', 'STXEUR', 'SZSCNY', 'TAIILS', 'TPXJPY', 'TSXCAD',
        'USDBRL', 'USDCAD', 'USDCHF', 'USDCLP', 'USDIDR', 'USDILS', 
        'USDINR', 'USDJPY', 'USDKRW', 'USDMXN', 'USDNOK', 'USDPHP', 'USDRUB', 'USDSEK', 
        'USDSGD', 'USDTRY', 'USDTWD', 'USDZAR', 'XAGUSD', 'XAUUSD', 'XPDUSD', 
        'XPTUSD', 'CHFNOK', 'GBPNOK', 'GBPPLN', 'EURRUB', 'NOKSEK', 
        'EEMUSD')
    
    limited_instrument_set <- A.getInstrumentId(limited_training_set)
 #   limited_instrument_set <- INSTRUMENTS$instrument_id
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    path_backtest <- paste0(DIRECTORY_DATA_HD, "/Backtestings/")
    file_name_strat <- paste0(path_backtest, "backtest_full_strat_%s.csv")
    file_name_strat_w <- gsub(".csv", "_weights.csv", file_name_strat)
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    readStrategyData <- function(strat_id) {
        dat_w <- file_name_strat_w %>%
            sprintf(strat_id) %>% 
            U.read.csv %>%
            mutate(predict_w = predict) %>%
            select(date, instrument_id, predict_w)
        
        file_name_strat %>%
            sprintf(strat_id) %>%
            U.read.csv %>%
            left_join(dat_w, by = c("date", "instrument_id")) %>%
            left_join(select(INSTRUMENTS, instrument_id, use_for_trading_ib), by = "instrument_id") %>%
            filter(predict %in% c("up", "down")) %>%
     #       filter(predict %in% "up") %>%
            mutate(
                strategy_id = strat_id,
                buy_sell = (predict == "up") - (predict == "down"),
                px_exit = px_exit * (1 - buy_sell * fees_per_trade),
                px_entry = close * (1 + buy_sell * fees_per_trade),
                gross_pnl = buy_sell * (px_exit - px_entry) / (t_up - close),
                proba_antisignal = 1 - proba_signal_plus_flat,
                proba_diff = proba_signal - proba_antisignal
            ) %>%
            filter(
                date >= min_date,
                instrument_id %in% limited_instrument_set,
                (use_for_trading_ib == 1) ,
                (predict == predict_w) ,
                (proba_signal >= min_proba_signal) |
                    (proba_diff >= min_proba_diff) | 
                    (proba_signal_plus_flat >= min_proba_signal_plus_flat)
            ) %>%
            select(
                strat_id, date, instrument_id, 
                predict, gross_pnl, proba_signal, proba_diff, proba_signal_plus_flat
            )
    }
    
    calcQuickPnL <- function(dat) {
        dat_0 <- dat %>% 
            group_by(date) %>%
            summarize(
                N = n(), 
                gross_pnl = mean(gross_pnl), 
                nav_mult = 1 + 0.01 * gross_pnl * pmin(N, 10)
            ) %>% 
            ungroup %>%
            mutate(
                nav = cumprod(nav_mult),
                year = year(date),
                nav_prev = lag(nav, 1, default = 1),
                max_so_far = cummax(nav),
                drawdown = -(nav / max_so_far - 1),
                max_drawdown = cummax(drawdown),
                pnl_x_N = gross_pnl * N
                ) %>%
            group_by(year) %>%
            summarize(
                N = sum(N),
                low_ytd = pmin(0, (min(nav) / first(nav_prev) - 1)),
                nav = last(nav),
                gross_pnl = sum(gross_pnl) / N,
                rtn = round(100 * (nav / first(nav_prev) - 1), 2),
                max_drawdown = 100 * last(max_drawdown),
                log10_nav = log10(nav)
            ) %>% 
            data.frame %>%
            mutate(
                nav = round(nav, 2), 
                gross_pnl = round(gross_pnl, 3), 
                low_ytd = round(low_ytd, 2),
                max_drawdown = round(max_drawdown, 2)
                ) %>% 
            select(year, N, nav, log10_nav, gross_pnl, rtn, low_ytd, max_drawdown)
        
        start_date <- as.Date(paste0(dat_0$year[1], "-01-01"))
        end_date <- as.Date(paste0(tail(dat_0$year, 1), "-12-31"))
        n_years <- U.yearFrac(start_date, end_date)

        
        dat_1 <- dat %>% 
            group_by(date) %>%
            summarize(
                N = n(), 
                gross_pnl = mean(gross_pnl), 
                nav_mult = 1 + 0.01 * gross_pnl * pmin(N, 10)
            ) %>% 
            ungroup %>%
            mutate(
                nav =  cumprod(nav_mult),
                year = year(date),
                nav_prev = lag(nav, 1, default = 1),
                max_so_far = cummax(nav),
                drawdown = -(nav / max_so_far - 1),
                max_drawdown = 100 * cummax(drawdown)
                ) %>%
            summarize(
                N = sum(N),
                nav = last(nav),
                gross_pnl = mean(gross_pnl),
                rtn = round(100 * ((nav / first(nav_prev)) ^ (1 / n_years) - 1), 2),
                low_ytd = NUM_NA,
                max_drawdown = last(max_drawdown),
                log10_nav = log10(nav)
            ) %>% 
            data.frame %>%
            mutate(
                year = "Total", 
                nav = round(nav, 2), 
                gross_pnl = round(gross_pnl, 2), 
                low_ytd = round(low_ytd, 2),
                max_drawdown = round(max_drawdown, 2)
                ) %>%
            select(year, N, nav, log10_nav, gross_pnl, rtn, low_ytd, max_drawdown) 
        rbind(dat_0, dat_1)
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    strat_id_list %>%
        lapply(readStrategyData) %>%
        bind_rows %>%
        calcQuickPnL
    
}
V.readBacktest <-
function (strats_list = NULL) 
{
    time_start <- Sys.time()
    ####################################################################################################
    ### Script Parameters
    ####################################################################################################
    #   leverage <- 1
    
    if (is.null(strats_list)) {
        nb_strategies <- 14
        strats_list <- 1:nb_strategies
    }
    else {
        nb_strategies <- length(strats_list)
    }
    
    min_proba_diff <- 0#0.05
    min_proba_signal <- 0#0.37
    min_proba_signal_plus_flat <- 0#0.75
    
    start_date <- as.Date("2000-01-01")
    end_date <- as.Date("2025-04-20")
    
    pnl_tp_usd <- 0.01
    initial_nav <- 100
    
    flat_fee_per_trade <-  2.5 / 100000
    
    capital_maximum_allocation_per_day <- 0.1
    max_capital_per_trade_vs_nav = 5
    
    transaction_cost_vs_target_threshold <- 0.005
    stop_loss_slippage_vs_bid_offer <- 1
    
    path_backtest <- paste0(DIRECTORY_DATA_HD, "Backtestings/")
    file_name_normal <- "%sbacktest_full_strat_%s.csv"
    
    tradable_instruments <- filter(INSTRUMENTS, use_for_trading_ib == 1)$instrument_id
    
    cost_of_carry_pct_per_annum <- 5
    
    limited_training_set <- c(
        'A50CNY', 'AEXEUR', 'ASXAUD', 'ATXEUR', 'AUDCAD', 
        'AUDCHF', 'AUDJPY', 'AUDNZD', 'AUDUSD', 'BELEUR', 'BMVMXN', 'BVPBRL', 
        'CADJPY', 'CHFJPY', 'CHFSEK', 'CSICNY', 'DAXEUR', 
        'DJIUSD', 'DJNNZD', 'EURAUD', 'EURCAD', 'EURCHF', 'EURCZK', 'EURDKK', 'EURGBP', 'EURHUF', 
        'EURJPY', 'EURNOK', 'EURNZD', 'EURPLN', 'EURSEK', 'EURUSD', 
        'FTSGBP', 'GBPAUD', 'GBPCAD', 'GBPCHF', 
        'GBPJPY', 'GBPNZD', 'GBPSEK', 'GBPUSD', 'HSIHKD', 'IBXEUR',
        'JPYKRW', 'KSPKRW', 'MIBEUR', 'NDQUSD', 
        'NDXUSD', 'NKYJPY', 'NZDCAD', 'NZDCHF', 'NZDJPY', 'NZDUSD', 'OMHEUR', 
        'OMXSEK', 'PSIEUR', 'PX1EUR', 'RUTUSD', 'SEKJPY', 'SMICHF', 'SPXUSD', 'SSECNY', 
        'STISGD', 'STXEUR', 'SZSCNY', 'TAIILS', 'TPXJPY', 'TSXCAD',
        'USDBRL', 'USDCAD', 'USDCHF', 'USDCLP', 'USDIDR', 'USDILS', 
        'USDINR', 'USDJPY', 'USDKRW', 'USDMXN', 'USDNOK', 'USDPHP', 'USDRUB', 'USDSEK', 
        'USDSGD', 'USDTRY', 'USDTWD', 'USDZAR', 'XAGUSD', 'XAUUSD', 'XPDUSD', 
        'XPTUSD', 'CHFNOK', 'GBPNOK', 'GBPPLN', 'EURRUB', 'NOKSEK', 
        'EEMUSD')
    
    limited_instrument_set <- A.getInstrumentId(limited_training_set)
    #limited_instrument_set <- INSTRUMENTS$instrument_id
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    file_names_normal <- strats_list %>%
        U.sapply(function(i) sprintf(file_name_normal, path_backtest, i))
    file_names_weights <- gsub(".csv", "_weights.csv", file_names_normal)
    
    cols_to_keep <- c(
        "instrument_id", "date", "predict", "t_up", "t_dn", 
        "tgt", "close", "px_exit", "date_exit", "duration",
        "proba_signal", "proba_signal_plus_flat")
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    readFiles_i <- function(i, use_weights) {
        file_names <- file_names_normal
        if (use_weights) {
            file_names <- file_names_weights
        }
        i_pos <- which(strats_list == i)
        file_names[i_pos] %>% 
            U.read.csv %>% 
            select(cols_to_keep) %>%
            distinct(instrument_id, date, .keep_all = TRUE) %>%
            mutate(strategy = i) %>%
            filter(
                instrument_id %in% tradable_instruments,
                instrument_id %in% limited_instrument_set,
                date >= start_date,
                date <= end_date
            )
    }
    
    readFilesNormal_i <- function(i) readFiles_i(i, FALSE)
    readFilesWeights_i <- function(i) readFiles_i(i, TRUE)
    
    readFiles <- function(use_weights) {
        readFunction <- readFilesNormal_i
        if (use_weights)
            readFunction <- readFilesWeights_i
        strats_list %>%
            lapply(readFunction) %>%
            bind_rows %>%
            left_join(select(INSTRUMENTS, instrument_id, pair), by = "instrument_id") %>%
            rename(
                px_entry = close,
                date_entry = date 
            ) %>%
            mutate(
                ccy_1 = substr(pair, 1, 3),
                ccy_2 = substr(pair, 4, 6),
                buy_sell = case_when(
                    predict == "up" ~ 1,
                    predict == "down" ~ -1,
                    predict == "flat" ~ 0
                ),
                proba_antisignal = 1 - proba_signal_plus_flat,
                proba_diff = proba_signal - proba_antisignal
            ) %>%
            select(
                strategy, pair, ccy_1, ccy_2, date_entry, buy_sell, 
                px_entry, date_exit, px_exit, t_up, duration, tgt,
                proba_signal, proba_antisignal, proba_diff
            )
    }
    
    readFilesNormal <- function() readFiles(FALSE)
    readFilesWeights <- function() {
        readFiles(TRUE) %>%
            select(
                strategy, pair, date_entry, buy_sell, proba_signal, proba_antisignal, proba_diff
            ) %>% 
            rename(
                proba_signal_w = proba_signal, 
                proba_antisignal_w = proba_antisignal,
                proba_diff_w = proba_diff
            )
    }
    
    prepareBacktestData <- function() {
        dat_normal <- readFilesNormal()
        dat_weights <- readFilesWeights()
        dat_normal %>%
            semi_join(dat_weights, by=c("strategy", "pair", "date_entry", "buy_sell")) %>%
            left_join(dat_weights, by=c("strategy", "pair", "date_entry", "buy_sell")) #%>%
        #   filter(
        #        buy_sell == 1,
        #        (pmin(proba_signal, proba_signal_w) >= min_proba_signal) | 
        #            (pmin(proba_diff, proba_diff_w) >= min_proba_diff) | 
        #            (pmax(proba_antisignal, proba_antisignal_w) <= 1-min_proba_signal_plus_flat)
                
                #    (proba_signal >= min_proba_signal) | 
                #        (proba_diff >= min_proba_diff) | 
            #        (proba_antisignal <= 1-min_proba_signal_plus_flat)
        #    )
    }
    
    addNbTradesPerDay <- function(dat_backtest) {
        dat_per_day <- dat_backtest %>% 
            group_by(date_entry) %>% 
            summarize(n_trades_today = n()) %>%
            ungroup;
        dat_backtest %>% 
            left_join(dat_per_day, by = "date_entry")
    }
    
    prepareHistoPX <- function() {
        "SELECT A.code AS ccy, F.date, F.fx
        FROM histo_fx_close_vs_usd F
        LEFT JOIN static_asset A ON A.asset_id = F.asset_id
        WHERE F.date >= '%s'" %>% 
            sprintf(min_date) %>%
            D.select
    }
    
    prepareTradesList <- function(dat_backtest, dat_fx) {
        dat_backtest %>% 
            filter(buy_sell %in% c(1, -1)) %>%
            left_join(
                rename(dat_fx, ccy_1 = ccy, date_entry = date), 
                by = c("ccy_1", "date_entry")
            ) %>% 
            mutate(
                fx = na.locf0(fx, fromLast = TRUE),
                tp_pct = t_up / px_entry - 1,
                pnl_tgt_ccy_1 = pnl_tp_usd / fx, 
                notional_ccy_1 = buy_sell * pnl_tgt_ccy_1 / tp_pct, 
                notional_ccy_1 = pmin(notional_ccy_1, max_capital_per_trade_vs_nav),
                notional_ccy_2 = -notional_ccy_1 * px_entry,
                trd_outcome = case_when(
                    ((buy_sell == 1) & (tgt == "up")) ~ "target",
                    ((buy_sell == -1) & (tgt == "down")) ~ "target",
                    ((buy_sell == 1) & (tgt == "down")) ~ "stop",
                    ((buy_sell == -1) & (tgt == "up")) ~ "stop",
                    (tgt == "flat") ~ "matured",
                    TRUE ~ "undetermined"
                )
            ) %>%
            arrange(date_entry, pair, date_exit) %>% 
            select(
                strategy,
                pair,
                ccy_1,
                ccy_2,
                date_entry,
                buy_sell,
                px_entry,
                date_exit,
                px_exit,
                notional_ccy_1,
                notional_ccy_2,
                trd_outcome
            )
    }
    
    preparePnLTable <- function() {
        pnl <- data.frame(
            date = date_list,
            nav_morning = NUM_NA,
            nav_evening = NUM_NA,
            risk_morning = 0, risk_evening = 0,
            pnl = 0, pnl_new = 0, pnl_live = 0, pnl_exit = 0,
            n_new = 0, n_live = 0, n_exit = 0, 
            risk_new = 0, risk_live = 0, risk_exit = 0,
            fees = 0, fees_new = 0, fees_live = 0, fees_exit = 0,
            max_so_far = 0, drawdown = 0, max_drawdown = 0
        ) %>%
            U.dataFrame;
        for (i in strats_list) {
            pnl_i <- data.frame(
                date = date_list,
                nav_morning = 0, nav_evening = 0, 
                pnl = 0, pnl_new = 0, pnl_live = 0, pnl_exit = 0,
                n_new = 0, n_live = 0, n_exit = 0, 
                fees = 0, fees_new = 0, fees_live = 0, fees_exit = 0
            ) %>%
                U.dataFrame %>%
                select(-date);
            colnames(pnl_i) <- paste0(colnames(pnl_i), "_", i);
            pnl <- cbind(pnl, pnl_i);
        }
        pnl$nav_morning[1] <- initial_nav;
        pnl$nav_evening[1] <- initial_nav;
        for (j in strats_list) {
            pnl[1, paste0("nav_morning_", j)] <- initial_nav;
            pnl[1, paste0("nav_evening_", j)] <- initial_nav;
        }
        pnl;
    }
    
    initializeDailyMorningValues <- function(i, pnl) {
        pnl$nav_morning[i] <- pnl$nav_evening[i-1];
        pnl$risk_morning[i] <- pnl$risk_evening[i-1];
        
        for (j in strats_list) {
            pnl[i,paste0("nav_morning_", j)] <- pnl[i-1, paste0("nav_evening_", j)];
        }
        pnl;
    }
    
    preparePxYesterday_Try <- function(px_i) {
        px_i %>% 
            rename(px_t_1 = px_t);
    }
    preparePxYesterday <- function(px_i)
        U.tryNull(preparePxYesterday_Try, px_i)
    
    preparePxToday <- function(date_i) {
        dat_px %>%
            filter(date == date_i) %>%
            select(-date) %>%
            rename(px_t = fx);
    }
    
    findNotionalsForExistingTrade <- function(i, trd_live, pnl_per_day) {
        this_trd <- trd_live[i,];
        this_trd_date_entry <- this_trd$date_entry;
        date_tag <- paste0("date_", format(this_trd_date_entry, "%Y%m%d"))
        trd_new_that_day <- pnl_per_day[[date_tag]]$trd_new %>%
            select(strategy, pair, buy_sell, notional_ccy_1, notional_ccy_2);
        this_trd %>%
            left_join(trd_new_that_day, by = c("strategy", "pair", "buy_sell")) %>%
            head(1) %>%
            select(strategy, pair, date_entry, buy_sell, notional_ccy_1, notional_ccy_2);
    }
    
    calcPnLTradesLive <- function(pnl, date_i, px_i, px_i_1, nav_morning, pnl_per_day) {
        trd_live <- dat_trd %>% 
            filter(
                date_entry < date_i, 
                date_exit >= date_i
            ) %>%
            select(-notional_ccy_1, -notional_ccy_2);
        
        if (U.dfContainsData(trd_live)) {
            
            findNotionalsForExistingTrade_local <- function(i) 
                findNotionalsForExistingTrade(i, trd_live, pnl_per_day)
            
            trd_notionals <- 1:nrow(trd_live) %>%
                lapply(findNotionalsForExistingTrade_local) %>%
                bind_rows;
            
            trd_live <- trd_live %>%
                left_join(rename(px_i, ccy_1 = ccy, px_1_t = px_t), by = "ccy_1") %>% 
                left_join(rename(px_i, ccy_2 = ccy, px_2_t = px_t), by = "ccy_2") %>% 
                left_join(rename(px_i_1, ccy_1 = ccy, px_1_t_1 = px_t_1), by = "ccy_1") %>% 
                left_join(rename(px_i_1, ccy_2 = ccy, px_2_t_1 = px_t_1), by = "ccy_2") %>% 
                left_join(trd_notionals, by = c("strategy", "pair", "date_entry", "buy_sell")) %>%
                #   left_join(fees_per_asset, by = "pair") %>%
                mutate(
                    npv_usd_1_t = notional_ccy_1 * px_1_t,
                    npv_usd_1_t_1 = notional_ccy_1 * px_1_t_1,
                    npv_usd_2_t = notional_ccy_2 * px_2_t,
                    npv_usd_2_t_1 = notional_ccy_2 * px_2_t_1,
                    npv_usd_t = npv_usd_1_t + npv_usd_2_t,
                    npv_usd_t_1 = npv_usd_1_t_1 + npv_usd_2_t_1,
                    pnl_usd = npv_usd_t - npv_usd_t_1,
                    fees_carry = cost_of_carry_pct_per_annum * npv_usd_1_t / (365 * 100),
                    fees = fees_carry,
                )
            
            for (j in 1:nb_strategies) {
                pnl[i,paste0("n_live_", j)] <- nrow(filter(trd_live, strategy == j));
                pnl[i,paste0("pnl_live_", j)] <- sum(filter(trd_live, strategy == j)$pnl_usd);
                pnl[i,paste0("fees_live_", j)] <- 0;
            }
            
            pnl$n_live[i] <- nrow(trd_live);
            pnl$pnl_live[i] <- sum(trd_live$pnl_usd);
            pnl$risk_live[i] <- 0;
            pnl$fees_live[i] <- sum(trd_live$fees);
        }
        list(pnl = pnl, trd_live = trd_live);
    }
    
    calcPnLTradesExit <- function(pnl, date_i, px_i, px_i_1, nav_morning, pnl_per_day) {
        trd_exit <- dat_trd %>% 
            filter(date_exit == date_i) %>%
            select(-notional_ccy_1, -notional_ccy_2);
        
        if (U.dfContainsData(trd_exit)) {
            findNotionalsForExistingTrade_local <- function(i) 
                findNotionalsForExistingTrade(i, trd_exit, pnl_per_day)
            
            trd_notionals <- 1:nrow(trd_exit) %>%
                lapply(findNotionalsForExistingTrade_local) %>%
                bind_rows;
            
            trd_exit <- trd_exit %>%
                left_join(rename(px_i, ccy_1 = ccy, px_1_t = px_t), by = "ccy_1") %>% 
                left_join(rename(px_i, ccy_2 = ccy, px_2_t = px_t), by = "ccy_2") %>% 
                left_join(trd_notionals, by = c("strategy", "pair", "date_entry", "buy_sell")) %>%
                #    left_join(fees_per_asset, by = "pair") %>%
                mutate(
                    npv_usd_1_t = notional_ccy_1 * px_1_t,
                    npv_usd_2_t = notional_ccy_2 * px_2_t,
                    npv_usd_1_exit = notional_ccy_1 * px_1_t,
                    npv_usd_2_exit = notional_ccy_2 / px_exit * px_1_t,
                    npv_usd_exit = (notional_ccy_1 + notional_ccy_2 * px_exit) * px_1_t,
                    pnl_usd = (npv_usd_2_exit - npv_usd_2_t),
                    fees_brokerage = 0, #brokerage_per_100k * abs(npv_usd_1_t) / 100000,
                    fees_bid_offer = 0, #bid_offer_cost_per_100k * abs(npv_usd_1_t) / 100000,
                    fees_stop_loss = 0, #stop_loss_slippage_vs_bid_offer * fees_bid_offer * 
                    #(trd_outcome == "stop"),
                    flat_fee = flat_fee_per_trade * abs(npv_usd_1_t),
                    fees = fees_brokerage + fees_bid_offer + fees_stop_loss + flat_fee
                ) 
            
            for (j in strats_list) {
                pnl[i,paste0("n_exit_", j)] <- nrow(filter(trd_exit, strategy == j));
                pnl[i,paste0("pnl_exit_", j)] <- sum(filter(trd_exit, strategy == j)$pnl_usd);
                pnl[i,paste0("fees_exit_", j)] <- sum(filter(trd_exit, strategy == j)$fees);
            }
            
            pnl$n_exit[i] <- nrow(trd_exit);
            pnl$pnl_exit[i] <- sum(trd_exit$pnl_usd);
            pnl$risk_exit[i] <- -pnl$n_exit[i] * pnl_tp_usd;
            pnl$fees_exit[i] <- sum(trd_exit$fees);
            
        }
        list(pnl = pnl, trd_exit = trd_exit);
    }
    
    calcPnLTradesNew <- function(pnl, date_i, px_i, px_i_1, nav_morning) {
        trd_new <- dat_trd %>%
            filter(date_entry == date_i);
        
        if (U.dfContainsData(trd_new)) {
            
            nb_trades <- nrow(trd_new)    
            capital_allocation_ratio <- 100 * capital_maximum_allocation_per_day /
                pmax(100 * capital_maximum_allocation_per_day, nb_trades);
            
            trd_new <- trd_new %>%
                left_join(rename(px_i, ccy_1 = ccy, px_1_t = px_t), by = "ccy_1") %>% 
                left_join(rename(px_i, ccy_2 = ccy, px_2_t = px_t), by = "ccy_2") %>%
                #    left_join(fees_per_asset, by = "pair") %>%
                mutate(
                    notional_ccy_1 = notional_ccy_1 * nav_morning * capital_allocation_ratio,
                    notional_ccy_2 = notional_ccy_2 * nav_morning * capital_allocation_ratio,
                    npv_usd_1_t = notional_ccy_1 * px_1_t,
                    npv_usd_2_t = notional_ccy_2 * px_2_t,
                    fees_brokerage = 0, #brokerage_per_100k * abs(npv_usd_1_t) / 100000,
                    fees_bid_offer = 0, #bid_offer_cost_per_100k * abs(npv_usd_1_t) / 100000,
                    flat_fee = flat_fee_per_trade * abs(npv_usd_1_t),
                    fees = fees_brokerage + fees_bid_offer + flat_fee
                ) 
            
            
            for (j in 1:nb_strategies) {
                pnl[i,paste0("n_new_", j)] <- nrow(filter(trd_new, strategy == j));
                pnl[i,paste0("pnl_new_", j)] <- sum(filter(trd_new, strategy == j)$pnl_usd);
                pnl[i,paste0("fees_new_", j)] <- sum(filter(trd_new, strategy == j)$fees);
            }
            
            pnl$n_new[i] <- nrow(trd_new);
            pnl$pnl_new[i] <- 0;
            
            pnl$risk_new[i] <- pnl$n_new[i] * pnl_tp_usd;
            pnl$fees_new[i] <- sum(trd_new$fees);
        }
        list(pnl = pnl, trd_new = trd_new);
    }
    
    wrapUpDailyValues <- function(pnl, i, date_i) {
        for (j in strats_list) {
            pnl[i,paste0("fees_", j)] <- 
                pnl[i,paste0("fees_new_", j)] + 
                pnl[i,paste0("fees_live_", j)] +
                pnl[i,paste0("fees_exit_", j)];
            
            pnl[i,paste0("pnl_", j)] <- 
                pnl[i,paste0("pnl_new_", j)] + 
                pnl[i,paste0("pnl_live_", j)] +
                pnl[i,paste0("pnl_exit_", j)] - 
                pnl[i,paste0("fees_", j)];
            
            pnl[i,paste0("nav_evening_", j)] <- 
                pnl[i,paste0("nav_morning_", j)] + pnl[i,paste0("pnl_", j)];
        }
        
        pnl$fees[i] <- pnl$fees_new[i] + pnl$fees_live[i] + pnl$fees_exit[i];
        
        pnl$risk_evening[i] <- pnl$risk_morning[i] + 
            pnl$risk_new[i] +
            pnl$risk_live[i] + 
            pnl$risk_exit[i];
        
        pnl$pnl[i] <- pnl$pnl_new[i] + pnl$pnl_live[i] + pnl$pnl_exit[i] - pnl$fees[i];
        
        pnl$nav_evening[i] <- pnl$nav_morning[i] + pnl$pnl[i];
        
        if (is.na(pnl$nav_evening[i]) & !is.na(pnl$nav_evening[i-1])) {
            print(pnl[(i-5):i,] %>% data.frame)
        }
        
        plotAnnualPnLProgression(pnl, i, date_i);
        
        pnl;
    }
    
    plotAnnualPnLProgression <- function(pnl, i, date_i) {
        if ((substr(date_i, 6, 10) == "12-31") | (i == nrow(pnl))) {
            U.printBanner(date_i, FALSE);
            #    print(pnl %>% filter(date == date_i) %>% data.frame)
            print(
                pnl %>% 
                    ggplot(aes(x = date, y = nav_evening)) + 
                    geom_step(color = "lightseagreen") +
                    geom_vline(
                        xintercept = as.Date("1980-01-01") %m+% years(seq(0, 100, 5)), 
                        color = "black", 
                        alpha = 0.5, 
                        size = 0.25, 
                        linetype = "dashed"
                    ) + 
                    scale_y_continuous(labels = comma, trans='log10') + 
                    scale_x_date(date_breaks = "1 years", date_labels = "%y") +
                    theme(
                        axis.title = element_blank(),
                        legend.position = "bottom"
                    )
            )
        }
    }
    
    updatePnLPerDay <- function(pnl_per_day, trd_live, trd_new, trd_exit) {
        pnl_per_day[[paste0("date_", format(date_i, "%Y%m%d"))]] <- list(
            trd_live = dat_live$trd_live, trd_new = dat_new$trd_new, trd_exit = dat_exit$trd_exit
        );
        pnl_per_day;
    }
    
    finalComputations <- function(pnl) {
        pnl %>%
            mutate(
                max_so_far = cummax(nav_evening),
                drawdown = (nav_evening < max_so_far) * (nav_evening / max_so_far - 1),
                max_drawdown = -cummin(drawdown)
            )
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    dat_backtest <- prepareBacktestData() %>% U.debug(1)
    
    min_date <- start_date
    date_list <- seq(min_date, end_date, 1)
    date_list_extra <- seq(min_date - 14, end_date, 1)
    
    pnl <- preparePnLTable()
    
    dat_px <- prepareHistoPX()
    dat_trd <- prepareTradesList(dat_backtest, dat_px)
    px_i <- NULL;
    pnl_per_day <- {}
    for (i in 2:nrow(pnl)) {
        
        date_i <- pnl$date[i]
        pnl <- initializeDailyMorningValues(i, pnl);
        px_i_1 <- preparePxYesterday(px_i);
        px_i <- preparePxToday(date_i);
        nav_morning <- pnl$nav_morning[i];
        
        dat_live <- calcPnLTradesLive(pnl, date_i, px_i, px_i_1, nav_morning, pnl_per_day);
        
        dat_exit <- calcPnLTradesExit(dat_live$pnl, date_i, px_i, px_i_1, nav_morning, pnl_per_day);
        
        dat_new <- calcPnLTradesNew(dat_exit$pnl, date_i, px_i, px_i_1, nav_morning);
        
        pnl <- wrapUpDailyValues(dat_new$pnl, i, date_i);
        pnl_per_day <- updatePnLPerDay(pnl_per_day);
        
    }
    
    pnl <- finalComputations(pnl);
    
    dat_summary <- pnl %>% 
        mutate(year = year(date)) %>%
        group_by(year) %>%
        mutate(
            max_so_far = cummax(nav_evening),
            drawdown = (nav_evening < max_so_far) * (nav_evening / max_so_far - 1),
            day_rtn = log(nav_evening / nav_morning)
        ) %>%
        summarize(
            nb_trades = sum(n_new),
            rtn = 100 * (last(nav_evening) / first(nav_morning) - 1),
            max_drawdown = -100 * min(drawdown),
            lowest_ytd = 100 * (min(nav_evening) / first(nav_morning) - 1),
            volatility = 100 * sd(day_rtn) * sqrt(365)
        ) %>% 
        ungroup %>% 
        mutate(
            drawdown_ratio = rtn / max_drawdown, 
            sharpe = rtn / volatility
        ) 
    
    total_years = U.yearFrac(pnl$date[1], tail(pnl$date, 1))
    
    dat_summary_total <- pnl %>% 
        mutate(
            year = "Total",
            max_so_far = cummax(nav_evening),
            drawdown = (nav_evening < max_so_far) * (nav_evening / max_so_far - 1),
            day_rtn = log(nav_evening / nav_morning)
        ) %>%
        summarize(
            year = last(year),
            nb_trades = sum(n_new),
            rtn = 100 * ((last(nav_evening) / first(nav_morning)) ^ (1 / total_years) - 1),
            max_drawdown = -100 * min(drawdown),
            lowest_ytd = 100 * (min(nav_evening) / first(nav_morning) - 1),
            volatility = 100 * sd(day_rtn) * sqrt(365)
        ) %>% 
        ungroup %>% 
        mutate(
            drawdown_ratio = rtn / max_drawdown, 
            sharpe = rtn / volatility
        ) 
    
    dat_summary <- rbind(dat_summary, dat_summary_total)
    
    print(dat_summary, n = 50)
    
    
    dat_backtest <- list(
        pnl = pnl, 
        pnl_per_day = pnl_per_day, 
        data = dat_backtest,
        summary = dat_summary
    );
  #  save(dat_backtest, file = sprintf("backtest_%s_new.RData", "fees_2"));
    dat_backtest
    
    
    
}
V.readBumpedPredicts <-
function () 
{
    ####################################################################################################
    ### Script Parameters
    ####################################################################################################
    bump_list <- c(
        0, 
        -150, -100, -75, -50, -40, -30, -20, -10, -5, 
        5, 10, 20, 30, 40, 50, 75, 100, 150
        );
    bump_file_name <- "%sMount/Glenorchy_Fundamentals/Models_FX/predict_bump_%s.RData";
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    readPredict_Try <- function(this_bump) {
        load(sprintf(bump_file_name, DIRECTORY_ROOT, this_bump));
        
        dat_run_bump$summary_2 %>%
            select(pair) %>%
            mutate(bump = this_bump) %>%
            left_join(dat_run_bump$summary_2, by = "pair") %>%
            left_join(dat_run_bump$summary_3, by = "pair");
    }
    readPredict <- function(this_bump) 
        U.tryNull(readPredict_Try, this_bump)
    ####################################################################################################
    ### Script Parameters
    ####################################################################################################
    
    bump_list %>%
        lapply(readPredict) %>%
        bind_rows %>%
        arrange(pair, bump);
    
}
V.removeUselessFeatures <-
function (strat_id, starting_features = "") 
{
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    n_keep_features <- 120
    time_start <- Sys.time()
    nb_trees <- 2000
    
    file_name <- sprintf("%sFeatures/features_%s.RData", DIRECTORY_DATA_HD, strat_id)
    
    features <- D.loadTableLocal("feature")
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    nbFeaturesToRemove <- function(n) floor (1 + 30 * (((n / n_keep_features) - 0.8) ** 2))
    
    prepareTableUpdate <- function(res) {
        feature_in_model_ids <- filter(features, feature %in% res$features_in_model)$feature_id
        
        feature_configuration_ids <- "SELECT * FROM strategy_feature 
        WHERE strategy_id = %s 
        AND market_configuration = 1" %>%
            sprintf(strat_id) %>%
            D.select %>% 
            .$feature_id
        
        features_old <- "SELECT feature_id FROM strategy_feature 
        WHERE strategy_id = %s 
        AND train = 1" %>%
            sprintf(strat_id) %>%
            D.select %>% 
            left_join(features, by="feature_id") %>% 
            .$feature
        
        all_feature_ids <- c(feature_in_model_ids, feature_configuration_ids) %>% 
            unique
        
        dat <- data.frame(
            strategy_id = strat_id,
            feature_id = all_feature_ids,
            train = 0,
            market_configuration = 1
        ) %>% 
            U.data2Tibble
        dat$train[dat$feature_id %in% feature_in_model_ids] <- 1
        dat$market_configuration[dat$market_configuration %in% feature_configuration_ids] <- 1
        
        sql_delete <- "DELETE FROM strategy_feature WHERE strategy_id = %s"
        res$dat_feature <- dat
        res$sql_delete <- sql_delete
        
        res$features_common_with_live <- intersect(features_old, features_in_model)
        res$features_new_not_in_old <- setdiff(features_in_model, features_old)
        res$features_old_not_in_new <- setdiff(features_old, features_in_model)
        res$features_old <- features_old
        
        res
    }
        
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    n_features_left <- 2000
    features_to_ignore <- starting_features
    while (n_features_left > n_keep_features) {
        U.printBanner(paste0(n_features_left, " remaining"))
        run_model <- V.testModel(strat_id, YESTERDAY, features_to_ignore, TRUE, nb_trees)
        print(run_model$model)
        print(confusionMatrix(
            data = factor(predict(run_model$model)),
            reference = factor(run_model$train$tgt), 
            mode = "prec_recall"
        ))

        n_features_left <- ncol(run_model$train)
        n_remove <- nbFeaturesToRemove(n_features_left)
        features_to_ignore <- features_to_ignore %>%
            c(head(names(run_model$model_varimp), n_remove)) %>%
            setdiff("") %>%
            U.vectorize %>%
            U.debug("Features Ignored")
        features_in_model <- names(run_model$model_varimp)
        rm(run_model)
        gc()
        save(features_to_ignore, file = file_name)
    }
    
    res = {}
    res$features_to_ignore <- features_to_ignore
    res$features_in_model <- features_in_model
    
    res <- prepareTableUpdate(res)
    
    U.printBanner(paste0(
        "Finished... Execution time: ", 
        round(as.numeric(difftime(Sys.time(), time_start, units = "mins")), 2),
        " minutes"
        ), FALSE)
    
    res

}
V.runLive <-
function (import_live = FALSE, import_model = FALSE, import_technicals = FALSE, 
          bumped_technicals = "no", strats_to_do = 1:length(VENTURA$strats),
          preloaded_model = NULL
          ) 
{
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    file_path_model <- paste0(DIRECTORY_DATA_SD, "Models/model_%s.RData");
    file_path_predict <- paste0(
        DIRECTORY_ROOT, "Mount/Glenorchy_Fundamentals/Models_FX/predict_latest.RData"
    );
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    prepareHistoPrices <- function() {
        U.printBanner("Preparing price history data...", FALSE);
        dat_histo_live <- dat_histo_ohlc;
        if (import_live) {
            load(paste0(DIRECTORY_DATA_HD, "Spot/Live/px_live_latest.RData"));
            dat_histo_live <- dat_live;
        }
        dat_histo_live;
    }
    
    prepareTechnicalsOnePair_Try <- function(fx_pair) {
        dat_histo %>% 
            filter(pair == fx_pair) %>%
            tail(
                1 +
                    VENTURA$technical_param$vol$nb_years_vol_lt_decile *
                    VENTURA$technical_param$vol$days_vol_lt
            ) %>%
            T.calcTechnicalsLive %>%
            filter(date >= YESTERDAY) %>%
            tail(1);
    }
    prepareTechnicalsOnePair <- function(fx_pair)
        U.try(prepareTechnicalsOnePair_Try)(fx_pair)
    
    prepareTechnicals <- function() {
        if (import_technicals) {
            U.printBanner("Importing precalculated technicals", FALSE);
            bump <- bumped_technicals;
            if (bumped_technicals == "no") {
                bump <- "latest";
            }
            file_name_tech <- "%sSpot/Live/technicals_live_%s.RData" %>%
                sprintf(DIRECTORY_DATA_HD, bump);
            load(file_name_tech);
            dat_histo_technicals <- dat_technicals;
        } 
        else {
            U.printBanner("Computing new technicals", FALSE);
            dat_histo_technicals <- PAIR_LIST %>%
                setdiff(VENTURA$assets$pairs_ignore) %>%
                lapply(prepareTechnicalsOnePair) %>% 
                bind_rows %>% 
                T.addAssetClass;;    
        }
        dat_histo_technicals;
    }
    
    prepareModel <- function(i) {
        if (import_model) {
            dat_trades_models <- preloaded_model;
            if (is.null(preloaded_model)) {
                U.printBanner(sprintf("Importing pre-trained model strategy %s ...", i), FALSE);
                load(sprintf(file_path_model, i));
                U.printBanner("Import model done...", FALSE);
            }
        }
        else {
            U.printBanner(sprintf("Training model strategy %s ...", i), FALSE);
            dat_trades_list <- E.prepareModelData();
            this_strat <- VENTURA$strats[[i]];
            features_to_ignore <- this_strat$model$features$features_to_ignore;
            
        #    U.printBanner(sprintf("First, with all features for strategy %s ...", i), FALSE);
        #    this_strat$model$features$features_to_ignore <- "";
        #    dat_trades_models <- E.trainModel(dat_trades_list, this_strat);
        #    U.printBanner("Saving model", FALSE);
        #    save(dat_trades_models, file = sprintf(file_path_model, paste0(i, "_all_features")));    
            
            U.printBanner(sprintf("Second, with top features only for strategy %s ...", i), FALSE);
            this_strat$model$features$features_to_ignore <- features_to_ignore;
            dat_trades_models <- E.trainModel(dat_trades_list, this_strat);
            dat_trades_models$base <- NULL;
            dat_trades_models$train <- NULL;
            dat_trades_models$test <- NULL;
            U.printBanner("Saving model", FALSE);
            save(dat_trades_models, file = sprintf(file_path_model, i));
            
            U.printBanner("Saving done...", FALSE);
        }
        dat_trades_models;
    }
    
    predictModel <- function(dat_histo_technicals, dat_trades_models) {
        U.printBanner("Starting model predict...", FALSE);
        E.modelPredict(dat_histo_technicals, dat_trades_models);
    }
    
    scoreTrades_Try <- function(dat, i) {
        this_strat <- VENTURA$strats[[i]];
        dat_score <- dat %>%
            E.genericScoreFunction(this_strat) %>%
            T.addTargetLevels(this_strat);
        U.printBanner(sprintf("Results for strategy %s: %s ...", i, this_strat$description));
        if (bumped_technicals == "no") {
            print(
                dat_score %>% 
                    select(
                        pair, date, close, 
                        score, score_high_enough,
                        predict, t_up, t_dn,
                        starts_with("score"), 
                        this_strat$market_configuration$features_for_score
                    ) %>%
                    arrange(-score) %>% 
                    mutate_if(is.double, round, 4) %>%
                    data.frame
            )
        }
        dat_score;
    }    
    
    scoreTrades <- function(dat, i) 
        U.try(scoreTrades_Try, NA)(dat, i);
    
    prepareTradesSummaryByStrategy <- function(dat) {
        dat %>% 
            arrange(-score) %>%
            select(pair, date, close, score, score_high_enough, predict, t_up, t_dn) %>% 
            U.data2Tibble %>%
            rename(px = close);
    }
    
    prepareTradesSummary2 <- function(dat_summary, dat_predict, i) {
        this_dat <- dat_predict %>%
            select(pair, score);
        colnames(this_dat)[2] <- paste0("score_", i);
        
        dat_summary %>% 
            left_join(this_dat, by = "pair");
    }
    
    prepareTradesSummary3 <- function(dat_summary, dat_predict, i) {
        this_dat <- dat_predict %>%
            select(pair, predict);
        colnames(this_dat)[2] <- paste0("predict_", i);
        
        dat_summary %>% 
            left_join(this_dat, by = "pair");
    }
    
    doOneStrategy <- function(i) {
        dat_trades_model <- prepareModel(i);
        dat_predict <- predictModel(dat_histo_technicals, dat_trades_model);
        dat_predict_with_score <- scoreTrades(dat_predict, i);
        dat_summary <- prepareTradesSummaryByStrategy(dat_predict_with_score);
        
        res <- list(
            #    base = dat_trades_model$base,
            #    set = dat_trades_model$set,
            #    train = dat_trades_model$train,
            #    test = dat_trades_model$test,
            predict = dat_predict_with_score, 
            summary = dat_summary
        );
        rm(dat_trades_model);
        gc();
        res;
    }
    
    whichScoreMax <- function(this_vector) { 
        pos_max <- U.vectorize(U.try(which.max, NA)(this_vector)) %>% 
            U.noData2Null; 
        if (is.null(pos_max)) {
            pos_max <- NA;
        } 
        else {
            pos_max <- paste0("score_", pos_max);
        }
        pos_max
    }
    
    finalizeTradesSummary2 <- function(dat_summary_2, dat_summary_3) {
        colnames(dat_summary_3) <- gsub("predict_", "score_", colnames(dat_summary_3));
        dat_summary_3 <- dat_summary_3 %>%
            gather(which_max_score, predict, -pair);
        
        dat_summary_2 %>% 
            rowwise(pair) %>%
            mutate(
                max_score = max(c_across(starts_with("score_")), na.rm = TRUE),
                which_max_score = whichScoreMax(c_across(starts_with("score_")))
            ) %>% 
            ungroup %>% 
            left_join(dat_summary_3, by = c("pair", "which_max_score")) %>% 
            left_join(select(dat_histo_technicals, pair, date, close), by = "pair") %>%
            select(pair, date, close, max_score, which_max_score, predict, starts_with("score_")) %>%
            U.dataFrame;
        
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    dat_histo <- prepareHistoPrices();
    
    dat_histo_technicals <- prepareTechnicals();
    
    dat_run <- list(
        histo = dat_histo,
        technicals = dat_histo_technicals
    );
    
    dat_summary_1 <- NULL;
    dat_summary_2 <- data.frame(pair = PAIR_LIST) %>% 
        U.data2Tibble;
    dat_summary_3 <- dat_summary_2;
    
    for (i in strats_to_do) {
        U.printBanner(sprintf("Starting Strategy %s", i));
        this_run <- doOneStrategy(i);
        dat_run[[paste0("strat_", i)]] <- this_run;
        this_summary_1 <- filter(this_run$summary, score_high_enough) %>%
            mutate(strategy = i) %>% 
            select(strategy, pair, date, px, predict, t_up, t_dn);
        dat_summary_1 <- dat_summary_1 %>% 
            rbind(this_summary_1);
        
        dat_summary_2 <- prepareTradesSummary2(dat_summary_2, this_run$predict, i);
        dat_summary_3 <- prepareTradesSummary3(dat_summary_3, this_run$predict, i);
        rm(this_run);
        gc();
    }
    
    fx_spot_vs_usd <- T.histoPXvsUSD(dat_histo, TRUE);
    
    dat_summary_1 <- dat_summary_1 %>%
        mutate(
            ccy = substr(pair, 1, 3), 
        ) %>% 
        left_join(fx_spot_vs_usd, by = "ccy") %>% 
        mutate(
            pnl_tgt_ccy_1 = 1000 / fx, 
            tp_pct = t_up / px - 1, 
            notional_ccy1 = pnl_tgt_ccy_1 / tp_pct, 
        ) %>% 
        rename(fx_ccy1 = fx) %>% 
        select(strategy, pair, px, predict, t_up, t_dn, tp_pct, fx_ccy1, notional_ccy1);
    
    dat_summary_2 <- finalizeTradesSummary2(dat_summary_2, dat_summary_3);
    
    dat_run$summary_1 <- dat_summary_1;
    dat_run$summary_2 <- dat_summary_2;
    dat_run$summary_3 <- dat_summary_3;
    
 #   save(dat_run, file = file_path_predict)
 #   new_file_name <- gsub("latest", U.dateTimeFormatForExport(), file_path_predict);
 #   file.copy(file_path_predict, new_file_name);
    if (bumped_technicals == "no") {
        U.printBanner("Saving predictions...", FALSE)
        U.printBanner("Predict summary even when signal is bad")
        print(data.frame(dat_summary_3));
        U.printBanner("Scores summary");
        print(dat_summary_2 %>% mutate_if(is.double, round, 4) %>% U.dataFrame);
        U.printBanner("Scores summary, same table but ordered by maximum score");
        print(dat_summary_2 %>% arrange(-max_score) %>% mutate_if(is.double, round, 4) %>%
                  U.dataFrame);
        U.printBanner("Trades summary");
        print(dat_summary_1 %>% mutate_if(is.double, round, 4) %>% U.dataFrame);
    }

    U.printBanner("All done...")
    dat_run;
    
    
    
}
V.save <-
function() {
    All_Objects <- ls(".GlobalEnv", pattern = "V.");
    All_Objects <- All_Objects[substr(All_Objects,1,2) == "V."];
    dump(All_Objects, paste0(DIRECTORY_CODE_HD, "Code/Ventura.R"));
  }
V.saveTradesNewToGithub <-
function (dat_run) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    winter_or_summer <- "Summer";
    dat_assets <- rename(select(ASSETS_FXDM, ticker, time_zone_trading)) %>% 
        rbind(select(ASSETS_FXEM, ticker, time_zone_trading)) %>%
        rbind(select(ASSETS_MTL, ticker, time_zone_trading)) %>%
        rbind(select(ASSETS_IDX, ticker, time_zone_trading))
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    determineRegion <- function() {
        hour_now <- U.hourTime();
        this_region <- CHAR_NA; 
        
        if (winter_or_summer == "Summer") {
            if (hour_now >= 20) this_region <- "Europe"
            else if (hour_now == 3) this_region <- "America"
            else if (hour_now == 4) this_region <- "America_FX"
            else if (hour_now == 13) this_region <- "Asia_Oceania_Japan"
            else if (hour_now == 14) this_region <- "Asia_China"
            else if (hour_now == 15) this_region <- "Asia_Hong_Kong"
            else if (hour_now == 16) this_region <- "Asia_Singapore"
        }
    #    if (winter_or_summer == "Winter") {
    #        if (hour_now >= 20) this_region <- "Europe"
    #        else if (hour_now <= 4) this_region <- "America"
    #        else if (hour_now <= 7) this_region <- "America_FX"
    #        else if (hour_now == 12) this_region <- "Oceania"
    #        else if (hour_now <= 13) this_region <- "Asia_Japan"
    #        else if (hour_now <= 16) this_region <- "Asia_China"
    #    }
        
        this_region;
    }
    
    
    
    determineRegionId <- function(this_region) {
        if (winter_or_summer == "Summer") {
            region_id <- switch(
                this_region, 
                "Asia_Oceania_Japan" = 1, 
                "Asia_China" = 2, 
                "Asia_Hong_Kong" = 3, 
                "Asia_Singapore" = 4, 
                "Europe" = 5, 
                "America" = 6,
                "America_FX" = 7
            );
        }
        if (winter_or_summer == "Winter") {
            region_id <- switch(
                this_region, 
                "Asia_Oceania" = 1, 
                "Asia_Japan" = 2, 
                "Asia_China" = 3, 
                "Europe" = 4, 
                "America" = 5
            );
        }
        region_id;
    }
    

    
    determineTradingDate <- function(this_region) {
        TO_DAY;
    }
    
    prepareFileName <- function(this_region, this_region_id, this_date) {
        dat <- list(
            path = sprintf("%sGit/Ventura/Trades_New/", DIRECTORY_CODE_HD),
            name = "%s_trd_ventura_%s_%s.csv" %>%
                sprintf(format(this_date, "%Y%m%d"), this_region_id, this_region)
        );
        dat$file_path_name <- paste0(dat$path, dat$name);
        print(dat)
        dat;
    }
    
    retrieveAssetsData <- function() {
        T.urlAssets %>%
            U.readGoogleSheet %>% 
            select(pair, ticker, time_zone_trading);
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################

    trd_region <- determineRegion();
    trd_region_id <- determineRegionId(trd_region);
    trd_date <- determineTradingDate(trd_region);
    
    print(sprintf("Region: %s - Region id: %s - Trade Date: %s", trd_region, trd_region_id, trd_date))

    dat_file <- prepareFileName(trd_region, trd_region_id, trd_date);

    dat_git <- dat_run$summary_1 %>% 
        filter(
            pair %in% ASSETS_PAIR_TICKER$pair,
            pair != "MCHUSD", pair != "ENZUSD"
        ) %>%
        mutate(
            trade_id = paste0(
                "V", 
                format(trd_date, "%Y%m%d"), 
                "0", 
                trd_region_id, 
                U.right(paste0("00",rownames(.)), 3)
                ),
            date_trade = trd_date,
            buy_sell = case_when(
                predict == "up" ~ 1, 
                predict == "down" ~ -1, 
                predict == "flat" ~ 0
                ),
            target = case_when(
                predict == "up" ~ t_up, 
                predict == "down" ~ t_dn, 
                predict == "flat" ~ NUM_NA
                ),
            stop_loss = case_when(
                predict == "up" ~ t_dn, 
                predict == "down" ~ t_up, 
                predict == "flat" ~ NUM_NA
                ),
            date_exit_latest = trd_date + 7
        ) %>% 
        rename(
            range_up = t_up, 
            range_dn = t_dn, 
            range_up_pct = tp_pct, 
            price_entry = px, 
            fx_ccy_1_usd = fx_ccy1
            ) %>%
        left_join(ASSETS_PAIR_TICKER, by = "pair") %>%
        left_join(dat_assets, by = "ticker") %>% 
        U.debug("All trades") %>%
        filter(time_zone_trading == trd_region) %>%
        U.debug("Trades just this region") %>%
        select(
            trade_id, date_trade, strategy, ticker, 
            price_entry, predict, buy_sell, target, stop_loss, date_exit_latest, 
            range_dn, range_up, 
            range_up_pct, fx_ccy_1_usd, notional_ccy1
        )
    U.printBanner("Git file: ")
    print(dat_git %>% data.frame)
    U.printBanner("Saving: ")
    print(dat_file$file_path_name)
    U.write.csv(dat_git, dat_file$file_path_name);
    U.printBanner("Saved: ")
    git_comment <- "Adding Trades Ventura %s %s" %>%
        sprintf(trd_region, format(trd_date, "%Y-%m-%d"));
    print(git_comment)

    git_command <- "cd %s && git add %s && git commit -m '%s' && git push origin master" %>%
        sprintf(dat_file$path, dat_file$name, git_comment);
    print(git_command)
    
    U.debug("Sending now")
    print(1)
    system(git_command)
    print(2)
    system("cd %s && git add %s && git commit -m '%s' && git push origin master" %>%
               sprintf(dat_file$path, dat_file$name, git_comment)
           )
    print(3)
    system('cd %s && git add %s && git commit -m "%s" && git push origin master' %>%
               sprintf(dat_file$path, dat_file$name, git_comment)
    )
    print(4)
    system(
        "cd %s && /usr/bin/git add %s && /usr/bin/git commit -m '%s' && /usr/bin/git push origin master" %>%
            sprintf(dat_file$path, dat_file$name, git_comment)
    )
    print(4)
    U.debug("Sending done")

    dat_git;
    
}
V.simulate <-
function (p_up, p_dn, bet_size, n_sim = 1000, n_paths = 10) 
{
    ####################################################################################################
    ### Script Parameters
    ####################################################################################################
    
    p_flat <- 1 - p_up - p_dn
    threshhold_dn <- p_dn
    threshold_flat <- p_dn + p_flat
    
    mean_middle = (p_up-p_dn)/(p_up+p_dn)
    
    rtn = bet_size / 100
    
    doOneSim <- function(sim_i) {
        data.frame(
            trajectory = as.character(sim_i),
            n = 1:n_sim,
            roll_dice = runif(n_sim)
        ) %>%
            U.data2Tibble %>%
            mutate(
                outcome = case_when(
                    roll_dice <= threshhold_dn ~ -1,
                    roll_dice <= threshold_flat ~ runif(1, 0, 1), #rnorm(1, mean_middle, 1),
                    TRUE ~ 1
                ),
                mlt = (1 + rtn * outcome),
                nav = cumprod(mlt)
            ) %>% 
            select(trajectory, n, outcome, nav)
    }
    
    1:n_paths %>%
        lapply(doOneSim) %>%
        bind_rows
}
V.testModel <-
function (strat_id = NULL, max_date = "2015-12-31", features_to_ignore = "", 
          calc_var_importance = FALSE, 
          model_n_trees = NULL,
          dat_input = NULL)
{
    ####################################################################################################
    ### Script Parameters
    ####################################################################################################
    USE_WEIGHTS_FOR_TRAINING <- FALSE
    
    MIN_PROBA_SIGNAL <- 0
    MIN_PROBA_SIGNAL_PLUS_FLAT <- 0
    
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    dat_strat <- "SELECT S.strategy_id, S.description, B.bb_width / 100 AS bb_width, 
        D.max_duration, S.rf_n_trees
    FROM static_strategy S
    LEFT JOIN static_trade_bb_width B ON B.bb_width_id = S.bb_width_id
    LEFT JOIN static_trade_max_duration D ON D.max_duration_id = S.max_duration_id
    WHERE strategy_id = %s" %>% 
        sprintf(strat_id) %>%
        D.select
    
    bb_band_target_width <- dat_strat$bb_width[1]
    n_days_trade <- dat_strat$max_duration[1]
    
    library("caret")
    
    time_start <- Sys.time()

    max_date <- as.Date(max_date)
    
    if (is.null(model_n_trees)) {
        model_n_trees <- dat_strat$rf_n_trees[1]
    }
    
    max_train_date_backup <- MAX_TRAIN_DATE

    ####################################################################################################
    ### Step 1: Market setup configuration and scoring function. 
    ####################################################################################################
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    prepareDataAndTrainModelUpToMaxDate <- function() {
        MAX_TRAIN_DATE <<- max_date
        dat <- T.getTechnicals("train", strat_id, TRUE, TRUE) %>%
            filter(
                asset_class != "bond",
                instrument_id <= 132
            ) %>%
            E.trainModel(strat_id, TRUE, model_n_trees, features_to_ignore, USE_WEIGHTS_FOR_TRAINING) 
        U.printBanner("Model training finished", FALSE)
        MAX_TRAIN_DATE <<- max_train_date_backup
        dat
    }
    
    varImportance <- function(dat_trade_model) {
        U.printBanner("Starting variable importance...", FALSE)
        dat_model <- dat_trade_model$model
        var_imp <- varimp(dat_model)
        var_imp <- var_imp[order(var_imp)]
        
        var_imp_plot <- data.frame(
            feature = names(var_imp), 
            var_imp = U.vectorize(var_imp)
        ) %>% 
            ggplot(aes(x = reorder(feature, var_imp), y = var_imp)) + 
            geom_point(col = "lightseagreen") + 
            geom_segment(
                aes(x = feature, xend = feature, y = 0, yend = var_imp), 
                col = "lightseagreen"
            ) + 
            coord_flip() +            
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank()
            );
        print(var_imp_plot)
        
        dat_trade_model$model_varimp <- var_imp
        dat_trade_model
    } 
    
    runTest_Try <- function() {
        dat_trade_model <- prepareDataAndTrainModelUpToMaxDate()
        dat_test <- dat_trade_model$test
        dat_train <- dat_trade_model$train
        dat_trade_model$res <- dat_test

        if (nrow(dat_test) > 0) {
            U.printBanner("Running predictions...", FALSE)
            dat_test <- E.modelPredict(dat_test, dat_trade_model, TRUE)
            dat_train$predict <- predict(dat_trade_model$model)
            U.printBanner("Predictions done...", FALSE)
            dat_test <- dat_test %>%
                mutate(
                    proba_signal = (predict == "up") * predict_proba_up +
                        (predict == "down") * predict_proba_down + 
                        (predict == "flat") * predict_proba_flat,
                    proba_signal_plus_flat = (predict != "flat") * proba_signal + predict_proba_flat
                ) %>%
                filter(
                    proba_signal >= MIN_PROBA_SIGNAL,
                    proba_signal_plus_flat >= MIN_PROBA_SIGNAL_PLUS_FLAT
                )

            dat_test <- U.dataFrame(dat_test)
            dat_test$buy_sell <- 0
            dat_test$buy_sell[which(dat_test$predict == "up")] <- 1
            dat_test$buy_sell[which(dat_test$predict == "down")] <- -1

            dat_test <- dat_test %>% 
                mutate(
                    close = 0.5 * (t_up + t_dn),
                    rtn = px_exit / close - 1,
                    pnl = rtn * buy_sell
                ) %>% 
                U.data2Tibble %>%
                left_join(select(INSTRUMENTS, instrument_id, pair), by = "instrument_id")

            dat_trade_model$res <- dat_test

            dat_1 <- dat_test %>% 
                filter(
                    buy_sell != 0, 
                    !is.na(buy_sell), 
                    tgt != "unknown", 
                    !is.na(pnl)
                ) %>%
                mutate(
                    year = year(date),
                    win = (pnl >= 0) + 0,
                    pnl_positive = pnl * win,
                    pnl_negative = pnl * (1-win),
                    pnl_gross = buy_sell * (px_exit - close) / (t_up - close)
                ) %>% 
                group_by(year) %>%
                summarize(
                    N = n(),
                    n_days_with_trade = length(unique(date)),
                    pnl = round(100 * mean(pnl), 2),
                    pnl_gross = round(mean(pnl_gross), 2),
                    duration_days = round(mean(duration), 2),
                    win_ratio = round(100 * sum(win) / N, 2),
                    profit_ratio = round(-mean(pnl_positive) / mean(pnl_negative), 2)
                ) %>% 
                ungroup

            
            dat_2 <- dat_test %>% 
                filter(
                    buy_sell != 0, 
                    !is.na(buy_sell), 
                    tgt != "unknown", 
                    !is.na(pnl)
                ) %>%
                mutate(
                    year = "Total",
                    win = (pnl >= 0) + 0,
                    pnl_positive = pnl * win,
                    pnl_negative = pnl * (1-win),
                    pnl_gross = buy_sell * (px_exit - close) / (t_up - close)
                ) %>% 
                group_by(year) %>%
                summarize(
                    N = n(),
                    n_days_with_trade = length(unique(date)),
                    pnl = round(100 * mean(pnl), 2),
                    pnl_gross = round(mean(pnl_gross), 2),
                    duration_days = round(mean(duration), 2),
                    win_ratio = round(100 * sum(win) / N, 2),
                    profit_ratio = round(-mean(pnl_positive) / mean(pnl_negative), 2)
                ) %>% 
                ungroup

            colnames(dat_2) <- colnames(dat_1)

            dat_1 <- rbind(dat_1, dat_2)

            
            dat_1$win_ratio[which(is.nan(dat_1$win_ratio))] <- NA
            dat_1$win_ratio[which(is.infinite(dat_1$win_ratio))] <- NA
            dat_1$profit_ratio[which(is.nan(dat_1$profit_ratio))] <- NA
            dat_1$profit_ratio[which(is.infinite(dat_1$profit_ratio))] <- NA

            dat_3 <- dat_test %>% 
                group_by(predict, tgt) %>%
                summarize(N = n()) %>% 
                ungroup %>%
                spread(tgt, N, fill = 0)

            dat_trade_model$stats_1 <- dat_1;
            dat_trade_model$stats_2 <- dat_3;

            dat_4 <- dat_test %>%
                filter(
                    buy_sell != 0, 
                    !is.na(buy_sell), 
                    tgt != "unknown", 
                    !is.na(pnl)
                ) %>%
                mutate(
                    year = paste0("Y",year(date))
                ) %>%
                group_by(pair, year) %>%
                summarize(N = n()) %>%
                ungroup %>%
                spread(year, N, fill = 0) %>%
                mutate(Total = reduce(select(., -pair), `+`)) %>%
                arrange(-Total)
            
            dat_5 <- dat_test %>% 
                mutate(gross_pnl = buy_sell * (px_exit - close) / (t_up - close)) %>% 
                group_by(date) %>%
                summarize(
                    N = n(), 
                    gross_pnl = mean(gross_pnl), 
                    nav_mult = 1 + 0.01 * gross_pnl * pmin(N, 10)
                ) %>% 
                ungroup %>%
                mutate(nav =  100*cumprod(nav_mult)-1) %>% 
                mutate(year = year(date)) %>%
                mutate(nav_prev = lag(nav, 1, default = 100)) %>%
                group_by(year) %>%
                summarize(
                    N = sum(N),
                    nav = last(nav),
                    rtn = round(100 * (nav / first(nav_prev) - 1), 2)
                ) %>% 
                data.frame %>%
                mutate(nav = round(nav, 2))

            
            U.printBanner("Model quality: train set")
            print(confusionMatrix(
                data = factor(dat_train$predict),
                reference = factor(dat_train$tgt), 
                mode = "prec_recall"
                ))
            
            U.printBanner("Model quality: test set")
            print(confusionMatrix(
                data = factor(dat_test$predict), 
                reference = factor(dat_test$tgt), 
                mode = "prec_recall"
                ))
            
            
            U.printBanner("Performance summary", FALSE)
            
            print(dat_1)
            print(dat_3)
            print(dat_4)
            print(dat_5)
        }

        if (calc_var_importance) {
            dat_trade_model <- varImportance(dat_trade_model)
        }    

        dat_trade_model
        
    }
    runTest <- function() U.try(runTest_Try, NA)();
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    U.printBanner("Starting. Preparing data...", FALSE)
    res <- runTest()
    MAX_TRAIN_DATE <<- max_train_date_backup
    
    U.printBanner(
        paste0(
            "Finished... Execution time: ", 
            round(as.numeric(difftime(Sys.time(),time_start, units = "mins")), 2), 
            " minutes"
        ), 
        FALSE)
    res
}
