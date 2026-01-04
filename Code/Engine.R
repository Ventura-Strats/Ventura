E.calculateModelResults <-
function (dat_trades_model, max_date = YESTERDAY)
    {
        ####################################################################################################
        ### Script variables
        ####################################################################################################
        
        
        ####################################################################################################
        ### Sub routines
        ####################################################################################################
        keepOnlyLastDateIfNightTesting <- function(dat) {
            if (NIGHT_TESTING) {
                dat <- dat %>%
                    filter(date == max_date);
            }
            dat;
        }
        
        replaceNanAndInfiniteWithNA <- function(dat) {
            for (j in 1:ncol(dat)) {
                dat[which(is.nan(dat[,j])),j] <- NA;
                dat[which(is.infinite(dat[,j])),j] <- NA;
            }
            dat;
        }
        
        addBuySellPnl_Try <- function(dat) {
            dat$buy_sell <- 0;
            dat$buy_sell[which(dat$predict == "up")] <- 1;
            dat$buy_sell[which(dat$predict == "down")] <- -1;
            dat %>% 
                mutate(pnl = rtn * buy_sell) %>% 
                U.data2Tibble;
        }
        addBuySellPnl <- function(dat) 
            U.try(addBuySellPnl_Try, dat)(dat)
        
        printResultsSummary <- function(dat) {
            dat_detail <- dat %>% 
                mutate(
                    year = year(date),
                    month = as.character(date - day(date) + 1),
                    win = (pnl >= 0) + 0,
                    pnl_positive = pnl * win,
                    pnl_negative = pnl * (1-win)
                ) %>% 
                filter(buy_sell != 0, !is.na(buy_sell), tgt != "unknown", !is.na(pnl))
            
            if (nrow(unique(select(dat_detail, year, month))) <= 12) {
                dat_detail <- dat_detail %>% 
                    group_by(month)
            }
            else {
                dat_detail <- dat_detail %>%
                    group_by(year)
            }
            
            dat_detail <- dat_detail %>%
                summarize(
                    N = n(),
                    pnl_per_trade = round(100*mean(pnl), 2),
                    stdev = round(100*sd(pnl), 2),
                    duration_days = round(mean(duration), 2),
                    win_ratio_N = round(100 * sum(win) / N, 2),
                    win_ratio_pnl = round(-mean(pnl_positive) / mean(pnl_negative), 2),
                    pnl_annualized = pnl_per_trade * 260 / duration_days
                ) %>% 
                ungroup %>% 
                tail(15);
            
            dat_summary <- dat %>% 
                filter(buy_sell != 0, !is.na(buy_sell), tgt != "unknown", !is.na(pnl)) %>%
                mutate(
                    year = "Total",
                    win = (pnl >= 0) + 0,
                    pnl_positive = pnl * win,
                    pnl_negative = pnl * (1-win)
                )
            if (night_testing) {
                dat_summary <- dat_summary %>%
                    group_by(date)
            }
            else {
                dat_summary <- dat_summary %>%
                    group_by(year)
            }
            dat_summary <- dat_summary %>%
                summarize(
                    N = n(),
                    pnl_per_trade = round(100*mean(pnl), 2),
                    stdev = round(100*sd(pnl), 2),
                    duration_days = round(mean(duration), 2),
                    win_ratio_N = round(100 * sum(win) / N, 2),
                    win_ratio_pnl = round(-mean(pnl_positive) / mean(pnl_negative), 2),
                    pnl_annualized = pnl_per_trade * 260 / duration_days
                ) %>% 
                ungroup;
            colnames(dat_summary) <- colnames(dat_detail)
            
            
            if (NIGHT_TESTING) {
                dat_display <- dat_summary;
            }
            else {
                dat_display <- rbind(dat_detail, dat_summary)
            }
            
            dat_display$win_ratio_pnl[which(is.nan(dat_display$win_ratio_pnl))] <- NA;
            dat_display$win_ratio_pnl[which(is.infinite(dat_display$win_ratio_pnl))] <- NA;
            dat_display$win_ratio_N[which(is.nan(dat_display$win_ratio_pnl))] <- NA;
            dat_display$win_ratio_N[which(is.infinite(dat_display$win_ratio_pnl))] <- NA;
            
            U.printBanner(max_date, FALSE);
            print(dat_display)
            
        }
        
        calculateModelResults <- function(dat_trades_model) {
            dat_result <- dat_trades_model$test %>%
                U.dataFrame %>%
                replaceNanAndInfiniteWithNA %>%
                T.addAssetClass %>%
                U.noData2Null %>%
                E.modelPredict(dat_trades_model$model) %>%
                addBuySellPnl %>%
                printResultsSummary;
            dat_trades_model$result <- dat_result;
            dat_trades_model;
        }
        
        ####################################################################################################
        ### Script 
        ####################################################################################################
        calculateModelResults(dat_trades_model);
    }
E.genericScoreFunction <-
function(dat, strat_id) {
        ####################################################################################################
        ### Script 
        ####################################################################################################
        
        criteria <- "SELECT score, criterion FROM Ventura.strategy_criteria WHERE strategy_id = %s" %>% 
            sprintf(strat_id) %>%
            D.select %>%
            mutate(score = paste0("score_", score, " = ")) %>%
            unite("criteria", sep = "") %>% 
            U.vectorize %>% 
            paste0(collapse = ", ")
        
        function_text <- paste0(
            "dat_trades %>% mutate(", 
            criteria, 
            ") %>%
        mutate(
            score = rowSums(select(., starts_with('score'))) * 10 / sum(grepl('score_', colnames(.))),
            score_high_enough = as.logical((score == 10))
        )")
        
        this_function <- function(dat_trades) eval(parse(text = function_text))
        do.call(this_function, list(dat))
    }
E.modelPredict <-
function (dat_tech, dat_model, calc_proba = FALSE) 
    {

        ####################################################################################################
        ### Sub routines
        ####################################################################################################
        prepareAdditionalSetForPredictionBecauseOfBugInCForest <- function() {
            dat_set <- dat_model$set
            col_names <- intersect(colnames(dat_set), colnames(dat_tech))
            rbind(
                dat_set[col_names] %>% sample_n(5),
                dat_tech[col_names]
            )
        }
        
        calcModelPredict_Try <- function(dat, rf_model) {
            predict(rf_model, newdata = dat, type = "response")
        }
        calcModelPredict <- function(dat, rf_model) {
            U.try(calcModelPredict_Try, "fail")(dat, rf_model)
        }
        
        calcModelProba_Try <- function(dat, rf_model) {
            if (calc_proba) {
                dat_proba <- predict(rf_model, newdata = dat, type = "prob") %>%
                    lapply(data.frame) %>%
                    bind_rows %>% 
                    U.data2Tibble
                colnames(dat_proba) <- gsub("tgt.", "predict_proba_", colnames(dat_proba))
                dat <- cbind(dat, dat_proba)
            }
            dat
        }
        calcModelProba <- function(dat, rf_model) 
            U.try(calcModelProba_Try, dat)(dat, rf_model)
        
        calcModelPredictions <- function() {
            dat_prediction_set <- prepareAdditionalSetForPredictionBecauseOfBugInCForest()
            rf_model <- dat_model$model
            if (U.dfContainsData(dat_prediction_set)) {
                dat_prediction_set$predict <- calcModelPredict(dat_prediction_set, rf_model)
                dat_prediction_set <- dat_prediction_set %>% 
                    calcModelProba(rf_model) %>% 
                    select(instrument_id, date, starts_with("predict"))
                dat_tech <- dat_tech %>% 
                    left_join(dat_prediction_set, by = c("instrument_id", "date"))
            }
            dat_tech
        }    
        
        ####################################################################################################
        ### Script
        ####################################################################################################
        calcModelPredictions()
    }
E.prepareModelData <-
function (strat_id, dat_input = NULL)
    {
        ####################################################################################################
        ### Script variables
        ####################################################################################################
        
        ####################################################################################################
        ### Sub routines
        ####################################################################################################
        loadTechFile <- function(fx_pair) {
            T.getTechnicals(fx_pair, strat_id)
        }
        
        prepareBaseData <- function() {
            INSTRUMENTS %>%
                filter(use_for_training == 1) %>%
                .$pair %>%
                lapply(loadTechFile) #%>% 
            bind_rows %>% 
                U.data2Tibble %>%
                arrange(date, instrument_id)
        }
        
        ####################################################################################################
        ### Script 
        ####################################################################################################
        res <- dat_input
        if (is.null(dat_input)) {
            res <- prepareBaseData()
        }
        res
    }
E.prepareTechnicalsLive <-
function (pair_list, force_recompute = FALSE, scenario_bump = NULL) 
    {
        ####################################################################################################
        ### Script variables
        ####################################################################################################
        INSTRUMENT_LIST <- A.getInstrumentId(pair_list)
        
        FEATURES <- D.select("SELECT feature_id, feature FROM static_feature")
        
        nb_years_vol_lt_decile <- filter(TECH_PARAM, technical_param == "nb_years_vol_lt_decile")$value
        n_days_vol_lt <- filter(TECH_PARAM, technical_param == "vol_days_lt")$value
        keep_rows_recent <- 21 + nb_years_vol_lt_decile * n_days_vol_lt
        
        ####################################################################################################
        ### Sub routines
        ####################################################################################################
        findBumpId <- function() {
            bump_id <- INT_NA
            if (!is.null(scenario_bump)) {
                bump_id <- D.select(
                    paste0("SELECT bump_id FROM static_scenario_bump WHERE bump = ", scenario_bump)
                )$bump_id[1]
            }
            bump_id
        }
        
        adjustPricesForScenario <- function(dat_live) {
            if (!is.null(scenario_bump)) {
                dat_bump <- dat_technicals_db %>% 
                    mutate(bump_reference = bb_up - bb_dn) %>%
                    select(instrument_id, bump_reference)
                
                dat_live <- dat_live %>%
                    left_join(dat_bump, by = "instrument_id") %>%
                    mutate(
                        close = close + scenario_bump * bump_reference / 100
                    )
            } 
            
            dat_live %>%       
                mutate(
                    high = pmax(high, close),
                    low = pmin(low, close)
                )
        }
        
        adjustLivePricesByEstimatingExpectedHighLowBeforeClose <- function(dat_live) {
            dat_vol_st <- "SELECT instrument_id, value AS vol_st 
            FROM live_technicals_dbl T
            LEFT JOIN static_feature F ON F.feature_id = T.feature_id
            WHERE F.feature = 'vol_st'" %>%
                D.SQL
            
            dat_live %>%
                left_join(dat_vol_st, by = "instrument_id") %>% 
                T.adjustHighLowWithMinMaxBeforeClose %>%
                select(instrument_id, date, timestamp_px, open, high, low, close)
        }
        
        prepareExistingHistoTechnicals_Try <- function() {
            dat_live <- D.SQL("SELECT * FROM live_px") %>% 
                filter(instrument_id %in% INSTRUMENT_LIST) %>%
                rename(timestamp_px = timestamp) %>%
                mutate(
                    date = as.Date(date),
                    timestamp_px = as.POSIXct(timestamp_px, tz = TZ_LOCAL)
                ) %>% 
                as_tibble %>% 
                adjustPricesForScenario %>% 
                adjustLivePricesByEstimatingExpectedHighLowBeforeClose
            
            T.getHistoPx(date_from = TO_DAY - (366 * (nb_years_vol_lt_decile + 1))) %>%
                filter(instrument_id %in% INSTRUMENT_LIST) %>%
                mutate(timestamp_px = dat_live$timestamp_px[1]) %>%
                select(colnames(dat_live)) %>%
                anti_join(dat_live, by = c("instrument_id", "date")) %>%
                rbind(dat_live) %>%
                arrange(instrument_id, date)
        }
        
        prepareExistingHistoTechnicals <- function()
            U.try(prepareExistingHistoTechnicals_Try)()
        
        doWeNeedToRecomputeTechnicals_Try <- function(instrument) {
            test_recompute <- TRUE
            if (!force_recompute) {
                fx_pair <- filter(INSTRUMENTS, instrument_id == instrument)$pair 
                test_recompute <- dat_live %>% 
                    filter(instrument_id == instrument) %>%
                    semi_join(
                        dat_technicals_db, 
                        by = c("instrument_id", "date", "open", "high", "low", "close")
                    ) %>%
                    U.noData2Null %>% 
                    is.null %>% 
                    U.printMilestone(paste0("Do we need to recompute technicals for ", fx_pair, ": ", .))
            }
            test_recompute
        }
        doWeNeedToRecomputeTechnicals <- function(instrument) 
            U.try(doWeNeedToRecomputeTechnicals_Try, TRUE)(instrument) 
        
        prepareTechnicalsOnePair_Try <- function(instrument) {
            fx_pair <- filter(INSTRUMENTS, instrument_id == instrument)$pair
            #U.printBanner(paste0("Doing technicals for ", fx_pair))
            U.printTickerProgressVerbose(fx_pair, pair_list)
            if (doWeNeedToRecomputeTechnicals(instrument)) {
                dat_tech <- dat_histo_live %>% 
                    filter(instrument_id == instrument) %>%
                    tail(keep_rows_recent) %>% 
                    U.printMilestone(paste0("Starting computations for ", fx_pair)) %>%
                    T.calcTechnicalsLive %>% 
                    U.printMilestone(paste0("Finished computations for ", fx_pair)) %>%
                    filter(date >= YESTERDAY) %>%
                    tail(1)
                if (ncol(dat_tech) < 100) {
                    dat_tech <- NULL
                }
                dat_tech %>% 
                    U.noData2Null
            }
            else {
                U.printBanner(paste(fx_pair, " - Unchanged data, not recomputing"), FALSE)
                dat_technicals_old %>% 
                    semi_join(dat_live, by = c("instrument_id", "date")) %>%
                    U.noData2Null
            }
        }
        prepareTechnicalsOnePair <- function(instrument)
            U.try(prepareTechnicalsOnePair_Try)(instrument)
        
        finalCleanData <- function(dat) {
            dat <- as.data.frame(dat)
            dat <- dat[,setdiff(colnames(dat), c("open_na", "low_na", "high_na"))]
            
            for (j in 1:ncol(dat)) {
                if (!(colnames(dat)[j] %in% c("date", "timestamp", "timestamp_px"))) {
                    dat[,j] <- U.vectorize(dat[,j])
                }
                dat[which(is.nan(dat[,j])),j] <- NA
                dat[which(is.infinite(dat[,j])),j] <- NA
            }
            
            dat$asset_class <- factor(
                U.vectorize(dat$asset_class), 
                levels = c("fx_dm", "fx_em", "index", "metal", "yield", "bond")
            );
            
            dat %>% as_tibble
        }
        
        formatForDB_Try <- function(dat) {
            time_now <- Sys.time()
            
            dat %>%
                gather(feature, value, -instrument_id, -date, -timestamp_px) %>% 
                left_join(D.loadTable("static_feature"), by = "feature") %>%
                filter(!is.na(feature_id), !is.na(instrument_id)) %>% 
                mutate(
                    timestamp = time_now,
                    instrument_id = as.integer(instrument_id),
                    feature_id = as.integer(feature_id),
                    int_or_dbl = as.integer(int_or_dbl)
                ) %>%
                select(instrument_id, date, timestamp, timestamp_px, feature_id, value, int_or_dbl) %>% 
                na.omit
        }
        formatForDB <- function(dat) 
            U.tryNull(formatForDB_Try, dat)
        
        saveToDBTableWithBump <- function(dat, int_or_dbl) {
            U.debug("Save to DB with bump")
            
            zero_one <- switch(int_or_dbl, "int" = 0, "dbl" = 1)
            
            db_tbl_name <- "scenario_technicals_%s" %>% 
                sprintf(int_or_dbl)
            
            dat_db <- dat %>%
                filter(int_or_dbl == zero_one) %>%
                select(-int_or_dbl) %>% 
                select(bump_id, date, instrument_id, feature_id, value)
            
            "DELETE FROM %s WHERE bump_id = %s" %>%
                sprintf(db_tbl_name, bump_id) %>%
                D.SQL
            
            D.insertDataIntoTable(db_tbl_name, dat_db, FALSE)
            
            dat
        }
        
        saveToDBTableWithoutBump <- function(dat, int_or_dbl) {
            db_tbl_name <- "live_technicals_%s" %>% 
                sprintf(int_or_dbl)
            
            zero_one <- switch(int_or_dbl, "int" = 0, "dbl" = 1)
            
            "DELETE FROM %s WHERE instrument_id IN (%s)" %>%
                sprintf(db_tbl_name, paste(INSTRUMENT_LIST, collapse = ",")) %>%
                D.SQL
            
            dat %>%
                filter(int_or_dbl == zero_one) %>%
                select(-int_or_dbl) %>%
                D.insertDataIntoTable(db_tbl_name, ., FALSE)
            dat
        }
        
        saveToDBTable <- function(dat, int_or_dbl) {
            if (is.null(scenario_bump)) {
                saveToDBTableWithoutBump(dat, int_or_dbl)
            } else {
                dat$bump_id <- bump_id
                saveToDBTableWithBump(dat, int_or_dbl)
            }
        }
        
        saveToDB_Try <- function(dat) {
            dat %>%
                saveToDBTable("int") %>%
                saveToDBTable("dbl")
        }
        saveToDB <- function(dat)
            U.tryNull(saveToDB_Try,dat)
        
        updateTechnicalsStatusInDB <- function(dat){
            if (is.null(scenario_bump)) {
                U.printBanner("Updating status db...", FALSE)
                updateDB <- function(instrument_id) {
                    "UPDATE status_instrument
                SET live_technicals_last_update = '%s'
                WHERE instrument_id = %s" %>%
                        sprintf(
                            format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
                            instrument_id
                        ) %>%
                        D.SQL 
                }
                lapply(U.vectorizeUnique(dat$instrument_id), updateDB)
                U.printBanner("Updating status db done...", FALSE)
            }
            dat
        }
        
        prepareTechnicals <- function() {
            INSTRUMENT_LIST %>% 
                lapply(prepareTechnicalsOnePair) %>% 
                bind_rows %>% 
                finalCleanData %>% 
                formatForDB %>% 
                saveToDB %>% 
                updateTechnicalsStatusInDB
        }
        ####################################################################################################
        ### Script 
        ####################################################################################################
        bump_id <- findBumpId()
        
        dat_technicals_db <<- T.getTechnicalsLive() %>%
            filter(instrument_id %in% INSTRUMENT_LIST)
        
        dat_histo_live <<- prepareExistingHistoTechnicals()
        
        prepareTechnicals()
        
    }
E.save <-
function() {
        All_Objects <- ls(".GlobalEnv", pattern = "E.");
        All_Objects <- All_Objects[substr(All_Objects,1,2) == "E."];
        dump(All_Objects, paste0(DIRECTORY_CODE_HD, "Code/Engine.R"));
    }
E.trainModel <-
function (dat_trades_list, strat_id, export_all_data = FALSE, n_trees = NULL, features_to_ignore = "",
          weights_with_time = FALSE, use_limited_training_set = TRUE)
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    FEATURES <- D.loadTableLocal("feature")
    
    features_this_strat <- 
        "SELECT F.feature
        FROM strategy_feature S
        LEFT JOIN static_feature F ON F.feature_id = S.feature_id
        WHERE strategy_id = %s" %>%
        sprintf(strat_id) %>%
        D.select %>%
        U.vectorize %>%
        gsub("asset_class_id", "asset_class", .)
    
    columns_to_never_train <- c(
        "instrument_id", "pair", "date", 
        "open", "high", "low", "close", "open_na", "high_na", "low_na",
        "spline_slow", "spline_fast", "spline_medium", "bb_up", "bb_dn", "duration",
        "px_exit", "hit_hi", "hit_lo", "rtn", "t_up", "t_dn", "trend_next_month_r2", 
        "trend_slope", "nxt_max_21", "nxt_min_21", "nxt_max_ampl_21",  "nxt_max_65", "nxt_min_65", 
        "nxt_max_ampl_65", "nxt_max_130", "nxt_min_130", "nxt_max_ampl_130", 
        "nxt_max_260", "nxt_min_260", "nxt_max_ampl_260", "date_exit", "vol_lt", "vol_st"
    )
    
    rf_n_trees <- D.loadTableLocal("strategy") %>% 
        filter(strategy_id == strat_id) %>% 
        .$rf_n_trees
    
    if (!is.null(n_trees)) {
        rf_n_trees <- n_trees
    }
    
    weight_speed_years <- 10
    
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
    
    limited_training_set <- INSTRUMENTS %>% 
        filter(pair %in% limited_training_set) %>%
        .$instrument_id
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    prepareAllTrades <- function(dat_base) {
        dat_base %>% 
            E.genericScoreFunction(strat_id) %>% 
            arrange(-score) %>% 
            filter(
                tgt %in% c("up", "flat", "down"),
                score_high_enough
            ) %>% 
            select(-starts_with("score"))
    }
    
    prepareInitialTrainingSet <- function(dat_trades) {
        dat <- dat_trades %>%
            filter(
                date < MAX_TRAIN_DATE, # This line should be useless
                date_exit < MAX_TRAIN_DATE
            ) %>%
            sample_n(nrow(.))
        U.printBanner(paste0("Max entry date: ", max(dat$date)), FALSE)
        U.printBanner(paste0("Max exit date: ", max(dat$date_exit)), FALSE)
        dat
    }
    
    prepareInitialTestSet <- function(dat_trades, dat_train) {
        dat_trades %>% 
            anti_join(dat_train, by = c("instrument_id", "date")) %>%
            filter(date >= MAX_TRAIN_DATE)
    }
    
    addModelWeights <- function(dat_train) {
        dat_train %>% 
            mutate(
                t_years = U.yearFrac(date, MAX_TRAIN_DATE),
                model_weight = exp(-t_years / weight_speed_years)
            ) %>%
            select(-t_years)
    }
    
    removeColumnsToNeverTrain <- function(dat_train) {
        dat_train %>%
            select(setdiff(colnames(dat_train), columns_to_never_train))
    }
    
    removeFeaturesToNotTrainForThisModel <- function(dat_train) {
        keep_columns <- setdiff(colnames(dat_train), features_to_ignore)
        dat_train[, keep_columns] %>%
            na.omit
    }
    
    removeNAsInTestSet <- function(dat_train, dat_test) {
        columns_train_set <- setdiff(colnames(dat_train), "tgt")
        columns_to_span <- intersect(colnames(dat_test), columns_train_set)
        for (this_col in columns_to_span) {
            dat_test <- dat_test[which(!is.na(dat_test[[this_col]])),]
        }
        dat_test
    }
    
    prepareTrainingAndTestSet <- function(dat_base) {

        dat_trades <- prepareAllTrades(dat_base)

        dat_train <- prepareInitialTrainingSet(dat_trades)

        dat_test <- prepareInitialTestSet(dat_trades, dat_train)
        
        if (use_limited_training_set) {
            dat_train <- dat_train %>% 
                filter(instrument_id %in% limited_training_set)
        }

        dat_train <- dat_train %>% 
            addModelWeights %>%
            removeColumnsToNeverTrain %>%
            removeFeaturesToNotTrainForThisModel %>%
            U.dataFrame

        for (j in 1:ncol(dat_train)) {
            dat_train[which(is.nan(dat_train[,j])),j] <- NA
            dat_train[which(is.infinite(dat_train[,j])),j] <- NA
        }
        
        dat_train <- na.omit(dat_train)

        dat_weights <- NULL
        if (weights_with_time) {
            dat_weights <- dat_train$model_weight
        }
        
        dat_train <- dat_train %>%
            select(-model_weight)

        if ("asset_class" %in% colnames(dat_train)) {
            dat_train$asset_class <- factor(
                U.vectorize(dat_train$asset_class),
                levels = c("fx_dm", "fx_em", "index", "metal", "yield", "bond")
            )
        }
        dat_train$tgt <- factor(U.vectorize(dat_train$tgt), levels = c("up", "flat", "down"))
        
        dat_test <- dat_test %>% 
            U.dataFrame

        for (j in 1:ncol(dat_test)) {
            dat_test[which(is.nan(dat_test[,j])),j] <- NA
            dat_test[which(is.infinite(dat_test[,j])),j] <- NA
        }

        dat_test <- removeNAsInTestSet(dat_train, dat_test)
        dat_test$tgt <- factor(U.vectorize(dat_test$tgt), levels = c("up", "flat", "down"))
        
        if ("asset_class" %in% colnames(dat_test)) {
            dat_test$asset_class <- factor(
                dat_test$asset_class,
                levels = c("fx_dm", "fx_em", "index", "metal", "yield", "bond")
            )
        }

        dat_trades_list <- list(
            set = dat_trades,
            train = dat_train,
            weights = dat_weights
        )
        
        if (export_all_data) {
            dat_trades_list$base <- dat_base
            dat_trades_list$test <- dat_test
        }
        
        dat_trades_list
    }
    
    trainModel_Try <- function(dat_trades_list) {
        U.printBanner(
            paste0(
                "Starting model training. Size of training set: ", 
                nrow(dat_trades_list$train)
            )
            , FALSE)
        cforest(
            tgt ~ ., 
            data = dat_trades_list$train, 
            controls = cforest_unbiased(ntree = rf_n_trees),
            weights = dat_trades_list$weights
        )
    }
    
    trainModel <- function(dat_trades_list) {
        dat_trades_list$model <- U.try(trainModel_Try, NA)(dat_trades_list)
        gc()
        dat_trades_list
    }
    
    keepOnlyNecessaryData <- function(dat_trades_list) {
        if (!export_all_data) {
            dat_trades_list$train <- NULL
        }
        dat_trades_list
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    dat_trades_list %>% 
        prepareTrainingAndTestSet %>% 
        trainModel %>% 
        keepOnlyNecessaryData
}
