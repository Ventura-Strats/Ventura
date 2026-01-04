C.initTelegramBot <-
function (bot_nb = 1) 
{
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    bot_id <- switch(
        bot_nb,
        "1" = "1581341974:AAFuSnppwldQzf6GaF80oybMSee-YvceMOY",
        "2" = "1628162434:AAHnq0CaCYQejt2VCI5OfABc328ZVkP-204"
    )
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    library(telegram.bot)
    TELEGRAM_BOT <<- Bot(token = bot_id)
    bot_updates <- TELEGRAM_BOT$getUpdates()
    TELEGRAM_CHAT_ID <<- try(bot_updates[[1]]$from_chat_id())
    if (class(TELEGRAM_CHAT_ID) == "try-error") {
        
        TELEGRAM_CHAT_ID <<- switch(
            bot_nb,
            "1" = 506340054,
            "2" = 506340054
        )
    }
    NULL
}
C.save <-
function() {
    All_Objects <- ls(".GlobalEnv", pattern = "C.");
    All_Objects <- All_Objects[substr(All_Objects,1,2) == "C."];
    dump(All_Objects, paste0(DIRECTORY_CODE_HD, "/Code/Communications.R"));
}
C.sendJobsStatus <-
function () 
{
    ####################################################################################################
    ### Script
    ####################################################################################################

    G.Diagnostic.Jobs.Data.Activity(Sys.Date()) %>%
        G.Diagnostic.Jobs.Plot.Utilization(Sys.Date()) %>%
        C.telegramSendPlot(c(100, 90))
    
}
C.sendLivePnL <-
function () 
{
    ####################################################################################################
    ### Script
    ####################################################################################################

    B.readTradesFromDB() %>%
        G.Book.Trades_Live.Table.trades_live("table") %>%
        C.telegramSendTable(c(48, 1+nrow(.)))
    
}
C.sendPredict <-
function () 
{
    ####################################################################################################
    ### Script
    ####################################################################################################
    no_signal <- FALSE
    dat <- G.Predict.Data.predict() %>%
        left_join(select(INSTRUMENTS, pair, asset), by="pair") %>%
        mutate(
            asset = case_when(
                asset_class %in% c("index") ~ asset,
                TRUE ~ pair
            )
        ) %>%
        filter(signal_ok) %>%
        select(-date, -time_prd, -notional, -ends_with("_ok"), -models_agree, -score, -predict_w) %>%
        rename(
            strategy = strategy_id,
            proba = proba_signal,
            proba_w = proba_signal_w,
            spread = proba_spread,
            spread_w = proba_spread_w,
            antisignal = proba_antisignal,
            class = asset_class,
            name = asset
        ) %>%
        mutate(
            tp_pct = round(100 * tp_pct, 3),
            proba = round(100 * proba, 1),
            proba_w = round(100 * proba_w, 1),
            spread = round(100 * spread, 1),
            spread_w = round(100 * spread_w, 1),
            threshold = round(100 * threshold, 1),
            antisignal = round(100 * antisignal, 1)
        ) %>%
        select(
            region, class, strategy, pair, ticker, name, time_px, price, target, stop, 
            tp_pct, predict,proba, proba_w, spread, spread_w, threshold, antisignal
        )
    if (nrow(dat) == 0) {
        no_signal <- FALSE
        dat <- data.frame(Signal = "No signal")
    }
    C.telegramSendTable(
        dat, 
        c(
            if (no_signal) 20 else 47, 
            pmax(3, nrow(dat)+2)
        )
    )
    
}
C.sendSignal <-
function () 
{
    
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    
    EXECUTION_TIME_ID <- A.getExecutionTimeId(start_time - 2*60)
    #    EXECUTION_TIME_ID <- 4
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    readFile <- function() {
        "%sGit/Ventura/trades_new/%s/%s/new_%s-%s.csv" %>% 
            sprintf(
                DIRECTORY_CODE_HD,
                format(TO_DAY, "%Y-%m"),
                format(TO_DAY, "%Y-%m-%d"),
                format(TO_DAY, "%Y%m%d"),
                U.right(paste0("0", EXECUTION_TIME_ID), 2)
            ) %>% 
            read.csv
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat <- readFile() %>% U.debug(1)
        
        if (U.dfContainsData(dat)) {
            dat %>%
                left_join(select(INSTRUMENTS, ticker, asset, asset_class), by="ticker") %>% 
                U.debug(2) %>%
                mutate(
                    asset = case_when(
                        asset_class %in% c("index") ~ asset,
                        TRUE ~ ticker
                    )
                ) %>% U.debug(3) %>%
                rename(name = asset) %>% U.debug(4) %>%
                select(asset_class, strategy, ticker, name, price_entry, predict, 
                       target, stop_loss, tp_pct, 
                       notional_for_1k_pnl) %>% U.debug(5) %>%
                arrange(asset_class, ticker, strategy) %>% U.debug(6) %>%
                rename(class = asset_class) %>% U.debug(7) %>%
                mutate(tp_pct = round(100 * tp_pct, 3)) %>% U.debug(8) %>%
                C.telegramSendTable(c(28, pmax(2, nrow(.))))
        }
    
    
}
C.telegramSendPlot <-
function (plot_to_send, plot_dimensions_cm = c(50, 75)) 
{
    ####################################################################################################
    ### Script
    ####################################################################################################
    file_name <- "tmp_plot_%s.png" %>% sprintf(U.genPassword(10, FALSE))
    ggsave(
        file_name, 
        plot_to_send, 
        width = plot_dimensions_cm[1],
        height = plot_dimensions_cm[2],
        units = "cm",
        dpi = "screen"
        )
    try(TELEGRAM_BOT$sendPhoto(chat_id = TELEGRAM_CHAT_ID, photo = file_name))
    unlink(file_name)
    
}
C.telegramSendTable <-
function (dat, plot_dimensions_cm = c(50, 75)) 
{
    ####################################################################################################
    ### Script
    ####################################################################################################
    file_name <- "tmp_tbl_%s.png" %>% sprintf(U.genPassword(10, FALSE))
    library(gridExtra)
    png(
        filename = file_name, 
        width = plot_dimensions_cm[1], 
        height = plot_dimensions_cm[2], 
        units="cm", 
        res=150
        )
    grid.table(dat)
    dev.off()
    
    try(TELEGRAM_BOT$sendPhoto(chat_id = TELEGRAM_CHAT_ID, photo = file_name))
    unlink(file_name)
    
}
