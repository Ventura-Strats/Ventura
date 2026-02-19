T.addAllSplines <-
function (dat_ohlc, only_recent = FALSE, do_only_last = FALSE) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    period_type <- "D";
    nb_years_vol_lt_decile <- filter(TECH_PARAM, technical_param == "nb_years_vol_lt_decile")$value
    days_vol_lt <- filter(TECH_PARAM, technical_param == "vol_days_lt")$value
    bb_width <- filter(TECH_PARAM, technical_param == "bb_width")$value
    nb_points_spline <- filter(TECH_PARAM, technical_param == "spline_nb_points")$value
    recent_days <-  10 + nb_years_vol_lt_decile * days_vol_lt
    
    smf <- c("slow", "medium", "fast")
    n_list <- c(5, 20, 130, 260)
    col_names_list <- c(
        paste0("spline_", smf),
        paste0("slope_", smf),
        paste0("accel_", smf),
        paste0("slope_change_", smf),
        paste0("n_slope_", smf),
        paste0("lvl_spline_l_", 1:5, "_fast"),
        paste0("lvl_spline_r_", 1:5, "_fast"),
        paste0("lvl_spline_l_", 1:5, "_medium"),
        paste0("lvl_spline_r_", 1:5, "_medium"),
        paste0("lvl_spline_l_", 1:5, "_slow"),
        paste0("lvl_spline_r_", 1:5, "_slow"),
        "macdfls",
        "macdcross"
    )
    
    dat_spline_empty <- rep(NUM_NA, length(col_names_list)) %>%
        t %>% 
        data.frame %>% 
        U.data2Tibble;
    colnames(dat_spline_empty) <- col_names_list;
    histo_length <- (nb_points_spline + 1);
    
    ####################################################################################################
    ### Script sub functions
    ####################################################################################################
    fillMissingOpenWithPreviousClose <- function(dat) {
        if ("high_na" %in% colnames(dat)) {
            dat$high <- dat$high_na;
            dat$low <- dat$low_na;
            dat$open <- dat$open_na;
        }
        dat;
    }

    keepOnlyRecentDataIfNecessary <- function(dat) {
        if (only_recent) {
            dat <- dat %>% 
                tail(recent_days)
        }
        dat;
    }
    
    addSplineOneDate_Try <- function(i) {
        dat_spline <- dat_spline_empty;
        if (
            (!do_only_last & i >= histo_length) | 
            (do_only_last & i == nrow(dat_work))
            ) {
            dat_spline <- dat_work[(i-histo_length):i,] %>% 
                select(date, open, high, low, close, vol_lt) %>% 
                T.addSpline("slow", period_type) %>%  
                T.addSpline("medium", period_type) %>% 
                T.addSpline("fast", period_type) %>% 
                tail(2) %>% 
                T.addMACDFLS %>% 
                tail(1) %>%
                select(-open, -high, -low, -close, -vol_lt);
            if (!("macdfls" %in% colnames(dat_spline))) {
                dat_spline <- dat_spline_empty;
            }
        }
        dat_spline;
    }
    addSplineOneDate <- function(i)
        U.try(addSplineOneDate_Try, dat_spline_empty)(i)

    ####################################################################################################
    ### Script 
    ####################################################################################################
    dat_work <- dat_ohlc %>% 
        fillMissingOpenWithPreviousClose %>%
        keepOnlyRecentDataIfNecessary;
    
    dat_spline <- 1:nrow(dat_work) %>%
        lapply(addSplineOneDate) %>%
        bind_rows
    
    dat_ohlc %>%
        left_join(dat_spline, by = "date")
}
T.addAssetClass <-
function (dat) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    ####################################################################################################
    ### Script
    ####################################################################################################
    if (nrow(dat) > 0) {
        dat <- dat %>%
            left_join(select(INSTRUMENTS, instrument_id, asset_class), by = "instrument_id");
    }

    dat$asset_class <- factor(
        U.vectorize(dat$asset_class),
        levels = c("fx_dm", "fx_em", "index", "metal", "yield", "bond")
        );

    dat;
}
T.addAutoCorrelation <-
function (dat_ohlc) 
{
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    n_days_cor <- filter(TECH_PARAM, technical_param == "autocorrelation_n_days")$value
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    calcCorrel_Try <- function(dat) {
        dat_cor <- dat %>% 
            mutate(
                rtn_t = log(close / lag(close, 1)),
                rtn_y = lag(rtn_t, 1)
            ) %>%
            select(rtn_t, rtn_y) %>%
            U.dataFrame;
            
        dat$autocor_close <- rollapply(
            data = dat_cor,
            width = n_days_cor,
            FUN = function(x) cor(x[,1],x[,2]), 
            align = "right",
            by.column=FALSE,
            fill = NUM_NA
        ) %>%
            U.vectorize;
        
        dat;
    }
    calcCorrel <- function(dat)
        U.try(calcCorrel_Try, dat %>% mutate(autocor_close = NUM_NA))(dat)
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    calcCorrel(dat_ohlc)
    
}
T.addBiggestRecentCandle <-
function (dat_ohlc) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    n_list <- c(5, 20, 65, 130, 260);
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    addBiggestRecentCandleN <- function(dat, n) {
        dat[paste0("hi_lo_", n)] <- T.rollApply(dat$hi_lo, n, max);
        dat[paste0("n_hi_lo_", n)] <- T.rollApply(dat$hi_lo, n, T.whichMaxLast);
        dat[paste0("cl_op_", n)] <- T.rollApply(dat$cl_op, n, max);
        dat[paste0("n_cl_op_", n)] <- T.rollApply(dat$cl_op, n, T.whichMaxLast);
        sumhc <- T.rollApply(dat$h_c, n, sum) ^ 2;
        sumlc <- T.rollApply(dat$l_c, n, sum) ^ 2;
        dat[paste0("hivslo_cl_mean_", n)] <- log(sumhc / sumlc) %>% U.replaceNaNWithNA;
        dat;
    }
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    
    dat <- dat_ohlc %>% 
        mutate(
            hi_lo = log(high / low) / vol_lt,
            cl_op = abs(open_close),
            h_c = log(high / close),
            l_c = log(close / low)
        ) %>%
        select(
            -starts_with("hi_lo_"),
            -starts_with("n_hi_lo_"),
            -starts_with("cl_op_"),
            -starts_with("n_cl_op_"),
            -starts_with("hivslo_cl_mean")
        )

    for (n in n_list) {
        dat <- U.try(addBiggestRecentCandleN, dat)(dat, n);
    }

    dat %>%
        select(-cl_op, -h_c, -l_c, -starts_with("sumhc_"), -starts_with("sumlc_"))
    
}
T.addBollingerBandsFLS <-
function (dat_ohlc_vol_spline, n_stdev, period_type) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    sqt_days_year <- TECH_PARAM %>% 
        filter(technical_param == paste0("vol_n_periods_year_", period_type)) %>%
        .$value %>%
        sqrt;

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    addBollingerFLSBands_Try <- function(dat) {
        dat %>%
            mutate(
                adj_vol = n_stdev * vol_st / sqt_days_year,
                bb_up = spline_medium * (1 + adj_vol),
                bb_dn = spline_medium * (1 - adj_vol),
            ) %>%
            select(-adj_vol);
    }
    ####################################################################################################
    ### Script
    ####################################################################################################
    U.try(addBollingerFLSBands_Try, dat_ohlc_vol_spline)(dat_ohlc_vol_spline)
}
T.addCandleShadowsMeasure <-
function (dat_ohlc) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    n_list <- c(5, 20, 65, 130, 260);
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    addShadows <- function(dat, n) {
        dat[paste0("up_shdw_", n)] <- T.rollApply(dat$upper_shadow, n, sum) %>% U.replaceNaNWithNA;
        dat[paste0("dn_shdw_", n)] <- T.rollApply(dat$lower_shadow, n, sum) %>% U.replaceNaNWithNA;
        dat[paste0("up_vs_dn_shdw_", n)] <- log(
            dat[paste0("up_shdw_", n)] / dat[paste0("dn_shdw_", n)]
            ) %>% 
            U.replaceNaNWithNA;
        shdw_sq <- T.rollApply(dat$shdw_sq, n, sum) %>% U.replaceNaNWithNA;
        candle_sq <- T.rollApply(dat$total_candle_sq, n, sum) %>% U.replaceNaNWithNA;
        shdw_sd <- T.rollApply(dat$shdw_m, n, sd) %>% U.replaceNaNWithNA;
        dat[paste0("shdw_vs_body_", n)] <- -log(shdw_sq / candle_sq) %>% U.replaceNaNWithNA;
        dat[paste0("shdw_sd_", n)] <- shdw_sd %>% U.replaceNaNWithNA;
        dat;
    }
    

    ####################################################################################################
    ### Script variables
    ####################################################################################################
    dat <-  dat_ohlc %>%
        mutate(
            hi_bound = pmax(open, close),
            lo_bound = pmin(open, close),
            upper_shadow = log(high / hi_bound) / vol_lt,
            lower_shadow = log(lo_bound / low) / vol_lt,
            shdw_sq = ((high - hi_bound) + (lo_bound - low)) ** 2,
            total_candle_sq = (high - low) ** 2,
            shdw_m = upper_shadow + lower_shadow
        );
    
    dat$upper_shadow <- dat$upper_shadow %>% U.replaceNaNWithNA;
    dat$lower_shadow <- dat$lower_shadow %>% U.replaceNaNWithNA;
    
    for (n in n_list) {
        dat <- addShadows(dat, n);
    }
    
    dat %>%
        select(-hi_bound, -lo_bound, -upper_shadow, -lower_shadow, -shdw_sq, -total_candle_sq, -shdw_m);
    
}
T.addDateRelated <-
function (dat_ohlc, do_only_last = FALSE) 
{
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    addWeekDays <- function(dat) {
        weekday_str <- U.vectorize(weekdays(dat$date));
        dat$weekday <- INT_NA;
        
        pos_monday <- which(weekday_str == "Monday");
        pos_tuesday <- which(weekday_str == "Tuesday");
        pos_wednesday <- which(weekday_str == "Wednesday");
        pos_thursday <- which(weekday_str == "Thursday");
        pos_friday <- which(weekday_str == "Friday");
        pos_saturday <- which(weekday_str == "Saturday");
        pos_sunday <- which(weekday_str == "Sunday");
        
        dat$weekday[pos_monday] <- 1;
        dat$weekday[pos_tuesday] <- 2;
        dat$weekday[pos_wednesday] <- 3;
        dat$weekday[pos_thursday] <- 4;
        dat$weekday[pos_friday] <- 5;
        dat$weekday[pos_saturday] <- 6;
        dat$weekday[pos_sunday] <- 7;
        
        dat;
    }
    
    calcWeekDaysBetweenTwoDates <- function(date_from, date_to) {
        dates_list <- seq(date_from, date_to, 1);
        dates_weekday <- weekdays(dates_list);
        dates_len <- length(dates_list[which(dates_weekday != "Saturday", dates_weekday != "Sunday")]);
        if (length(dates_len) == 0) {
            dates_len <- 1;
        }
        as.integer(dates_len - 1);
    }
    
    calcWeekdaysTillMonthEnd <- function(date_from) {
        date_from <- as.Date(date_from);
        date_to <- fmdates::eom(date_from);
        calcWeekDaysBetweenTwoDates(date_from, date_to);
    }
    
    addDaysTillMonthEnd <- function(dat) {
        dat$days_to_eom <- U.sapply(dat$date, calcWeekdaysTillMonthEnd);
        dat;
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    dat_ohlc %>% 
        addWeekDays %>%
        addDaysTillMonthEnd;
}
T.addEngulfing <-
function (dat_ohlc) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    candle_yesterday_not_too_small_vs_today_threshold <- 1.25;
    candles_not_too_small_threshold <- 0.5 / sqrt(260);
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    testEngulfing <- function(o_t, h_t, l_t, c_t, o_y, h_y, l_y, c_y, vol_lt) {
        updn_t <- sign(c_t - o_t);
        updn_y <- sign(c_y - o_y);
        0 + (
            (updn_t == -updn_y) &
                (l_t <= l_y) &
                (h_t >= h_y) &
                ((c_t - o_t) / (c_y - o_y) <= 
                     candle_yesterday_not_too_small_vs_today_threshold) &
                (h_t - l_t >= vol_lt * candles_not_too_small_threshold) &
                (h_y - l_y >= vol_lt * candles_not_too_small_threshold)
        )
    }
    
    findFirstNonZero <- function(this_vector) {
        this_vector <- this_vector[length(this_vector):1];
        non_zeros <- which(this_vector != 0);
        find_non_zero <- 1000;
        if (length(non_zeros) > 0) {
            find_non_zero <- non_zeros[1] - 1;
        }
        find_non_zero;
    }
    
    addNbDaysSinceEngulfing <- function(dat) {
        dat$last_date_row <- INT_NA;
        pos_change <- which(dat$engulfing != 0);
        dat$last_date_row[pos_change] <- pos_change;
        dat$last_date_row <- na.locf(dat$last_date_row, na.rm = FALSE);
        dat$n_engulfing <- as.integer(1:nrow(dat) - dat$last_date_row);
        dat %>% 
            select(-last_date_row);
    }    
    
    calcEngulfing <- function(dat) { 
        dat$engulfing <- mapply(
            testEngulfing,
            dat$open,
            dat$high,
            dat$low,
            dat$close,
            c(NUM_NA, head(dat$open, -1)),
            c(NUM_NA, head(dat$high, -1)),
            c(NUM_NA, head(dat$low, -1)),
            c(NUM_NA, head(dat$close, -1)),
            dat$vol_lt
        );

        dat %>%
            addNbDaysSinceEngulfing;
        
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    U.try(calcEngulfing, dat_ohlc)(dat_ohlc);
}
T.addFXPriority <-
function (dat_ohlc) 
{
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    dat_work <- dat_ohlc;
    
    if (!("pair" %in% colnames(dat_work))) {
        dat_work <- dat_work %>%
            left_join(select(INSTRUMENTS, pair, instrument_id), by = "instrument_id");
    }
    if (!("asset_class" %in% colnames(dat_work))) {
        dat_work <- dat_work %>%
            left_join(select(INSTRUMENTS, asset_class, instrument_id), by = "instrument_id");
    }
    
    dat_ohlc$ccy_1_reference <- 1;
    dat_ohlc$ccy_1_reference[which(dat_work$asset_class == "index")] <- -1
    dat_ohlc$ccy_1_reference[which(dat_work$asset_class == "metal")] <- -1
    dat_ohlc$ccy_1_reference[which(dat_work$asset_class %in% c("yield","bond"))] <- 0
    
    pairs_fx_todo <- c("AUDJPY", "AUDUSD", "BTCUSD", "GBPUSD", "NZDJPY", "NZDUSD")
    dat_ohlc$ccy_1_reference[which(dat_work$pair %in% pairs_fx_todo)] <- -1;
    
    pairs_fx_todo <- 
        c("AUDNZD", "EURGBP", "EURJPY", "EURCAD", "EURUSD", "GBPJPY", "AUDCAD", "AUDCHF", "CHFJPY")
    dat_ohlc$ccy_1_reference[which(dat_work$pair %in% pairs_fx_todo)] <- 0;
    
    dat_ohlc;
}
T.addLTVol <-
function(dat_ohlc, period_type = "D") {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    days_vol_lt <- filter(TECH_PARAM, technical_param == "vol_days_lt")$value
    n_periods_year <- 
        filter(TECH_PARAM, technical_param == paste0("vol_n_periods_year_", period_type))$value
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_ohlc %>%
        T.addVol(days_vol_lt, n_periods_year) %>%
        rename(vol_lt = vol);
    
}
T.addLTVolAverage <-
function (dat_ohlc, do_only_last = FALSE) 
{
    
    # Very slow and ugly
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    period_type <- "D"
    nb_years_vol_lt_decile <- filter(TECH_PARAM, technical_param == "nb_years_vol_lt_decile")$value
    n_periods_year <- 
        filter(TECH_PARAM, technical_param == paste0("vol_n_periods_year_", period_type))$value

    nb_points_to_keep <- n_periods_year * nb_years_vol_lt_decile;

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    calcLTVolAverage <- function(dat) {
        dat$vol_lt_avg <- NUM_NA;
        dat$vol_st_decile <- NUM_NA;
        for (i in (1+nb_points_to_keep):nrow(dat)) {
            if (!do_only_last | (i >= (nrow(dat) - 21))) {
                dat$vol_lt_avg[i] <- mean(dat$vol_lt[(i-nb_points_to_keep):i], na.rm = TRUE);
                vol_st <- dat$vol_st[i];  
                if (!is.na(vol_st)) {
                    vol_data <- na.omit(dat$vol_st[(i-nb_points_to_keep):i]);
                    n_total <- length(vol_data)
                    if (n_total >= 700) {
                        n_below <- length(which(vol_data <= vol_st));
                        dat$vol_st_decile[i] <- n_below / n_total;
                    }
                }
            }
        }

        dat %>% 
            mutate(
                vol_decile_diff = vol_st_decile - lag(vol_st_decile, 20)
            );
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat <- U.try(calcLTVolAverage, dat_ohlc)(dat_ohlc);
}
T.addMACDFLS <-
function (dat_ohlc_spline) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################

    addMACDFLS_Try <- function(dat_ohlc_spline) {
        dat_ohlc_spline %>%
            mutate(
                macdfls = spline_fast / spline_medium - 1,
                macdcross = sign(sign(macdfls) - sign(lag(macdfls, 1)))
            );
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    U.try(addMACDFLS_Try, dat_ohlc_spline)(dat_ohlc_spline)
}
T.addMaxMoveInNextMonths <-
function (dat_ohlc) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    
    ####################################################################################################
    ### Subroutines
    ####################################################################################################
    maxNA <- function(this_vector) max(this_vector, na.rm = TRUE);
    minNA <- function(this_vector) min(this_vector, na.rm = TRUE);
    maxAB <- function(a, b) max(c(a,b))
    
    fillMissingOpenHighLow <- function(dat) {
        dat$open <- dat$open_na;
        dat$high <- dat$high_na;
        dat$low <- dat$low_na;
        dat
    }
    
    rollApplyNext <- function(this_vector, this_width, this_function)
        rollapply(
            data = this_vector, 
            width = this_width, 
            FUN = this_function, 
            align = "left",
            fill = NUM_NA
        ) %>%
        U.vectorize;
    
    maxNext <- function(dat, n) {
        col_name <- paste0("nxt_max_", n)
        dat[col_name] <- dat %>% 
            select(high) %>% 
            mutate(high = lead(high, 1)) %>%
            U.vectorize %>%
            rollApplyNext(n, maxNA);
        dat[col_name] <- log(dat[col_name] / dat$close) / dat$vol_lt;
        dat;
    }
    
    minNext <- function(dat, n) {
        col_name <- paste0("nxt_min_", n)
        dat[col_name] <- dat %>% 
            select(low) %>% 
            mutate(low = lead(low, 1)) %>%
            U.vectorize %>%
            rollApplyNext(n, minNA);
        dat[col_name] <- log(dat[col_name] / dat$close) / dat$vol_lt;
        dat;
    }
    
    maxAmplitude <- function(dat, n) {
        dat[paste0("nxt_max_ampl_", n)] <- mapply(
            maxAB, 
            U.vectorize(dat[paste0("nxt_max_", n)]), 
            -U.vectorize(dat[paste0("nxt_min_", n)])
        );
        dat;
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_work <- fillMissingOpenHighLow(dat_ohlc);
    
    for (n in c(21, 65, 130, 260)) {
        dat_work <- dat_work %>%
            maxNext(n) %>%
            minNext(n) %>%
            maxAmplitude(n);
    }
    dat_work$open <- dat_ohlc$open;
    dat_work$high <- dat_ohlc$high;
    dat_work$low <- dat_ohlc$low;
    
    dat_work;

    
}
T.addMeanReversionMomentum <-
function(dat_ohlc) {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    days_vol_lt <- filter(TECH_PARAM, technical_param == "vol_days_lt")$value
    days_vol_st <- filter(TECH_PARAM, technical_param == "vol_days_st")$value

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    addHurst_Try <- function(x) hurstexp(x, length(x), FALSE)$Hrs;
    addHurst <- function(x) U.try(addHurst_Try, NUM_NA)(x)
    
    addMomersion_Try <- function(x) {
       dx <- diff(x);
       mc <- sum(dx > 0);
       mrc <- sum(dx < 0);
       mc / (mc + mrc);
    }
    addMomersion <- function(x) U.try(addMomersion_Try, NUM_NA)(x)
    
    calcMR_Try <- function(x) {
        dr <- log(tail(x, -1) / head(x, -1));
        dr_1 <- tail(dr, -1);
        dr_0 <- head(dr, -1);
        lm(dr_1 ~ dr_0)$coefficients["dr_0"] %>% U.vectorize;
    }
    calcMR <- function(x) 
        U.try(calcMR_Try, NUM_NA)(x);
    ####################################################################################################
    ### Script
    ####################################################################################################

    dat_ohlc$hurst_st <- T.rollApply(dat_ohlc$close, days_vol_st, addHurst);
    dat_ohlc$hurst_lt <- T.rollApply(dat_ohlc$close, days_vol_lt, addHurst);

    dat_ohlc$momersion_st <- T.rollApply(dat_ohlc$close, days_vol_st, addMomersion);
    dat_ohlc$momersion_lt <- T.rollApply(dat_ohlc$close, days_vol_lt, addMomersion);

    dat_ohlc$mr_st <- T.rollApply(dat_ohlc$close, days_vol_st, calcMR);
    dat_ohlc$mr_lt <- T.rollApply(dat_ohlc$close, days_vol_lt, calcMR);

    dat_ohlc;
}
T.addNewHighsOrLows <-
function (dat_ohlc) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    n_days <- 65;
    n_days_month <- 20;

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    fillMissingOpenHighLow <- function(dat) {
        dat$open <- dat$open_na;
        dat$high <- dat$high_na;
        dat$low <- dat$low_na;
        dat
    }
    
    isNewHigh <- function(this_vector) {
        as.integer(tail(this_vector, 1) > max(head(this_vector, -1)));
    }
    isNewLow <- function(this_vector) isNewHigh(-this_vector);
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    dat <- fillMissingOpenHighLow(dat_ohlc);
    dat_ohlc$new_hi <- T.rollApply(dat$high, n_days, isNewHigh); 
    dat_ohlc$new_lo <- T.rollApply(dat$low, n_days, isNewLow);
    dat_ohlc$nb_new_hi_last_month <- T.rollApply(dat_ohlc$new_hi, n_days_month, sum);
    dat_ohlc$nb_new_lo_last_month <- T.rollApply(dat_ohlc$new_lo, n_days_month, sum);
    dat_ohlc;
    
}
T.addNextMonthTrend <-
function (dat_ohlc) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################

    fillMissingOpenHighLow <- function(dat) {
        dat$open <- dat$open_na;
        dat$high <- dat$high_na;
        dat$low <- dat$low_na;
        dat
    }
    
    calcNextMonthTrendDate <- function(dat_i) {
        px_t <- head(dat_i$close, 1);
        vol_t <- head(dat_i$vol_lt, 1);
        
        lm_fit <- dat_i %>% 
            select(date, open, high, low, close) %>% 
            gather(type, price, -date) %>%
            lm(price ~ date, data = .);
        
        r2 <- summary(lm_fit)$r.squared %>% atanh;
        trend_slope <-  U.vectorize(lm_fit$coefficients[2]) / px_t / vol_t;
        c(r2, trend_slope)
    }
    
    calcNextMonthTrend <- function(dat) {
        dat <- fillMissingOpenHighLow(dat);
        dat$trend_next_month_r2 <- NUM_NA;
        dat$trend_slope <- NUM_NA;
        
        for (i in 1:(nrow(dat) - 21)) {
            dat_i <- dat[i:(i+21),]; 
            dat_trend <- U.try(calcNextMonthTrendDate, c(NUM_NA, NUM_NA))(dat_i);
            dat$trend_next_month_r2[i] <- dat_trend[1];
            dat$trend_slope[i] <- dat_trend[2];
        }
        
        dat$open <- dat_ohlc$open;
        dat$high <- dat_ohlc$high;
        dat$low <- dat_ohlc$low;
        
        dat;
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    U.try(calcNextMonthTrend, dat_ohlc)(dat_ohlc);
}
T.addOvernightStats <-
function (dat_ohlc) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    n_list <- c(5, 20, 65, 130, 260);
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    addOvernight <- function(dat, n) {
        dat$cumul_overnight <- T.rollApply(dat$gap, n, sum);
        col_name <- paste0("perf_ovn_", n);
        dat <- dat %>% 
            mutate(
                close_t_n = lag(close, n),
                tmp_xyz = (cumul_overnight - (close - close_t_n)) / (close_t_n * vol_lt)
            ) %>%
            select(-cumul_overnight, -close_t_n);
        colnames(dat)[which(colnames(dat) == "tmp_xyz")] <- col_name;
        dat;
    }
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    dat <-  dat_ohlc %>%
        mutate(
            gap = open - lag(close, 1)
        ) %>%
        select(-starts_with("perf_ovn_"))

    for (n in n_list) {
        dat <- U.try(addOvernight, dat)(dat, n);
    }
    
    dat %>%
        select(-gap);
    
}
T.addPareto <-
function(dat_ohlc) {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    instrument_id <- dat_ohlc$instrument_id[1];
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    dat_pareto <- 
        "SELECT *
        FROM histo_pareto
        WHERE instrument_id = %s" %>%
        sprintf(instrument_id) %>% 
        D.select %>%
        arrange(date) %>%
        group_by(date) %>%
        summarize(
            pl_alpha_left = first(alpha_left),
            pl_l_left = first(l_left),
            pl_alpha_right = first(alpha_right),
            pl_l_right = first(l_right)
        ) %>%
        ungroup;

    ####################################################################################################
    ### Script
    ####################################################################################################

    cols_to_keep <- setdiff(
        colnames(dat_ohlc), 
        c("pl_alpha_left", "pl_alpha_right", "pl_l_left", "pl_l_right", 
          "pl_l_width", "pl_alpha_ratio")
        );
    dat_ohlc[, cols_to_keep] %>%
        left_join(dat_pareto, by = "date") %>% 
        mutate(
            pl_alpha_left = na.locf0(pl_alpha_left),
            pl_l_left = na.locf0(pl_l_left),
            pl_alpha_right = na.locf0(pl_alpha_right),
            pl_l_right = na.locf0(pl_l_right)
        ) %>%
        mutate(
            pl_l_width = (pl_l_right - pl_l_left) / vol_lt,
            pl_alpha_ratio = pl_alpha_right / pl_alpha_left
        );

}
T.addRecentHiLo <-
function (dat_ohlc) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    n_days_list <- c(5, 20, 65, 130, 260);

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    fillMissingOpenHighLow <- function(dat) {
        dat$open <- dat$open_na;
        dat$high <- dat$high_na;
        dat$low <- dat$low_na;
        dat;
    }
    
    calcColumn <- function(dat, col_name, n, this_function, new_col_name) {
        this_col_name <- paste0(new_col_name, "_", n);
        dat[this_col_name] <- T.rollApply(dat_work[col_name], n, this_function);
        if (new_col_name %in% c("hi", "lo")) {
            dat[this_col_name] <- log(dat[this_col_name] / dat$close) / dat$vol_lt;
        }
        dat;
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    dat_work <- fillMissingOpenHighLow(dat_ohlc);
    
    for (n in n_days_list) {
        dat_ohlc <- dat_ohlc %>% 
            calcColumn("low", n, min, "lo") %>%
            calcColumn("low", n, T.whichMinLast, "n_lo") %>%
            calcColumn("high", n, max, "hi") %>%
            calcColumn("high", n, T.whichMaxLast, "n_hi");
        high <- dat_ohlc[paste0("hi_", n)];
        low <- dat_ohlc[paste0("lo_", n)];
        vol <- dat_ohlc$vol_lt;
        dat_ohlc[paste0("pos_in_range_", n)] <- 
            (exp(-low * vol) - 1) / (exp((high - low) * vol) - 1);
        dat_ohlc;
    }
    
    dat_ohlc %>%
        mutate(
            hi_vs_hi_20 = high_close - hi_20,
            lo_vs_lo_20 = low_close - lo_20
            );
}
T.addRecentTrend <-
function (dat_ohlc, do_only_last = FALSE) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    res_na <- c(NUM_NA, NUM_NA, NUM_NA);
    nb_days_list <- c(10, 21, 130);
    nb_seconds_in_one_year <- 31536000;
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    fillMissingOpenHighLow <- function(dat) {
        if ("open_na" %in% colnames(dat)) {
            dat$open <- dat$open_na;
            dat$high <- dat$high_na;
            dat$low <- dat$low_na;
        }
        dat;
    }

    addDateTime <- function(dat) {
        dat$date_time <- paste0(dat$date, " 14:00:00") %>% 
            as.POSIXct;
        pos_open <- which(dat$type == "open");
        dat$date_time[pos_open] <- paste0(dat$date, " 07:00:00") %>% 
            as.POSIXct;
        pos_close <- which(dat$type == "close");
        dat$date_time[pos_close] <- paste0(dat$date, " 20:00:00") %>% 
            as.POSIXct;
        dat;
    }
    
    calcRecentTrendOneDate <- function(dat_i) {
        px_t <- tail(dat_i$close, 1);
        vol_t <- tail(dat_i$vol_lt, 1);
        lm_fit_data <- dat_i %>% 
            select(date_time, open, high, low, close) %>%
            gather(type, price, -date_time);

        lm_fit <- lm(price ~ date_time, data = lm_fit_data);

        dw <- dwtest(lm_fit)
        dw_s <- dw$statistic %>% U.vectorize;

        r2 <- summary(lm_fit)$r.squared %>% atanh;

        trend_slope <-  U.vectorize(lm_fit$coefficients[2]) / px_t / vol_t * nb_seconds_in_one_year;

        c(r2, trend_slope, dw_s) %>% U.vectorize;
    }
    
    calcTrendOneRow_Try <- function(i, n, dat, dat_empty) {
        dat_trend <- dat_empty;
        if ((i >= (n+1)) & (!do_only_last | (i == nrow(dat)))) {
            dat_i <- dat[(i-n):i,];
            dat_trend <- U.try(calcRecentTrendOneDate, res_na)(dat_i) %>%
                t %>% 
                data.frame %>%
                U.data2Tibble;
            colnames(dat_trend) <- colnames(dat_empty);
        }
        dat_trend;
        
    }
    calcTrendOneRow <- function(i, n, dat, dat_empty) 
        U.try(calcTrendOneRow_Try, dat_empty)(i, n, dat, dat_empty)
    
    calcRecentTrend_Try <- function(dat) {
        for (n in nb_days_list) {
            col_r2 <- "trend_%s_r2" %>% sprintf(n);
            col_slope <- "trend_%s_slope" %>% sprintf(n);
            col_dw_s <- "trend_%s_dw_s" %>% sprintf(n);

            dat_empty <- rep(NUM_NA, 3) %>%
                t %>% 
                data.frame %>%
                U.data2Tibble;
            colnames(dat_empty) <- c(col_r2, col_slope, col_dw_s);
            calcTrend_i_local <- function(i) calcTrendOneRow(i, n, dat, dat_empty);
            
            dat_trend <- 1:nrow(dat) %>%
                lapply(calcTrend_i_local) %>%
                bind_rows;
            dat <- cbind(dat, dat_trend);

        }
        dat;
    }
    calcRecentTrend <- function(dat)
        U.try(calcRecentTrend_Try, dat)(dat);
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_work <- dat_ohlc %>%
        select(date, open, high, low, close, vol_lt) %>%
        fillMissingOpenHighLow %>%
        addDateTime %>%
        calcRecentTrend %>%
        select(-date_time, -open, -high, -low, -close, -vol_lt);

    dat_ohlc %>%
        left_join(dat_work, by = "date");
    
}
T.addSimpleStuff <-
function (dat_ohlc) 
{
    ####################################################################################################
    ### Sub routines
    ####################################################################################################

    ####################################################################################################
    ### Script
    ####################################################################################################
    
    dat_ohlc %>% 
        mutate(
            psi_1 = log(close / lag(close, 1)) / vol_lt,
            psi_5 = log(close / lag(close, 5)) / vol_lt,
            psi_20 = log(close / lag(close, 20)) / vol_lt,
            psi_60 = log(close / lag(close, 20)) / vol_lt,
            psi_130 = log(close / lag(close, 130)) / vol_lt,
            psi_260 = log(close / lag(close, 260)) / vol_lt,
            open_close = log(open / close) / vol_lt,
            high_close = log(high / close) / vol_lt,
            low_close = log(low / close) / vol_lt,
            vol_ratio = log(vol_st / vol_lt),
            vol_ratio_diff_vs_yesterday = vol_ratio - lag(vol_ratio, 1),
            gap_open = log(open / lag(close, 1)) / vol_lt,
            psi_5_yesterday = lag(psi_5, 1),
            volstdiff = vol_st - lag(vol_st, 20),
            volltdiff = vol_lt - lag(vol_lt, 20)
        );
}
T.addSkewKurtosis <-
function(dat_ohlc) {
    # SHOULDN'T THIS BE APPLIED TO DAILY MOVES RATHER THAN CLOSE PRICE ????
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    days_vol_lt <- filter(TECH_PARAM, technical_param == "vol_days_lt")$value
    days_vol_st <- filter(TECH_PARAM, technical_param == "vol_days_st")$value

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    normalityTest_Try <- function(x) log(jarque.bera.test(x)$statistic)
    normalityTest <- function(x) U.try(normalityTest_Try, NUM_NA)(x)

    ####################################################################################################
    ### Script
    ####################################################################################################

    # SHOULDN'T THIS BE APPLIED TO DAILY MOVES RATHER THAN CLOSE PRICE ????
    px_close <- dat_ohlc$close

    dat_ohlc$kurtosis_st <- T.rollApply(px_close, days_vol_st, kurtosis)
    dat_ohlc$skew_st <- T.rollApply(px_close, days_vol_st, skewness)
    dat_ohlc$normality_st <- T.rollApply(px_close, days_vol_st, normalityTest)
    
    dat_ohlc$kurtosis_lt <- T.rollApply(px_close, days_vol_lt, kurtosis)
    dat_ohlc$skew_lt <- T.rollApply(px_close, days_vol_lt, skewness)
    dat_ohlc$normality_lt <- T.rollApply(px_close, days_vol_lt, normalityTest)
    
    dat_ohlc;
}
T.addSpline <-
function (dat_ohlc, speed, period_type) 
{
    ####################################################################################################
    ### Script variables
    sqt <- 1 / sqrt(
        filter(TECH_PARAM, technical_param == paste0("vol_n_periods_year_", period_type))$value
        )
    nb_points_spline <- filter(TECH_PARAM, technical_param == "spline_nb_points")$value
    df_spline <- filter(TECH_PARAM, technical_param == paste0("spline_df_", speed))$value
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    keepOnlyMinimumNbPoints <- function(dat) {
        dat %>% 
            tail(nb_points_spline)
    }
    
    calcPriceValueToSpline <- function(dat) {
        dat %>%
            mutate(tp = (close + high + low) / 3) %>% 
            select(date, tp)
    }
    
    calcSplineVectorForMinimumPoints <- function(dat) {
        dat$spline <- dat %>% 
            calcPriceValueToSpline %>%
            smooth.spline(df = df_spline) %>%
            predict %>%
            .$y
        dat
    }

    calcSplineTable <- function(dat) {
        dat %>% 
            keepOnlyMinimumNbPoints %>%
            calcSplineVectorForMinimumPoints %>%
            select(date, spline)
    }
    
    combineWithFullTable <- function(dat, dat_tail) {
        dat %>% 
            left_join(dat_tail, by = "date")
    }
    
    addSlopeMetrics <- function(dat) {
        dat %>%
            mutate(
                slope = log(spline / lag(spline)) / (vol_lt * sqt),
                accel = slope - lag(slope),
                slope_change = sign(sign(slope) - sign(lag(slope)))
            )
    }

    addNbDaysSinceSlopeSignChanged <- function(dat) {
        dat$last_date_row <- INT_NA
        pos_change <- which(dat$slope_change != 0)
        dat$last_date_row[pos_change] <- pos_change
        dat$last_date_row <- na.locf(dat$last_date_row, na.rm = FALSE)
        dat$n_slope <- as.integer(1:nrow(dat) - dat$last_date_row)
        dat %>% 
            select(-last_date_row)
    }    
    
    renameColumns <- function(dat) {
        new_col_positions <-  ncol(dat) + (-18:0)
        colnames(dat)[new_col_positions] <- paste0(colnames(dat)[new_col_positions], "_", speed)
        dat
    }
    
    addLevels <- function(dat) {
        pos_change <- which(dat$slope_change != 0)
        px_spot <- tail(dat$close, 1)
        px_vol <- tail(dat$vol_lt, 1)
        if (is.na(px_vol) | (length(px_vol) == 0)) {
            px_vol <- tail(na.omit(tail(dat$vol_lt, 10)), 1)
            if (length(px_vol) == 0) {
                px_vol <- 0.1
            }
        }
        px_levels <- dat$spline[pos_change]

        px_levels_left <- px_levels[which(px_levels < px_spot)]
        px_levels_left <- px_levels_left[order(-px_levels_left)]
        px_levels_left <- log(px_levels_left / px_spot) / px_vol
        px_levels_left <- px_levels_left %>% c(seq(-100, -104, -1)) %>% head(5)

        px_levels_right <- px_levels[which(px_levels > px_spot)]
        px_levels_right <- px_levels_right[order(px_levels_right)]
        px_levels_right <- log(px_levels_right / px_spot) / px_vol
        px_levels_right <- px_levels_right %>% c(seq(100, 104, 1)) %>% head(5)

        dat_levels <- c(px_levels_left, px_levels_right) %>% 
            data.frame %>% 
            t %>% 
            U.dataFrame

        colnames_left <- paste0("lvl_spline_l_", 1:5)
        colnames_right <- paste0("lvl_spline_r_", 1:5)
        colnames(dat_levels) <- c(colnames_left, colnames_right)
        dat_levels
        dat <- cbind(dat, dat_levels);
        for (this_col in colnames(dat_levels)) {
            dat[1:(nrow(dat)-1), this_col] <- NUM_NA
        }
        dat
    }
    
    addNbSlopeChangeN <- function(dat, n) {
        pos_change <- which(tail(dat$slope_change, n) != 0)
        dat[paste0("nb_x_slope_chg_", n)] <- length(pos_change)
        dat
    }
    addNbSlopeChange <- function(dat) {
        for (n in c(5, 20, 65, 130)) {
            dat <- addNbSlopeChangeN(dat, n)
        }
        dat
    }
    
    addSpline_Try <- function(dat) {
        dat_tail <- calcSplineTable(dat)
        
        dat %>% 
            combineWithFullTable(dat_tail) %>%
            addSlopeMetrics %>%
            addNbDaysSinceSlopeSignChanged %>%
            addLevels %>%
            addNbSlopeChange %>%
            renameColumns
    }
    ####################################################################################################
    ### Script 
    ####################################################################################################
    U.try(addSpline_Try, dat_ohlc)(dat_ohlc)
}
T.addSplineDivergence <-
function (dat_ohlc) 
{
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    dat_ohlc %>% 
        mutate(
            spline_skew = slope_fast - slope_slow, 
            spline_range = (slope_slow - slope_medium) ^ 2 + (slope_fast - slope_medium) ^ 2, 
            spline_divergence = 0.5*(spline_slow + spline_fast) - spline_medium,
            close_spline = log(close / spline_medium) / vol_st, #vol_st is not an error
            close_spline_slow = log(close / spline_slow) / vol_lt
        ) 
    
}
T.addSTVol <-
function(dat_ohlc, period_type = "D") {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    days_vol_st <- filter(TECH_PARAM, technical_param == "vol_days_st")$value
    n_periods_year <- 
        filter(TECH_PARAM, technical_param == paste0("vol_n_periods_year_", period_type))$value
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_ohlc %>%
        T.addVol(days_vol_st, n_periods_year) %>%
        rename(vol_st = vol);
}
T.addSuperExponentiality <-
function (dat_ohlc, n_days, save_file = FALSE) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    
    fillMissingOpenWithPreviousClose <- function(dat) {
        pos_na <- which(is.na(dat$high))
        dat$high[pos_na] <- dat$close[pos_na];
        pos_na <- which(is.na(dat$low))
        dat$low[pos_na] <- dat$close[pos_na];
        dat;
    }
    
    dat_px <- dat_ohlc %>%
        fillMissingOpenWithPreviousClose %>%
        mutate(
            open = log(open),
            high = log(high),
            low = log(low),
            close = log(close)
        )
    px_date <- dat_px$date;
    px_high <- dat_px$high;
    px_low <- dat_px$low;
    px_close <- dat_px$close;
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    genDatesCombinationsForOneDate <- function(i) {
        i0 <- i-n_days;
        i1 <- i;
        i_list <- t(combn(i0:i1, 2)) %>% U.data2Tibble;
        colnames(i_list) <- c("i0", "i1");
        i_list %>% 
            filter(i1 > i0 + 1);
    }
    
    genAllPossibleDates <- function() {
        (n_days+1):nrow(dat_px) %>%
            lapply(genDatesCombinationsForOneDate) %>%
            bind_rows %>% 
            unique;
    }
    
    calcInterpolations <- function(dat) {
        dat <- dat %>% 
            mutate(
                y0 = px_close[i0],
                y1 = px_close[i1],
                a = (y1 - y0) / (i1 - i0),
                b = y0 - a * i0
            ); 
        
        rep_na <- rep(INT_NA, n_days)
        X <- mapply(
            function(i0, i1) head(c((i0+1):(i1-1), rep_na), n_days-1),
            dat$i0,
            dat$i1
        ) %>%
            t;
        
        A <- dat %>%
            select(a) %>% 
            as.matrix;
        B <- dat %>% 
            select(b) %>% 
            as.matrix;
        
        Y <- X;
        for (j in 1:(n_days-1)) {
            Y[,j] <- A * X[,j] + B
        }
        
        X <- data.frame(X);
        colnames(X) <- paste0("X", 1:ncol(X));
        Y <- data.frame(Y);
        colnames(Y) <- paste0("Y", 1:ncol(Y));
        
        cbind(dat[,1:2],X,Y);
    }
    
    
    compareInterpolationsWithPrices <- function(dat) {
        dat_i <- dat[,1:2];
        dat_X <- as.matrix(select(dat, starts_with("X")));
        dat_interp <- as.matrix(select(dat, starts_with("Y")));
        dat_close <- dat_X;
        dat_high <- dat_X;
        dat_low <- dat_X;
        dat_nmax <- dat_X;
        for (j in 1:ncol(dat_close)) {
            dat_close[,j] <- px_close[dat_X[,j]];
            dat_high[,j] <- px_high[dat_X[,j]];
            dat_low[,j] <- px_low[dat_X[,j]];
            dat_nmax[,j] <- !is.na(dat_X[,j]);
        }
        
        n_above_close <- (dat_interp > dat_close) %>% rowSums(na.rm = TRUE);
        n_above_high <- (dat_interp > dat_high) %>% rowSums(na.rm = TRUE);
        n_below_low <- (dat_interp < dat_low) %>% rowSums(na.rm = TRUE);
        n_max <- rowSums(dat_nmax, na.rm = TRUE);
        
        data.frame(
            dat_i, 
            n_above_close,
            n_above_high,
            n_below_low,
            n_max
        )
    }
    
    testOneDate_Try <- function(i) {
        test_intersections_per_date %>% 
            filter(
                i0 >= i - n_days,
                i1 <= i
            ) %>% 
            summarize(
                nmax = sum(n_max),
                sup_exp_close = 2 * (sum(n_above_close) / nmax - 0.5),
                sup_exp_high = 2 * (sum(n_above_high) / nmax - 0.5) - sup_exp_close + 0.5,
                sub_exp_low = 2 * (0.5 - sum(n_below_low) / nmax) - sup_exp_close - 0.5,
                sup_exp_close = pmax(-0.99999, sup_exp_close),
                sup_exp_high = pmax(-0.99999, sup_exp_high),
                sub_exp_low = pmax(-0.99999, sub_exp_low),
                sup_exp_close = pmin(0.99999, sup_exp_close),
                sup_exp_high = pmin(0.99999, sup_exp_high),
                sub_exp_low = pmin(0.99999, sub_exp_low),
                sup_exp_close = atanh(sup_exp_close),
                sup_exp_high = atanh(sup_exp_high),
                sub_exp_low = atanh(sub_exp_low)
            ) %>%
            mutate(date = px_date[i]) %>% 
            select(date, sup_exp_close, sup_exp_high, sub_exp_low);
    }
    
    testOneDate <- function(i) 
        U.try(testOneDate_Try, NULL)(i)
    
    saveFile <- function(dat) {
        this_pair <- dat_ohlc$pair[1];
        file_name <- "%sSpot/SuperExponentiality/%s.csv" %>% 
            sprintf(DIRECTORY_DATA_HD, this_pair);
        if (save_file) {
            dat %>% 
                select(date, sup_exp_close, sup_exp_high, sub_exp_low) %>%
                U.write.csv(file_name);
        }
        
        dat;
    }
    
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    dat_i_list <- genAllPossibleDates();
    
    dat <- calcInterpolations(dat_i_list);
    
    test_intersections_per_date <- compareInterpolationsWithPrices(dat);
    
    test_intersections <- (n_days+1):nrow(dat_px) %>%
        lapply(testOneDate) %>%
        bind_rows;
    
    cols_keep <- setdiff(
        colnames(dat_ohlc), 
        c("sup_exp_close", "sup_exp_high", "sub_exp_low", "sup_exp_close_lagged", "sup_exp_close_chg")
    );
    
    dat_ohlc[,which(colnames(dat_ohlc) %in% cols_keep)] %>%
        left_join(test_intersections, by = "date") %>%
        mutate(
            sup_exp_close_lagged = lag(sup_exp_close, 5),
            sup_exp_close_chg = sup_exp_close - lag(sup_exp_close_lagged, 5)
        ) %>%
        saveFile;
    
}
T.addTargetLevels <-
function(dat, strat_id = NULL) {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    target_width <- 2;
    if (!is.null(strat_id)) {
        BB_WIDTH <- D.loadTable("static_trade_bb_width")
        this_bb_width_id <- filter(STRATEGIES, strategy_id == strat_id)$bb_width_id
        target_width <- filter(BB_WIDTH, bb_width_id == this_bb_width_id)$bb_width / 100
    }
    
    ####################################################################################################
    ### Sub
    ####################################################################################################
    
    dat %>% 
        mutate(
            t_up = close + target_width * (bb_up - spline_medium),
            t_dn = close + target_width * (bb_dn - spline_medium)
        );
}
T.addTechnicalZones <-
function (dat_ohlc) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    nb_years_vol_lt_decile <- filter(TECH_PARAM, technical_param == "nb_years_vol_lt_decile")$value
    spline_nb_points <- filter(TECH_PARAM, technical_param == "spline_nb_points")$value
    
    n <- spline_nb_points * nb_years_vol_lt_decile;

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    fillMissingOpenHighLow <- function(dat) {
        if ("open_na" %in% colnames(dat)) {
            dat$open <- dat$open_na;
            dat$high <- dat$high_na;
            dat$low <- dat$low_na;
        }
        dat;
    }
    
    calcLevels_Try <- function(this_date) {
        dat <- dat_work %>% 
            filter(date <= this_date);
    
        res <- NULL;
        if (nrow(dat) > n) {
            dat <- tail(dat, n+1);
            this_date <- tail(dat$date, 1);
            px_spot <- tail(dat$close, 1);
            px_max <- max(dat$high);
            px_min <- min(dat$low);
            vol_lt <- tail(dat$vol_lt, 1);

            if (is.na(vol_lt)) {
                vol_lt <- tail(na.omit(tail(dat$vol_lt, 10)), 1);
                if (length(vol_lt) == 0) {
                    vol_lt <- 0.1;
                }
            }

            step_increment <- px_spot * 2 * vol_lt / 100;
            n_steps <- ceiling((px_max - px_min) / step_increment);

            dat_histogram <- dat %>% 
                select(open, high, low, close) %>%
                tail(n) %>%
                U.vectorize %>%
                hist(breaks = n_steps, plot = FALSE);

            dat_histogram <- data.frame(
                spot = dat_histogram$mids,
                count = dat_histogram$counts
            )
            dat_minmax <- data.frame(
                spot = c(max(dat$high), min(dat$low), mean(dat$close), median(dat$close)),
                count = n
            )

            dat_extra <- data.frame(
                spot = px_spot * (1 + (-50:50)/100),
                count = 1
            )

            dat_histogram <- dat_histogram %>% 
                rbind(dat_minmax) %>%
                rbind(dat_extra) %>%
                arrange(-count);

            dat_left <- dat_histogram %>%
                filter(
                    spot < px_spot,
                    count > 0
                ) %>%
                head(10) %>% 
                select(spot) %>%
                arrange(-spot) %>%
                mutate(spot = log(spot / px_spot) / vol_lt) %>%
                U.vectorize;
            dat_left <- dat_left %>% c(seq(-100, -104, -1)) %>% head(5);
            dat_left <- dat_left %>%     
                t %>% 
                data.frame;

            dat_right <- dat_histogram %>%
                filter(
                    spot > px_spot,
                    count > 0
                ) %>%
                head(10) %>% 
                select(spot) %>%
                arrange(spot) %>%
                mutate(spot = log(spot / px_spot) / vol_lt) %>%
                U.vectorize;
            dat_right <- dat_right %>% c(seq(100, 104, 1)) %>% head(5);
            dat_right <- dat_right %>% 
                t %>% 
                data.frame;
            
            res <- cbind(dat_left, dat_right);
            colnames(res) <- paste0("lvl_", c(paste0("l_", 1:5), paste0("r_", 1:5)));
            res <- data.frame(date = this_date, res);
        }
        
        res;
    }
    
    calcLevels <- function(this_date) 
        U.try(calcLevels_Try, NULL)(this_date)
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################

    dat_work <- fillMissingOpenHighLow(dat_ohlc);

    dat_levels <- dat_ohlc$date %>%
        lapply(calcLevels) %>% bind_rows;

    dat_ohlc %>% 
        left_join(dat_levels, by = "date");
}
T.addTradingData <-
function (dat_ohlc, n_days_trade, bb_width_trade) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################

    ####################################################################################################
    ### Sub routines
    ####################################################################################################

    hitHiLo_Try <- function(dat, hi_lo) {
        dat <- data.frame(dat);
        tgt <- dat[1,2];
        dat <- tail(dat, -1);
        rownames(dat) <- NULL;
        pos_hit <- which(hi_lo * (dat[,1] - tgt) >= 0)[1];
        if (is.na(pos_hit)) {
            pos_hit <- 1000;
        } 
        pos_hit;
    }
    hitHiLo <- function(dat, hi_lo)
        U.try(hitHiLo_Try, NUM_NA)(dat, hi_lo)
    
    hitHi <- function(dat) hitHiLo(dat, 1);
    hitLo <- function(dat) hitHiLo(dat, -1);
    
    calcHitHi <- function(dat) {
        dat$hit_hi <- rollapply(
            data = select(dat, high, t_up),
            width = n_days_trade + 1,
            FUN = hitHi,
            align = "left",
            by.column = FALSE,
            fill = NUM_NA
        )
        dat;
    }
    
    calcHitLo <- function(dat) {
        dat$hit_lo <- rollapply(
            data = select(dat, low, t_dn),
            width = n_days_trade + 1,
            FUN = hitLo,
            align = "left",
            by.column = FALSE,
            fill = NUM_NA
        );
        dat;
    }
    
    determineTradeOutcome <- function(dat) {
        dat$tgt <- CHAR_NA;
        dat$tgt[which(dat$hit_lo == 1000 & dat$hit_hi == 1000)] <- "flat";
        dat$tgt[which(dat$hit_lo == 1000 & dat$hit_hi < 1000)] <- "up";
        dat$tgt[which(dat$hit_hi == 1000 & dat$hit_lo < 1000)] <- "down";
        dat$tgt[which(dat$hit_hi < 1000 & dat$hit_lo < 1000 & dat$hit_hi < dat$hit_lo)] <- "up";
        dat$tgt[which(dat$hit_hi < 1000 & dat$hit_lo < 1000 & dat$hit_hi > dat$hit_lo)] <- "down";
        dat$tgt[which(dat$hit_hi < 1000 & dat$hit_lo < 1000 & dat$hit_hi == dat$hit_lo)] <- "unknown";
        dat$tgt[which(is.na(dat$t_up) | is.na(dat$t_dn))] <- CHAR_NA;
        dat;
    }
    
    calcTradeDurationAndExitPrice <- function(dat) {
        #THIS IS UGLY
        dat$duration <- NUM_NA;
        dat$px_exit <- NUM_NA;
        dat$date_exit <- DATE_NA;
        
        for (i in 1:nrow(dat)) {
            if (is.na(dat$tgt[i])) {}
            else if (dat$tgt[i] == "flat") {
                dat$px_exit[i] <- dat$close[i + n_days_trade];
                dat$duration[i] <- n_days_trade;
                dat$date_exit[i] <- dat$date[i + n_days_trade];
            }
            else if (dat$tgt[i] == "up") {
                dat$px_exit[i] <- max(dat$open[i + dat$hit_hi[i]], dat$t_up[i]);
                dat$duration[i] <- dat$hit_hi[i];
                dat$date_exit[i] <- dat$date[i + dat$duration[i]];
            }
            else if (dat$tgt[i] == "down") {
                dat$px_exit[i] <- min(dat$open[i + dat$hit_lo[i]], dat$t_dn[i]);
                dat$duration[i] <- dat$hit_lo[i];
                dat$date_exit[i] <- dat$date[i + dat$duration[i]];
            }
        }
        dat;
    }
    
    finalFormatting <- function(dat) {
        dat <- dat %>% 
            mutate(rtn = px_exit / close - 1);
        dat$tgt <- as.factor(dat$tgt);
        dat;
    }

    calcTradingStrategy <- function(dat) {
        dat %>% 
            mutate(
                t_up = close + bb_width_trade * (bb_up - spline_medium),
                t_dn = close + bb_width_trade * (bb_dn - spline_medium)
            ) %>%
            calcHitHi %>%
            calcHitLo %>%
            determineTradeOutcome %>%
            calcTradeDurationAndExitPrice %>%
            finalFormatting;
    }

    ####################################################################################################
    ### Script
    ####################################################################################################
    U.try(calcTradingStrategy, dat_ohlc)(dat_ohlc);

}
T.addTradingDataAllPossibilities <-
function (fx_pair = NULL) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    DURATION <- D.loadTable("static_trade_max_duration")
    BB_WIDTH <- D.loadTable("static_trade_bb_width")

    n_days_list <- DURATION$max_duration %>% U.vectorize
    bb_width_list <- U.vectorize(BB_WIDTH$bb_width) / 100
    n_days_list <- 5
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    calcTradingDataOnePair_Try <- function(this_pair) {
        instrument_id <- A.getInstrumentId(this_pair);
        dat_0 <- T.getTechnicals(this_pair) %>% 
            select(instrument_id, date, open, high, low, close, vol_lt, spline_medium, bb_up, bb_dn);

        for (n_days in n_days_list) {
            for (bb_width in bb_width_list) {
                U.printBanner("%s - %s - %s" %>% sprintf(this_pair, n_days, bb_width), FALSE);

                dat_0 %>%
                    T.addTradingData(n_days, bb_width) %>%
                    mutate(
                        max_duration = n_days,
                        bb_width = as.integer(100 * bb_width),
                        outcome = tgt
                    ) %>% 
                    left_join(DURATION, by = "max_duration") %>%
                    left_join(BB_WIDTH, by = "bb_width") %>%
                    left_join(TRADE_OUTCOMES, by = "outcome")  %>% 
                    select(
                        instrument_id, 
                        date, 
                        bb_width_id,
                        max_duration_id,
                        outcome_id,                        
                        t_up,
                        t_dn,
                        hit_hi,
                        hit_lo,
                        date_exit,
                        px_exit,
                        duration,
                    ) %>% 
                    na.omit %>% U.debug(fx_pair) %>%
                    D.replaceDataIntoTable("histo_trade_outcome", .);
                
                gc();
            }
        }
    }
    
    calcTradingDataOnePair <- function(this_pair) 
        U.try(calcTradingDataOnePair_Try)(this_pair)
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    if (is.null(fx_pair)) {
        PAIR_LIST <- filter(INSTRUMENTS, (use_for_training + use_for_trading >= 1))$pair
        for (this_pair in PAIR_LIST) {
            calcTradingDataOnePair(this_pair)
        }
    }
    else {
        calcTradingDataOnePair(fx_pair)
    }
    
}
T.addVol <-
function(dat_ohlc, nb_days, nb_days_vol_annual = 260) {
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    fillMissingOpenWithPreviousClose <- function(dat) {
        if ("high_na" %in% colnames(dat)) {
            dat$high <- dat$high_na;
            dat$low <- dat$low_na;
        }
        dat;
    }
    
    calcVol_Try <- function(dat) {
        dat_xts <- dat %>% 
            fillMissingOpenWithPreviousClose %>%
            select(date, open, high, low, close) %>%
            T.dfToXts;
        
        volatility(
            dat_xts, 
            nb_days, 
            calc = "yang.zhang", 
            N = nb_days_vol_annual
        ) %>%
            U.replaceNaNWithNA;
    }
    calcVol <- function(dat)
        U.try(calcVol_Try, NUM_NA)(dat)
    
    addVol <- function(dat) {
        dat$vol <- calcVol(dat)
        dat;
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_ohlc %>%
        addVol;
}
T.addVolDifferences <-
function (dat_ohlc, period_type, do_only_last = FALSE) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    n_days_histo_vol <- filter(TECH_PARAM, technical_param == "vol_days_st")$value
    n_days_vol_lt <- filter(TECH_PARAM, technical_param == "vol_days_lt")$value
    n_days_year_vol <- 
        filter(TECH_PARAM, technical_param == paste0("vol_n_periods_year_", period_type))$value
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    fillMissingOpenHighLow <- function(dat) {
        dat$open <- dat$open_na;
        dat;
    }
    
    transformToXts <- function(dat) {
        dat %>% 
            select(date, open, high, low, close) %>% 
            T.dfToXts;
    }
    
    calcVolGeneric <- function(dat_xts, vol_type) {
        volatility(
            dat_xts, 
            n_days_histo_vol, 
            calc = vol_type, 
            N = n_days_year_vol
        ) %>%
            U.vectorize;
    }

    calcMAD <- function(dat_ohlc, n_days_vol) {
        dat_ohlc %>%
            mutate(rtn = log(close / lag(close, 1))) %>%
            .$rtn %>%
            U.vectorize %>%
            T.rollApply(n_days_vol, mad);
    }

    calcCloseToCloseVol <- function(dat_xts) calcVolGeneric(dat_xts, "close")
    calcHiLoVol <- function(dat_xts) calcVolGeneric(dat_xts, "parkinson")
    calcGKVol <- function(dat_xts) calcVolGeneric(dat_xts, "garman.klass")
    calcRSVol <- function(dat_xts) calcVolGeneric(dat_xts, "rogers.satchell")
    calcGKYZVol <- function(dat_xts) calcVolGeneric(dat_xts, "gk.yz")
    
    calcVolRatioUpDn_Try <- function(this_day, n_days_vol, max_date, dat_px) {
        vol_ratio_up_dn <- NUM_NA;
        if (!do_only_last | (this_day == max_date)) {
            dat_local <- dat_px %>%
                filter(date <= this_day) %>%
                tail(n_days_vol) %>%
                mutate(move = sign(close - lag(close, 1)));
            
            if (nrow(dat_local) == n_days_vol) {
                vol_days_up <- NUM_NA;
                vol_days_dn <- NUM_NA;
                
                dat_local_up <- dat_local %>% 
                    filter(move == 1) %>%
                    select(date, open, high, low, close);
                
                if (nrow(dat_local_up) >= 2) {
                    vol_days_up <- volatility(
                        T.dfToXts(dat_local_up), 
                        nrow(dat_local_up) - 1,
                        "yang.zhang",
                        n_days_year_vol
                        ) %>%
                        U.vectorize %>%
                        last;
                }
                
                dat_local_dn <- dat_local %>% 
                    filter(move == -1) %>%
                    select(date, open, high, low, close);
                
                if (nrow(dat_local_dn) >= 2) {
                    vol_days_dn <- volatility(
                        T.dfToXts(dat_local_dn), 
                        nrow(dat_local_dn) - 1,
                        "yang.zhang",
                        n_days_year_vol
                    ) %>%
                        U.vectorize %>%
                        last;
                }
                vol_ratio_up_dn <- log(vol_days_up / vol_days_dn)
                if (length(vol_ratio_up_dn) == 0) {
                    vol_ratio_up_dn <- NUM_NA;
                }
                if (is.infinite(vol_ratio_up_dn)) {
                    vol_ratio_up_dn <- NUM_NA;
                }
            }
        }
        vol_ratio_up_dn;
    }
    calcVolRatioUpDn <- function(this_day, n_days_vol, max_date, dat_px) 
        U.try(calcVolRatioUpDn_Try, NUM_NA)(this_day, n_days_vol, max_date, dat_px)
    
    calcCloseToCloseVolAssumingZeroMean <- function(dat_xts) {
        volatility(dat_xts, n_days_histo_vol, calc = "close", N = n_days_year_vol, mean0 = TRUE) %>% 
            U.vectorize;
    }
    
    compareVolatilities <- function(this_vol, vol_ref) {
        vol_compare <- log(this_vol / vol_ref)
        vol_compare[which(is.infinite(vol_compare))] <- NUM_NA
        vol_compare
    }

    calcVolDifferences <- function(dat) {
        dat_work <- fillMissingOpenHighLow(dat);
        dat_xts <- transformToXts(dat_work);

        vol_close <- calcCloseToCloseVol(dat_xts);
        vol_close_mean0 <- calcCloseToCloseVolAssumingZeroMean(dat_xts);
        vol_hilo <- calcHiLoVol(dat_xts);
        vol_gk <- calcGKVol(dat_xts);
        vol_rs <- calcRSVol(dat_xts);
        vol_gkyz <- calcGKYZVol(dat_xts);

        mad_st <- calcMAD(dat, n_days_histo_vol) * sqrt(n_days_year_vol)
        mad_lt <- calcMAD(dat, n_days_year_vol) * sqrt(n_days_year_vol)

        max_date <- max(dat$date);

        calcVolRatioUpDnLT <- function(this_day) 
            calcVolRatioUpDn(this_day, n_days_vol_lt, max_date, dat_work);
        calcVolRatioUpDnST <- function(this_day) 
            calcVolRatioUpDn(this_day, n_days_histo_vol, max_date, dat_work);

        dat$vol_ratio_up_dn_st <- U.try(U.sapply, NUM_NA)(dat$date, calcVolRatioUpDnST);
        dat$vol_ratio_up_dn_lt <- U.try(U.sapply, NUM_NA)(dat$date, calcVolRatioUpDnLT);
        
        dat$vol_c_vs_yz = compareVolatilities(vol_close, dat$vol_st);
        dat$vol_0_vs_trend = compareVolatilities(vol_close_mean0, vol_close);
        dat$vol_hilo_vs_yz <- NULL;
        dat$vol_gk_vs_yz <- NULL;
        dat$vol_rs_vs_yz <- NULL;
        dat$vol_gkz_vs_yz <- NULL;
        dat$vol_hilo_vs_c <- compareVolatilities(vol_hilo, vol_close);
        dat$vol_gk_vs_c <- compareVolatilities(vol_gk, vol_close);
        dat$vol_rs_vs_c <- compareVolatilities(vol_rs, vol_close);
        dat$vol_gkz_vs_c <- compareVolatilities(vol_gkyz, vol_close);

        dat$mad_st_vs_vol_c <- compareVolatilities(mad_st, vol_close);
        dat$mad_lt_vs_vol_lt <- compareVolatilities(mad_lt, dat$vol_lt);

        dat %>%
            mutate(
                vol_st_20 = log(vol_st / lag(vol_st, 20))    
            );
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    U.try(calcVolDifferences, dat_ohlc)(dat_ohlc);

}
T.adjustHighLowWithMinMaxBeforeClose <-
function (dat_px) 
{
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    # Formula expected maximum of geometric brownian motion
    # https://www.ntu.edu.sg/home/nprivault/MA5182/maximum-brownian-motion.pdf
    # page 19
    
    time_now <- Sys.time()
    vol_default <- 0.1

    ####################################################################################################
    ### Sub Routines
    ####################################################################################################

    addVolStWhenNoVol <- function(dat) {
        dat_vol <- 
            "SELECT instrument_id, value AS vol_st
            FROM live_technicals_dbl T
            LEFT JOIN static_feature F ON F.feature_id = T.feature_id
            WHERE F.feature = 'vol_st'" %>%
            D.select
        dat <- dat %>%
            left_join(dat_vol, by = "instrument_id")
        dat$vol_st[which(is.na(dat$vol_st))] <- vol_default
        dat
    }
    addVolSt <- function(dat) {
        if (!("vol_st" %in% colnames(dat))) {
            dat <- addVolStWhenNoVol(dat)
        }
        dat
    }
    
    calcExpectedMax_Try <- function(px, vol, time_close) {
        t_exp <- pmax(0, as.numeric(difftime(time_close, time_now, units = "days")) / 365)
        s2t <- t_exp * (vol ** 2)
        sqrt_s2t <- sqrt(s2t)
        px * (2 * (1 + 0.25 * s2t) * pnorm(0.5 * sqrt_s2t) + sqrt_s2t / sqrt(2*pi) * exp(-s2t / 8))
    }
    
    calcExpectedMin_Try <- function(px, vol, time_close) 
        1 / calcExpectedMax(1/px, vol, time_close)
    
    calcExpectedMax <- function(px, vol, time_close) 
        U.try(calcExpectedMax_Try, NUM_NA)(px, vol, time_close)
    calcExpectedMin <- function(px, vol, time_close) 
        U.try(calcExpectedMin_Try, NUM_NA)(px, vol, time_close)
    
    ####################################################################################################
    ### Script
    ####################################################################################################

    market_times <- dat_px$instrument_id %>%
        U.vectorizeUnique %>%
        T.instrumentOpenCloseTimes %>%
        select(instrument_id, time_close)

    dat <- dat_px %>%
        addVolSt %>%
        left_join(market_times, by = "instrument_id")

    dat$expected_max <- mapply(calcExpectedMax, dat$close, dat$vol_st, dat$time_close)
    dat$expected_min <- mapply(calcExpectedMin, dat$close, dat$vol_st, dat$time_close)

    dat <- dat %>%
        mutate(
            high_expected = pmax(high, expected_max, na.rm = TRUE),
            low_expected = pmin(low, expected_min, na.rm = TRUE)
        ) %>%
        select(instrument_id, date, high_expected, low_expected)

    dat_px %>% 
        left_join(dat, by = c("instrument_id", "date")) %>%
        mutate(
            high = case_when(!is.na(high_expected) ~ high_expected, TRUE ~ high),
            low = case_when(!is.na(low_expected) ~ low_expected, TRUE ~ low)
        ) %>%
        select(-high_expected, -low_expected)

    
}
T.calcHistoricalCorrelationsMatrix <-
function(
    instrument_ids = NULL,
    lookback_weeks = 104,
    shrinkage = 0.1,
    as_of_date = NULL
) {
    ####################################################################################################
    ### Script description:
    ### Calculates a correlation matrix of weekly returns across instruments.
    ### Uses Friday-to-Friday returns to handle asynchronous market closes (Tokyo/London/NY).
    ### Applies shrinkage toward identity matrix for numerical stability in optimization.
    ####################################################################################################

    ####################################################################################################
    ### Script variables
    ####################################################################################################
    if (is.null(as_of_date)) {
        as_of_date <- YESTERDAY
    }

    lookback_days <- lookback_weeks * 7 + 30  # extra buffer for weekends/holidays
    start_date <- as_of_date - lookback_days

    ####################################################################################################
    ### Sub routines
    ####################################################################################################

    loadPrices <- function() {
        sql <- "SELECT instrument_id, date, close
                FROM histo_px_daily
                WHERE date >= '%s' AND date <= '%s'" %>%
            sprintf(start_date, as_of_date)

        dat <- D.select(sql)

        if (!is.null(instrument_ids)) {
            dat <- dat %>% filter(instrument_id %in% instrument_ids)
        }

        dat
    }

    calcWeeklyReturns <- function(dat_prices) {
        # Prefer Tuesday (weekday 2) to avoid Friday data release noise
        # Fall back to closest available day if Tuesday not available
        dat_prices %>%
            mutate(
                weekday = lubridate::wday(date, week_start = 1),
                year_week = format(date, "%Y-%W")
            ) %>%
            group_by(instrument_id, year_week) %>%
            # Prefer Tuesday (weekday 2), else closest day to Tuesday
            mutate(dist_to_tuesday = abs(weekday - 2)) %>%
            filter(dist_to_tuesday == min(dist_to_tuesday)) %>%
            # If tie (e.g., Mon and Wed both dist=1), take later day
            filter(date == max(date)) %>%
            ungroup() %>%
            select(-dist_to_tuesday) %>%
            arrange(instrument_id, date) %>%
            group_by(instrument_id) %>%
            mutate(
                close_prev = lag(close),
                weekly_return = log(close / close_prev)
            ) %>%
            ungroup() %>%
            filter(!is.na(weekly_return)) %>%
            select(instrument_id, year_week, date, weekly_return)
    }

    pivotToWideFormat <- function(dat_returns) {
        # Create wide matrix: rows = weeks, cols = instruments
        dat_returns %>%
            select(year_week, instrument_id, weekly_return) %>%
            pivot_wider(
                names_from = instrument_id,
                values_from = weekly_return
            ) %>%
            arrange(year_week) %>%
            select(-year_week)
    }

    calcCorrelationMatrix <- function(dat_wide) {
        # Only keep instruments with sufficient data (at least 50% of weeks)
        min_obs <- nrow(dat_wide) * 0.5
        cols_with_data <- colSums(!is.na(dat_wide)) >= min_obs
        dat_filtered <- dat_wide[, cols_with_data, drop = FALSE]

        if (ncol(dat_filtered) < 2) {
            warning("Not enough instruments with sufficient data for correlation matrix")
            return(NULL)
        }

        # Pairwise complete correlation
        cor_matrix <- cor(dat_filtered, use = "pairwise.complete.obs")

        # Handle any remaining NAs (set to 0 = uncorrelated)
        cor_matrix[is.na(cor_matrix)] <- 0

        cor_matrix
    }

    applyShrinkage <- function(cor_matrix, shrinkage_factor) {
        # Shrink toward identity: (1 - shrinkage) * cor + shrinkage * I
        n <- nrow(cor_matrix)
        identity_matrix <- diag(n)

        shrunk_matrix <- (1 - shrinkage_factor) * cor_matrix + shrinkage_factor * identity_matrix

        # Preserve row/col names
        rownames(shrunk_matrix) <- rownames(cor_matrix)
        colnames(shrunk_matrix) <- colnames(cor_matrix)

        shrunk_matrix
    }

    ####################################################################################################
    ### Script
    ####################################################################################################

    dat_prices <- loadPrices()

    if (is.null(dat_prices) || nrow(dat_prices) == 0) {
        warning("No price data found for correlation calculation")
        return(NULL)
    }

    dat_weekly <- calcWeeklyReturns(dat_prices)
    dat_wide <- pivotToWideFormat(dat_weekly)
    cor_matrix <- calcCorrelationMatrix(dat_wide)

    if (is.null(cor_matrix)) {
        return(NULL)
    }

    if (shrinkage > 0) {
        cor_matrix <- applyShrinkage(cor_matrix, shrinkage)
    }

    cor_matrix
}
T.calcPareto <-
function(dat_ohlc, nb_days, use_already_done = TRUE) {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    time_max <- 13 * 60
    start_time <- Sys.time()
    end_time <- start_time + time_max
    
    pl_na <- data.frame(
        alpha_left = NUM_NA, 
        l_left = NUM_NA, 
        alpha_right = NUM_NA, 
        l_right = NUM_NA
    )
    this_instrument_id <- dat_ohlc$instrument_id[1]

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    readPareto_Try <- function() {
        "SELECT *
        FROM histo_pareto
        WHERE instrument_id = %s" %>%
            sprintf(this_instrument_id) %>%
            D.select(TRUE, FALSE) %>%
            unique %>%
            group_by(date) %>%
            summarize(
                alpha_left = first(alpha_left),
                l_left = first(l_left),
                alpha_right = first(alpha_right),
                l_right = first(l_right)
            ) %>%
            ungroup
    }
    readPareto <- function() 
        U.try(readPareto_Try)()
    
    findCorrespondingDateInSchedule <- function(this_date, dat) {
        dat %>%
            anti_join(dat_pareto_db, by = "date") %>%
            select(date) %>%
            arrange(date) %>%
            filter(date <= this_date) %>%
            tail(1) %>% 
            .$date %>% 
            as.character
    }
    
    findCorrespondingDatesInSchedule <- function(dates_list, dat) {
        findDateLocal <- function(this_date) findCorrespondingDateInSchedule(this_date, dat);
        U.sapply(dates_list, findDateLocal) %>%
            as.Date
    }
    
    prepareDateList <- function(dat_work) {
        min_date <- min(dat_work$date)
        max_date <- max(dat_work$date)

        date_list <- 
            if (!U.dfContainsData(dat_pareto_db)) {
                c(min_date, max_date, sample_n(dat_work, nrow(dat_work))$date) %>% unique
            }
        else {
            data.frame(date = c(min_date, max_date)) %>% 
                U.data2Tibble %>% 
                rbind(dat_pareto_db %>% na.omit %>% select(date)) %>% 
                arrange(date) %>% 
                rename(from = date) %>%
                mutate(to = lead(from, 1)) %>%
                na.omit %>% 
                mutate(diff = as.numeric(to - from)) %>%
                arrange(-diff) %>% 
                mutate(date = from + ceiling(diff / 2)) %>% 
                .$date %>%
                findCorrespondingDatesInSchedule(dat_work) %>%
                unique
        }
        U.printBanner("Dates list")
        print(head(date_list, 100))
        date_list
    }
    
    calibratePareto <- function(dat, left_right) {
        if (left_right == "left") {
            dat <- dat %>% 
                mutate(rtn = -rtn_dn) %>%
                filter(rtn > 0)
        }
        
        else if (left_right == "right") {
            dat <- dat %>% 
                mutate(rtn = rtn_up) %>%
                filter(rtn > 0)
        }
        
        pl_distribution <- conpl$new(dat$rtn)
        pl_estimation <- estimate_xmin(pl_distribution)
        pl_distribution$setXmin(pl_estimation)
        
        dat_pl <- data.frame(
            alpha = pl_distribution$pars,
            l = pl_distribution$xmin
        );
        
        colnames(dat_pl) <- paste0(colnames(dat_pl), "_", left_right)
        dat_pl
    }
    
    minPx <- function(x) min(tail(x, -1))
    maxPx <- function(x) max(tail(x, -1))
    
    calcParetoDay_Try <- function(this_day) {
        U.printBanner(paste0(this_instrument_id, " - ", this_day), FALSE)
        dat <- dat_ohlc_work
        dat$highest_n <- rollapply(
            data = dat$high, 
            width = nb_days, 
            FUN = maxPx, 
            align = "left", 
            fill = NUM_NA
        ) %>%
            U.vectorize;
        dat$lowest_n <- rollapply(
            data = dat$low, 
            width = nb_days, 
            FUN = minPx, 
            align = "left",
            fill = NUM_NA) %>%
            U.vectorize
        
        dat <- dat %>% 
            mutate(
                rtn_up = highest_n / close - 1,
                rtn_dn = lowest_n / close - 1,
                rtn_up = lag(rtn_up, nb_days),
                rtn_dn = lag(rtn_dn, nb_days)
            )
        
        dat <- dat %>%
            filter(date <= this_day)

        dat_left <- calibratePareto(dat, "left")
        dat_right <- calibratePareto(dat, "right")
        data.frame(date = this_day) %>%
            cbind(dat_left) %>% 
            cbind(dat_right) %>%
            U.data2Tibble
    }
    
    calcParetoDay <- function(this_day) 
        U.try(calcParetoDay_Try, data.frame(date = this_day, pl_na))(this_day)
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_pareto_db <- readPareto()
    dat_ohlc_work <- T.fillMissingHighAndLowWithClose(dat_ohlc)
    dates_list <- prepareDateList(dat_ohlc_work)
    i <- 1;
    dat_pareto <- NULL;
    while ((i <= length(dates_list)) & (Sys.time() <= end_time)) {
        this_date <- dates_list[i]
        dat_pareto <- rbind(dat_pareto, calcParetoDay(this_date))
        i <- i+1
    }

    dat_save <- dat_pareto_db %>%
        rbind(dat_pareto) %>%
        arrange(date) %>%
        unique %>%
        group_by(date) %>%
        summarize(
            alpha_left = first(alpha_left),
            l_left = first(l_left),
            alpha_right = first(alpha_right),
            l_right = first(l_right)
        ) %>% 
        ungroup %>%
        mutate(instrument_id = this_instrument_id) %>%
        select(instrument_id, date, alpha_left, l_left, alpha_right, l_right) %>%
        na.omit

    D.replaceDataIntoTable("histo_pareto", dat_save)

    days_histo <- "SELECT COUNT(*) FROM histo_px_daily WHERE instrument_id = %s" %>% 
        sprintf(this_instrument_id) %>%
        D.select %>%
        U.vectorize

    days_pareto <- nrow(dat_save)
    days_ratio <- days_pareto / days_histo
    
    "UPDATE status_instrument 
        SET pareto_last_update = '%s', pareto_date_coverage_ratio =  %s 
        WHERE instrument_id = %s" %>%
        sprintf(as.character(Sys.time()), days_ratio, this_instrument_id) %>%
        D.SQL

    dat_save
}
T.calcTechnicals <-
function (fx_pair, only_recent = FALSE) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    period_type <- "D"
    
    nb_years_vol_lt_decile <- filter(TECH_PARAM, technical_param == "nb_years_vol_lt_decile")$value
    days_vol_lt <- filter(TECH_PARAM, technical_param == "vol_days_lt")$value
    bb_width <- filter(TECH_PARAM, technical_param == "bb_width")$value
    spline_nb_points <- filter(TECH_PARAM, technical_param == "spline_nb_points")$value
    n_row <- 21 + nb_years_vol_lt_decile * days_vol_lt
    
    n_vol_annual <- filter(TECH_PARAM, technical_param == "n_periods_year")$value
    recent_days <-  130 + n_row

    ####################################################################################################
    ### Script sub functions
    ####################################################################################################
    
    finalCleanData <- function(dat) {
        dat <- dat %>% 
            U.dataFrame
        
        dat0 <- dat_histo %>%
            rename(open_f = open, high_f = high, low_f = low) %>%
            select(date, open_f, high_f, low_f)
        
        dat <- dat %>%
            left_join(dat0, by = "date") %>%
            mutate(open = open_f, high = high_f, low = low_f) %>%
            select(-open_f, -high_f, -low_f)
        
        dat <- dat[,setdiff(colnames(dat), c("open_na", "low_na", "high_na"))]
        
        for (j in 1:ncol(dat)) {
            dat[which(is.nan(dat[,j])),j] <- NA
            dat[which(is.infinite(dat[,j])),j] <- NA
        }
        
        dat$asset_class <- factor(
            U.vectorize(dat$asset_class), 
            levels = c("fx_dm", "fx_em", "index", "metal", "yield", "bond")
        );

        dat;
    }
    
    keepOnlyRecentDataIfNecessary <- function(dat) {
        if (only_recent) {
            dat <- dat %>% 
                tail(recent_days)
        }
        dat
    }
    
    addTechnicals <- function() {
        dat_histo %>% 
            U.printMilestone("Starting...") %>%
            U.printMilestone("Keeping only necessary data...") %>%
            keepOnlyRecentDataIfNecessary %>% U.printMilestone("Filling missing NAs...") %>%
            T.fillMissingOpenWithPreviousClose %>%
            T.fillMissingHighAndLowWithClose %>% U.printMilestone("Adding Volatility...") %>%
            T.addSTVol(period_type) %>%
            T.addLTVol(period_type) %>% 
            U.data2Tibble %>% U.printMilestone("Adding Splines...") %>%
            T.addAllSplines %>% U.printMilestone("Add Bollinger Bands...") %>%
            T.addBollingerBandsFLS(bb_width, period_type) %>% 
            U.printMilestone("Add simple stuff...") %>%
            T.addSimpleStuff %>% U.printMilestone("Add date related...") %>%
            T.addDateRelated %>% U.printMilestone("Add asset class...") %>%
            T.addAssetClass %>% U.printMilestone("Add FX Priority...") %>%
            T.addFXPriority %>% U.printMilestone("Add Technical Zones...") %>%
            T.addTechnicalZones %>% U.printMilestone("Add Vol Differences...") %>%
            T.addVolDifferences(period_type) %>% U.printMilestone("Add LT Vol average...") %>%
            T.addLTVolAverage %>% U.printMilestone("Add Skew and Kurtosis...") %>%
            T.addSkewKurtosis %>% U.printMilestone("Add Mean Reversion and Momentum...") %>%
            T.addMeanReversionMomentum %>% U.printMilestone("Add Recent Hi Lo...") %>%
            T.addRecentHiLo %>% U.printMilestone("Add Biggest Recent Candle...") %>%
            T.addBiggestRecentCandle %>% U.printMilestone("Add Engulfing...") %>%
            T.addEngulfing %>%U.printMilestone("Add Overnight Stats...") %>%
            T.addOvernightStats %>% U.printMilestone("Add Shadows...") %>%
            T.addCandleShadowsMeasure %>% U.printMilestone("Add Making New Hi Lo...") %>%
            T.addNewHighsOrLows %>% U.printMilestone("Add Recent Trends...") %>%
            T.addRecentTrend(FALSE) %>% U.printMilestone("Add Next Month Trends...") %>%
            T.addNextMonthTrend %>% U.printMilestone("Add Autocorrelation...") %>%
            T.addAutoCorrelation %>% U.printMilestone("Add Spline Divergence...") %>%
            T.addSplineDivergence %>% U.printMilestone("Add Max Move Next Month...") %>%
            T.addMaxMoveInNextMonths %>% U.printMilestone("Add Pareto...") %>%
            T.addPareto %>% U.printMilestone("Add superexponentiality...") %>%
            T.addSuperExponentiality(20, FALSE) %>% U.printMilestone("Finishing...") %>%
            finalCleanData %>% U.printMilestone("Finished...")
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    dat_histo <- T.getHistoPx(fx_pair)
    
    addTechnicals()
}
T.calcTechnicalsFull <-
function (fx_pair) 
{
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    tech_file_name <- "%sSpot/Technicals_P/Technicals_%s.RData" %>%
        sprintf(DIRECTORY_DATA_HD, fx_pair)
    
    FEATURES <- D.loadTable("static_feature")
    
    instrument_id = A.getInstrumentId(fx_pair)
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    calcTech <- function() T.calcTechnicals(fx_pair)
    
    saveToFile_Try <- function(dat) {
        save(dat, file = tech_file_name)
        dat
    }
    saveToFile <- function(dat)
        U.try(saveToFile_Try, dat)(dat, tech_file_name)

    formatDataForDB <- function(dat) {
        dat %>%
            gather(feature, value, -instrument_id, -date) %>%
            na.omit %>%
            left_join(FEATURES, by = "feature") %>%
            filter(!is.na(feature_id))
    }
    
    deleteDBTechnicalsBeforeSaving <- function(int_or_dbl) {
        "DELETE
        FROM histo_technicals_%s
        WHERE instrument_id = %s" %>%
            sprintf(int_or_dbl, instrument_id) %>%
            D.SQL
    }
    
    saveTechnicalsToDB <- function(dat, int_or_dbl) {
        U.printBanner("Now saving to DB", FALSE)
        deleteDBTechnicalsBeforeSaving(int_or_dbl)
        feature_type <- switch(int_or_dbl, "int" = 0, "dbl" = 1)
        db_tbl_name <- paste0("histo_technicals_", int_or_dbl)
        dat %>%
            filter(int_or_dbl == feature_type) %>%
            select(instrument_id, date, feature_id, value) %>%
            D.insertDataIntoTable(db_tbl_name, .)
        U.printBanner("Saving to DB done", FALSE)
        dat
    }
    
    updateTechnicalsStatusInDB <- function(dat){
        "UPDATE status_instrument
        SET histo_technicals_last_full_recompute = '%s'
        WHERE instrument_id = %s" %>%
            sprintf(
                format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
                instrument_id
            ) %>%
            D.SQL
        dat
    }
    
    doTechnicalsFull_Try <- function() {
        calcTech() %>%
            saveToFile %>%
            formatDataForDB %>%
            saveTechnicalsToDB("int") %>%
            saveTechnicalsToDB("dbl") %>%
            updateTechnicalsStatusInDB
    }
    doTechnicalsFull <- function()
        U.try(doTechnicalsFull_Try)()
    
    removeFirstPareto <- function() {
        initial_date <- "SELECT MIN(date) AS date FROM histo_px_daily WHERE instrument_id = %s" %>%
            sprintf(instrument_id) %>%
            D.select %>% 
            .$date;
        first_ok_date <- initial_date %m+% years(1);
        features_pareto <- FEATURES %>%
            filter(feature %in% c(
                "pl_alpha_left", "pl_l_left", "pl_alpha_right", "pl_l_right", 
                "pl_l_width", "pl_alpha_ratio"
            )
            ) %>% 
            select(feature_id) %>% 
            U.vectorize
        
        "DELETE FROM histo_technicals_dbl
        WHERE instrument_id = %s
        AND feature_id IN (%s)
        AND date <= '%s'" %>%
            sprintf(
                instrument_id,
                paste(features_pareto, collapse = ", "),
                first_ok_date
                ) %>%
            D.SQL
    }
    
    computeAllTradingConfigurations <- function() {
        U.printBanner("Computing trade outcomes", FALSE)
        U.try(T.addTradingDataAllPossibilities)(fx_pair)
    }

    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    doTechnicalsFull()
    removeFirstPareto()
    
    computeAllTradingConfigurations()
    gc()
    NULL
}
T.calcTechnicalsFull_TMP <-
function (fx_pair) 
{
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    tech_file_name <- "%sSpot/Technicals_P/Technicals_%s.RData" %>%
        sprintf(DIRECTORY_DATA_HD, fx_pair)
    
    FEATURES <- D.loadTable("static_feature")
    
    instrument_id = A.getInstrumentId(fx_pair)
    

    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    U.printBanner(fx_pair, FALSE)
    
    calcTech <- function() {
        this_instrument_id <- A.getInstrumentId(fx_pair)
        DAT_TECHNICALS %>% filter(instrument_id == this_instrument_id)
    }
    
    saveToFile <- function(dat) {
        save(dat, file = tech_file_name)
        dat
    }

    formatDataForDB <- function(dat) {
        dat %>%
            gather(feature, value, -instrument_id, -date) %>%
            na.omit %>%
            left_join(FEATURES, by = "feature") %>%
            filter(!is.na(feature_id))
    }
    
    deleteDBTechnicalsBeforeSaving <- function(int_or_dbl) {
        "DELETE
        FROM histo_technicals_%s
        WHERE instrument_id = %s" %>%
            sprintf(int_or_dbl, instrument_id) %>%
            D.SQL
    }
    
    saveTechnicalsToDB <- function(dat, int_or_dbl) {
        U.printBanner(paste0("Now saving to DB - ", int_or_dbl), FALSE)
        deleteDBTechnicalsBeforeSaving(int_or_dbl)
        feature_type <- switch(int_or_dbl, "int" = 0, "dbl" = 1)
        db_tbl_name <- paste0("histo_technicals_", int_or_dbl)
        dat %>%
            filter(int_or_dbl == feature_type) %>%
            select(instrument_id, date, feature_id, value) %>%
            D.insertDataIntoTable(db_tbl_name, .)
        U.printBanner("Saving to DB done", FALSE)
        dat
    }
    
    updateTechnicalsStatusInDB <- function(dat){
        "UPDATE status_instrument
        SET histo_technicals_last_full_recompute = '%s'
        WHERE instrument_id = %s" %>%
            sprintf(
                format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
                instrument_id
            ) %>%
            D.SQL
        dat
    }
    
    doTechnicalsFull_Try <- function() {
        calcTech() %>%
            saveToFile %>%
            formatDataForDB %>%
            saveTechnicalsToDB("int") %>%
            saveTechnicalsToDB("dbl") %>%
            updateTechnicalsStatusInDB
    }
    doTechnicalsFull <- function()
        U.try(doTechnicalsFull_Try)()
    
    removeFirstPareto <- function() {
        initial_date <- "SELECT MIN(date) AS date FROM histo_px_daily WHERE instrument_id = %s" %>%
            sprintf(instrument_id) %>%
            D.select %>% 
            .$date;
        first_ok_date <- initial_date %m+% years(1);
        features_pareto <- FEATURES %>%
            filter(feature %in% c(
                "pl_alpha_left", "pl_l_left", "pl_alpha_right", "pl_l_right", 
                "pl_l_width", "pl_alpha_ratio"
            )
            ) %>% 
            select(feature_id) %>% 
            U.vectorize
        
        "DELETE FROM histo_technicals_dbl
        WHERE instrument_id = %s
        AND feature_id IN (%s)
        AND date <= '%s'" %>%
            sprintf(
                instrument_id,
                paste(features_pareto, collapse = ", "),
                first_ok_date
                ) %>%
            D.SQL
    }
    
    computeAllTradingConfigurations <- function() {
        U.printBanner("Computing trade outcomes", FALSE)
        U.try(T.addTradingDataAllPossibilities)(fx_pair)
    }

    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    doTechnicalsFull()
    removeFirstPareto()
    
    computeAllTradingConfigurations()
    gc()
    NULL
}
T.calcTechnicalsLive <-
function (dat_ohlc) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    period_type <- "D"
    nb_years_vol_lt_decile <- filter(TECH_PARAM, technical_param == "nb_years_vol_lt_decile")$value
    days_vol_lt <- filter(TECH_PARAM, technical_param == "vol_days_lt")$value
    bb_width <- filter(TECH_PARAM, technical_param == "bb_width")$value
    spline_nb_points <- filter(TECH_PARAM, technical_param == "spline_nb_points")$value
    n_row <- 21 + nb_years_vol_lt_decile * days_vol_lt
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_ohlc %>% #U.printMilestone("Filling missing NAs...") %>%
        T.fillMissingHighAndLowWithClose %>%  
        T.fillMissingOpenWithPreviousClose %>% #U.printMilestone("Keeping only enough rows...") %>%
        tail(n_row) %>% #U.printMilestone("Adding volatility...") %>%
        T.addSTVol(period_type) %>% 
        T.addLTVol(period_type) %>% #U.printMilestone("Adding Spline...") %>%
        T.addAllSplines(FALSE, TRUE) %>% #U.printMilestone("Adding BB Bands...") %>%
        T.addBollingerBandsFLS(bb_width, period_type) %>%#U.printMilestone("Adding simple stuff...") %>%
        T.addSimpleStuff %>% #U.printMilestone("Adding asset class...") %>%
        T.addAssetClass %>% #U.printMilestone("Adding fx priority...") %>%
        T.addFXPriority %>% #U.printMilestone("Adding technical zones...") %>%
        T.addTechnicalZones %>% #U.printMilestone("Adding LT vol deciles...") %>%
        T.addLTVolAverage(TRUE) %>% #U.printMilestone("Reducing nb rows ...") %>%
        tail(spline_nb_points + 1) %>% #U.printMilestone("Adding date related...") %>%
        T.addDateRelated %>% #U.printMilestone("Adding vol differences...") %>%
        T.addVolDifferences(period_type, TRUE) %>%# U.printMilestone("Adding skew kurtosis...") %>%
        T.addSkewKurtosis %>%# U.printMilestone("Adding mr + momentum...") %>%
        T.addMeanReversionMomentum %>%# U.printMilestone("Adding recent high lows...") %>%
        T.addRecentHiLo %>%# U.printMilestone("Adding biggest recent candle...") %>%
        T.addBiggestRecentCandle %>%# U.printMilestone("Adding engulfing...") %>%
        T.addEngulfing %>%# U.printMilestone("Adding overnight stats...") %>%
        T.addOvernightStats %>%# U.printMilestone("Adding shadows...") %>%
        T.addCandleShadowsMeasure %>%# U.printMilestone("Adding new highs and lows...") %>%
        T.addNewHighsOrLows %>% # U.printMilestone("Adding recent trend...") %>%
        T.addRecentTrend(TRUE) %>%# U.printMilestone("Adding autocorrelation...") %>%
        T.addAutoCorrelation %>% #U.printMilestone("Adding spline divergence...") %>%
        T.addSplineDivergence %>% #U.printMilestone("Adding super exponentiality...") %>%
        T.addSuperExponentiality(20, FALSE) %>%# U.printMilestone("Adding Pareto...") %>%
        T.addPareto %>%# U.printMilestone("Finishing...") %>%
        select(-open_na, -high_na, low_na) #%>% U.printMilestone("Finished...");

}
T.calcTechnicalsRecentOnly <-
function (pair_list) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    FEATURES <- D.loadTable("static_feature")
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    formatDataForDB_Try <- function(dat) {
        dat %>% U.printMilestone("Formatting for DB...") %>%
            gather(feature, value, -instrument_id, -date) %>%
            na.omit %>%
            left_join(FEATURES, by = "feature") %>%
            filter(!is.na(feature_id), !is.na(instrument_id))
    }
    formatDataForDB <- function(dat) 
        U.try(formatDataForDB_Try)(dat)
    
    deleteDBTechnicalsBeforeSaving_Try <- function(int_or_dbl, max_date, instrument_id) {
        U.printBanner(paste0("Delete recent technicals before writing - ", int_or_dbl), FALSE)
        "DELETE
        FROM histo_technicals_%s
        WHERE instrument_id = %s AND date > '%s'" %>%
            sprintf(int_or_dbl, instrument_id, max_date) %>%
            D.SQL
    }
    deleteDBTechnicalsBeforeSaving <- function(int_or_dbl, max_date, instrument_id)
        U.try(deleteDBTechnicalsBeforeSaving_Try)(int_or_dbl, max_date, instrument_id)
    
    saveTechnicalsToDB_Try <- function(dat, int_or_dbl, max_date) {
        instrument_id <- dat$instrument_id[1]
        deleteDBTechnicalsBeforeSaving(int_or_dbl, max_date, instrument_id)
        feature_type <- switch(int_or_dbl, "int" = 0, "dbl" = 1)
        db_tbl_name <- paste0("histo_technicals_", int_or_dbl)
        dat %>% U.printMilestone(sprintf("Saving technicals %s into DB now", int_or_dbl)) %>%
            filter(int_or_dbl == feature_type) %>% 
            select(instrument_id, date, feature_id, value) %>%
            D.insertDataIntoTable(db_tbl_name, .) %>% U.printMilestone("Saving done")
        dat
    }
    saveTechnicalsToDB <- function(dat, int_or_dbl, max_date)
        U.try(saveTechnicalsToDB_Try, dat)(dat, int_or_dbl, max_date)
    
    updateTechnicalsStatusInDB_Try <- function(dat){
        "UPDATE status_instrument
        SET histo_technicals_updated = 1, histo_technicals_last_update = '%s'
        WHERE instrument_id = %s" %>%
            sprintf(
                format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
                dat$instrument_id[1]
            ) %>%
            D.SQL
        dat
    }
    updateTechnicalsStatusInDB <- function(dat)
        U.try(updateTechnicalsStatusInDB_Try, dat)(dat)
    
    prepareTechnicalsOnePair_Try <- function(this_pair) {
        U.printTickerProgressVerbose(this_pair, pair_list);
        dat <-  T.getTechnicals(this_pair);
        dat_old <- head(dat, -60);
        max_date_old <- max(dat_old$date)
        this_pair %>%
            T.calcTechnicals(TRUE) %>%
            filter(date > max_date_old) %>% U.debug(this_pair) %>%
            formatDataForDB %>%
            saveTechnicalsToDB("int", max_date_old) %>%
            saveTechnicalsToDB("dbl", max_date_old) %>%
            updateTechnicalsStatusInDB
        NULL
    }
    prepareTechnicalsOnePair <- function(this_pair)
        U.try(prepareTechnicalsOnePair_Try)(this_pair)
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    # mclapply(
    #        pair_list, 
    #        prepareTechnicalsOnePair, 
    #       mc.cores = pmax(floor(detectCores() / 2), 1)
    #       );
    lapply(pair_list, prepareTechnicalsOnePair)
}
T.calcTradingStrategyOneTrade <-
function (this_trd, dat_fx, dat_histo_ohlc) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    fx_pair <- filter(INSTRUMENTS, ticker == this_trd$ticker[1])$pair
    this_instrument_id <- A.getInstrumentId(fx_pair)
    buy_sell <- this_trd$buy_sell[1]
    date_start <- this_trd$date_trade[1]
    date_max <- this_trd$date_exit_latest[1]
    
    date_max_tmp <- filter(dat_histo_ohlc, date >= date_max)$date[1]
    if (!is.na(date_max_tmp)) {
        date_max <- date_max_tmp
    }

    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    hitHiLo_Try <- function(px, tgt, hi_lo) {
        px <- U.vectorize(px)
        pos_hit <- which(hi_lo * (px - tgt) >= 0)[1]
        if (is.na(pos_hit)) {
            pos_hit <- 1000
        } 
        pos_hit
    }
    hitHiLo <- function(px, tgt, hi_lo)
        U.try(hitHiLo_Try, NUM_NA)(px, tgt, hi_lo)
    
    hitHi <- function(px, tgt) hitHiLo(px, tgt, 1)
    hitLo <- function(px, tgt) hitHiLo(px, tgt, -1)
    
    determineTradeOutcomePredictFlat <- function(
        dat_trd, dat_histo, hit_hi, hit_lo, px_hi, px_lo, still_before_max_date
    ) {
        px_now <- last(dat_histo$close);
        px_now_flat <- ((px_now <= px_hi) & (px_now >= px_lo))
        
        duration_bizdays <- NUM_NA
        price_exit <- NUM_NA
        date_exit <- DATE_NA
        
        price_status <- 
            if ((hit_lo == 1000) & (hit_hi == 1000)) {
                "inside_range_without_touching_barrier"
            } else {
                ifelse(px_now_flat, "inside_range_but_touched_barrier", "outside_range")
            } 
        if (!still_before_max_date & (price_status == "inside_range_without_touching_barrier")) {
            price_status <- "inside_range_without_touching_barrier"
        }
        
        trade_status <- price_status
        if (still_before_max_date){
            trade_status <- "live"
        }
        
        if (!still_before_max_date) {
            if (trade_status == "outside_range") {
                duration_bizdays <- pmin(hit_lo, hit_hi)
                date_exit <- dat_histo$date[duration_bizdays]
                price_exit <- if (hit_lo < hit_hi) px_lo
                else if (hit_hi > hit_lo) px_hi
                else NUM_NA
            }
            else {
                price_exit <- px_now
                duration_bizdays <- nrow(dat_histo)
                date_exit <- dat_trd$date_exit_latest[1]
            }
        }
        
        cbind(
            this_trd, 
            data.frame(price_status, trade_status, price_exit, duration_bizdays, date_exit)
        )
        
    }
    
    determineTradeOutcomePredictBuySell <- function(
        dat_trd, dat_histo, hit_hi, hit_lo, px_hi, px_lo, still_before_max_date, date_max_histo
    ) {
        duration_bizdays <- NUM_NA
        price_exit <- NUM_NA
        date_exit <- DATE_NA

        price_status <- 
            if ((hit_lo == 1000) & (hit_hi < 1000)) "up" 
        else if ((hit_hi == 1000) & (hit_lo < 1000)) "down" 
        else if ((hit_hi < 1000) & (hit_lo < 1000) & (hit_hi < hit_lo)) "up" 
        else if ((hit_hi < 1000) & (hit_lo < 1000) & (hit_hi > hit_lo)) "down" 
        else if ((hit_hi < 1000) & (hit_lo < 1000) & (hit_hi == hit_lo)) "unknown" 
        else if ((hit_lo == 1000) & (hit_hi == 1000)) ifelse(still_before_max_date, "live", "flat")
        else "error"
        
        trade_status <- 
            if ((buy_sell == 1) & (price_status == "up")) "target"
        else if ((buy_sell == -1) & (price_status == "down")) "target"
        else if ((buy_sell == -1) & (price_status == "up")) "stop"
        else if ((buy_sell == 1) & (price_status == "down")) "stop"   
        else if (price_status == "flat") "exit_maturity"
        else if (price_status == "live") "live"
        else if (price_status == "unknown") "unknown"
        else "error"

        if (trade_status != "error") {
            if (price_status == "flat" & (dat_trd$date_exit_latest[1] <= date_max_histo)) {
                price_exit <- tail(dat_histo$close, 1)
                duration_bizdays <- nrow(dat_histo)
                date_exit <- date_max
            }
            if (price_status == "up") {
                price_exit <- max(dat_histo$open[hit_hi], px_hi)
                duration_bizdays <- hit_hi
                date_exit <- dat_histo$date[duration_bizdays]
            }
            if (price_status == "down") {
                price_exit <- min(dat_histo$open[hit_lo], px_lo)
                duration_bizdays <- hit_lo
                date_exit <- dat_histo$date[duration_bizdays]
            }
        }
        
        cbind(
            this_trd, 
            data.frame(price_status, trade_status, price_exit, duration_bizdays, date_exit)
        )
    }
    
    determineTradeOutcome <- function(dat_trd) {
        dat_histo <- dat_histo_ohlc %>%
            filter(
                instrument_id == this_instrument_id,
                date > date_start,
                date <= date_max
            ) 

        
        
        
        date_max_histo <- max(dat_histo$date)
        still_before_max_date <- date_max_histo < date_max

        px_lo <- this_trd$range_dn[1]
        px_hi <- this_trd$range_up[1]
        hit_hi <- hitHi(dat_histo$high, px_hi)
        hit_lo <- hitLo(dat_histo$low, px_lo)

        if (buy_sell == 0) {
            determineTradeOutcomePredictFlat(
                dat_trd, dat_histo, hit_hi, hit_lo, px_hi, px_lo, still_before_max_date
            ) 
        }
        else {
            determineTradeOutcomePredictBuySell(
                dat_trd, dat_histo, hit_hi, hit_lo, px_hi, px_lo, still_before_max_date, date_max_histo
            )
        }
    }
    
    calcPnL <- function(dat_trd) {
        dat_trd$pnl_ccy_2_pct <- NUM_NA
        dat_trd$pnl_ccy_2_for_notional <- NUM_NA
        dat_trd$fx_ccy_2_usd_exit <- NUM_NA
        dat_trd$pnl_usd_for_notional <- NUM_NA

        ccy_2 = substr(fx_pair, 4, 6)
        notional <- this_trd$notional_ccy1[1]

        date_exit <- dat_trd$date_exit[1]

        if (!is.na(date_exit)) {
            dat_trd$fx_ccy_2_usd_exit[1] <- filter(dat_fx, ccy == ccy_2, date == date_exit)$fx[1]
        }
        if ((dat_trd$buy_sell[1] != 0) & !is.na(dat_trd$trade_status[1]))  {
            dat_trd <- dat_trd %>% 
                mutate(
                    pnl_ccy_2_pct = buy_sell * (price_exit / price_entry - 1),
                    pnl_ccy_2_for_notional = pnl_ccy_2_pct * price_entry * notional_for_1k_pnl,
                    pnl_usd_for_notional = pnl_ccy_2_for_notional * fx_ccy_2_usd_exit
                )
        }
        dat_trd
    }
    
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    this_trd %>% 
        determineTradeOutcome %>%
        calcPnL

}
T.correctHistoBadHighLows <-
function(dat, only_recent = TRUE) 
{
    ####################################################################################################
    ### Script
    ####################################################################################################
    res <- dat
    if (U.dfContainsData(dat)) {
        dat_old <- NULL
        dat_new <- dat
        if (only_recent) {
            dat_old <- filter(dat, date < TO_DAY - 7)
            dat_new <- filter(dat, date >= TO_DAY - 7)
        }
        res <- dat_new %>%
            mutate(
                new_high = pmax(open, high, low, close),
                new_low = pmin(open, high, low, close),
                high = new_high,
                low = new_low
            ) %>%
            select(-new_high, -new_low) %>%
            rbind(dat_old, .) %>%
            arrange(pair, date)
    }
    res
}
T.dailyToPeriod <-
function (xts_ohlc, to_period) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    doNothing <- function(dat) dat
    
    toMonthly <- function(dat) to.monthly(dat, indexAt = "endof");
    toQuarterly <- function(dat) to.quarterly(dat, indexAt = "endof");
    toYearly <- function(dat) to.yearly(dat, indexAt = "endof");
    
    transformToPeriod <- switch(
        to_period,
        "D" = doNothing,
        "W" = to.weekly,
        "M" = toMonthly,
        "Q" = toQuarterly,
        "Y" = toYearly
    )
    
    renameColumns <- function(dat) {
        colnames(dat) <- c("Open", "High", "Low", "Close");
        dat;
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    xts_ohlc %>%
        transformToPeriod %>%
        renameColumns;
    
}
T.detectWrongPriceSeries <-
function(fx_pair, year_from = year(Sys.Date()), year_to = year(Sys.Date())+2) {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    n_days_vol <- 520
    n_days_vol_1y <- 260
    acceptable_band_stdev <- 3
    sqt_1d <- 1/sqrt(260)
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    calcSlidingVol <- function(dat) {
        dat$vol <- volatility(
            as.xts(dat$close, dat$date), 
            n_days_vol, "close", n_days_vol_1y                
        ) %>% U.vectorize
        dat
    }
    
    calcAcceptableBands <- function(dat) {
        dat %>% 
            mutate(
                acceptable_move = acceptable_band_stdev * lag(vol, 1) * sqt_1d,
                acceptable_low = lag(close, 1) * exp(-acceptable_move),
                acceptable_high = lag(close, 1) * exp(acceptable_move),
                low_too_low = low < acceptable_low,
                high_too_high = high > acceptable_high
            )
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat <- fx_pair %>% 
        T.getHistoPx %>% 
        calcSlidingVol %>% 
        calcAcceptableBands %>% 
        filter(year(date) >= year_from, year(date) <= year_to)
    
    max_hole <- max(diff(dat$date))
    print("%s - max_date: %s - max_hole: %s" %>% sprintf(fx_pair, max(dat$date), max_hole))
    
    plot1 <- dat %>% 
        ggplot(aes(x = date, y = close)) + 
        geom_line(color = "lightseagreen") + 
        geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
        geom_point(
            data = dat %>% 
                filter(low_too_low),
            aes(x=date,y=low), 
            color="red", size=5
        ) + 
        geom_point(
            data = dat %>% 
                filter(high_too_high),
            aes(x=date,y=high),
            color="blue", size=5
        ) +
        #   scale_x_date(date_breaks = plot_breaks, date_labels = "%y") + 
        theme(
            legend.position = "bottom",
            axis.text=element_text(size = 10), 
            axis.title.y = element_blank(),
            axis.title.x = element_blank()
        ) +
        ggtitle(fx_pair) 
    
    print(plot1)
    
    dat
    
}
T.determineActiveContracts <-
function () 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    FUTURES <- D.loadTableLocal("future_contract")
    EXPIRIES <- D.loadTableLocal("future_expiry")
    
    DATE_LIMIT <- TO_DAY + 10
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat <- EXPIRIES %>%
        left_join(FUTURES, by = "future_id") %>% 
        left_join(INSTRUMENTS, by = "instrument_id") %>%
        mutate(
            month_expiry = month(expiry),
            liquid_maturity = (month_expiry %% liquid_contract_q_or_m == 0),
            enough_days_remaining = (expiry >= DATE_LIMIT)
        ) %>% 
        arrange(future_id, expiry) %>% 
        group_by(future_id, ib_symbol) %>% 
        mutate(
            metals_filter_out_first = case_when(
                asset_class == "metal" ~ !is.na(lag(future_id, 1)), 
                TRUE ~ TRUE
            )
        ) %>% 
        filter(
            liquid_maturity,
            enough_days_remaining,
            metals_filter_out_first
        ) %>% 
        arrange(future_id, expiry) %>% 
        summarize(expiry = first(expiry), conid = first(conid))
    
    print(data.frame(dat))
    D.SQL("TRUNCATE TABLE static_future_active")
    D.writeTable("static_future_active", select(dat, future_id, conid))
    dat
}
T.dfToXts <-
function (dat_df) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    upperCaseFirstLetterLowerCaseRest <- function(str) {
        paste0(
            toupper(substr(str, 1, 1)),
            tolower(substr(str, 2, nchar(str)))
        )
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    colnames(dat_df) <- upperCaseFirstLetterLowerCaseRest(colnames(dat_df));
    as.xts(
        select(dat_df, -Date),
        as.Date(dat_df$Date)
    );
}
T.downloadHistoOHLC <-
function (live_or_close = "close", pair_list = NULL) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    # reqHistoricalData(a.1, twsCurrency("EUR", currency = "USD"), 
    # whatToShow = "MIDPOINT", barSize = "1 day")
    
    file_name <- paste0(DIRECTORY_DATA_HD, "Spot/Daily/Histo_OHLC.RData");
    
    if (is.null(pair_list)) {
        pair_list <- INSTRUMENTS$pair;
    }
    
    dat_histo_ohlc <- D.loadTable("histo_px_daily") %>%
        left_join(INSTRUMENTS, by = "instrument_id") %>%
        select(pair, date, open, high, low, close);
    
    yahoo_pairs <- c("STISGD")
    
    url_fx <- "https://www.investing.com/currencies/%s-%s-historical-data";
    url_idx <- "https://www.investing.com/indices/%s-historical-data";
    url_yield <- "https://www.investing.com/rates-bonds/%s-year-bond-yield-historical-data";
    url_crypto <- "https://www.investing.com/crypto/%s-historical-data";
    url_etf <- "https://www.investing.com/etfs/%s-historical-data";
    url_btc <- sprintf(url_crypto, "bitcoin/btc-usd")
    
    ib_histo_file <- "%sSpot/IB/%s_histo.csv";
    
    idx_urls <- 
        "SELECT A.instrument_id, A.value FROM 
        static_instrument_attribute_chr A
        LEFT JOIN static_instrument_attribute_type T ON T.attribute_id = A.attribute_id
        WHERE T.attribute = 'url_investing'" %>%
        D.select %>%
        left_join(INSTRUMENTS, by = "instrument_id") %>%
        select(pair, value)
    
    pairs_done <<- NULL
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    genInvestingURL <- function(this_pair) {
        ccy_1 <- tolower(substr(this_pair, 1, 3));
        ccy_2 <- tolower(substr(this_pair, 4, 6));
        
        asset_class <- filter(INSTRUMENTS, pair == this_pair)$asset_class %>% U.vectorize;
        
        url_investing <- CHAR_NA;
        if (this_pair == "BTCUSD") {
            url_investing <- url_btc;
        }
        else if (asset_class %in% c("fx_dm", "fx_em", "metal")) {
            url_investing <- sprintf(url_fx, ccy_1, ccy_2)
        }
        else if (asset_class == "index") {
            url_investing <- url_idx %>%
                sprintf(filter(idx_urls, pair == this_pair)$value);
        }
        else if (asset_class == "yield") {
            url_investing <- url_yield %>% 
                sprintf(filter(idx_urls, pair == this_pair)$value);
            if (this_pair == "A30AUD") {
                url_investing <- gsub("bond-yield-", "", url_investing)
            }
        }
        url_investing;
    }
    
    retrieveWebPage <- function(this_url) {
        print(this_url)
        time_start <- Sys.time();
        time_max <- time_start + 10*60;
        dat <- NULL
        
        while (is.null(dat) & (Sys.time() < time_max)) {
            dat <- U.tryNull(read_html, this_url);
            if (is.null(dat))
                Sys.sleep(10);
        }
        dat;
    }
    
    formatData <- function(dat, this_pair) {
        dat <- dat %>% 
            html_node("#curr_table") %>% 
            html_table;
        colnames(dat)[1:5] <- c("date", "close", "open", "high", "low");
        dat$date <- as.Date(dat$date, format = "%b %d, %Y");
        dat$open <- as.numeric(U.vectorize(gsub(",", "", dat$open)));
        dat$high <- as.numeric(U.vectorize(gsub(",", "", dat$high)));
        dat$low <- as.numeric(U.vectorize(gsub(",", "", dat$low)));
        dat$close <- as.numeric(U.vectorize(gsub(",", "", dat$close)));
        
        dat <- dat %>% 
            mutate(pair = this_pair) %>%
            select(pair, date, open, high, low, close) %>%
            arrange(date) %>%
            U.data2Tibble;
        if (live_or_close == "close") {
            dat <- dat %>% 
                filter(date < TO_DAY);
        }
        dat;
    }
    
    genYahooSymbol_Try <- function(this_pair) {
        ccy_1 <- tolower(substr(this_pair, 1, 3));
        ccy_2 <- tolower(substr(this_pair, 4, 6));
        
        assets_fx <- c(ASSETS_FXDM$ticker, ASSETS_FXEM$ticker);
        assets_idx <- ASSETS_IDX$pair;
        
        yahoo_url <- CHAR_NA;        
        if (this_pair %in% assets_fx) {
            yahoo_url <- toupper(sprintf("%s%s=X", ccy_1, ccy_2))
        }
        else if (this_pair %in% assets_idx) {
            yahoo_url <- ASSETS_IDX$yahoo_ticker[which(ASSETS_IDX$pair == this_pair)[1]];
        }
        yahoo_url;
    }
    genYahooSymbol <- function(this_pair) 
        U.try(genYahooSymbol_Try, NULL)(this_pair)
    
    getLivePriceYahoo_Try <- function(this_pair) {
        res <- NULL;
        if (this_pair == "STISGD") {
            ticks <- "^STI"
            dat_yahoo <- fromJSON(
                paste(
                    "https://query1.finance.yahoo.com/v7/finance/quote?formatted=false&symbols=",
                    paste(ticks, collapse = ","), sep = "")
            )$quoteResponse$result %>%
                select(
                    regularMarketOpen, regularMarketDayHigh, regularMarketDayLow, regularMarketPrice
                ) %>%
                data.frame;
            dat_yahoo <- data.frame(
                pair = this_pair,
                date = TO_DAY,
                dat_yahoo
            ) %>% 
                U.data2Tibble;
            colnames(dat_yahoo)[3:6] <- c("open", "high", "low", "close");
            res <- dat_yahoo;
        }
        res;
    }
    getLivePriceYahoo <- function(this_pair) 
        U.try(getLivePriceYahoo_Try, NULL)(this_pair)
    
    getRecentPairHistoYahoo_Try <- function(this_pair) {
        dat_histo <- this_pair %>% 
            genYahooSymbol %>% 
            getSymbols.yahoo(auto.assign = FALSE, from = TO_DAY %m+% months(-1)) %>% 
            .[,1:4] %>% 
            T.xtsToDf;
        colnames(dat_histo) <- this_pair %>% 
            genYahooSymbol %>%
            gsub("=", ".", .) %>%
            paste0(".") %>% 
            tolower %>%
            gsub("", tolower(colnames(dat_histo)))
        dat_histo <- dat_histo %>%
            mutate(pair = this_pair) %>%
            select(pair, date, open, high, low, close);
    }
    getRecentPairHistoYahoo <- function(this_pair) 
        U.try(getRecentPairHistoYahoo_Try, NULL)(this_pair)
    
    getSTIPriceFromWSJ <- function() {
        dat <- "https://www.wsj.com/market-data/quotes/index/SG/STI/historical-prices" %>% 
            read_html %>%
            html_node("#cr_historical_page") %>% 
            html_text
        dat <- trim(substr(dat, gregexpr("CLOSE", dat)[[1]][1] + 5, nchar(dat))) %>% 
            strsplit(" ") %>% 
            U.vectorize %>% 
            matrix(ncol = 5, byrow = TRUE) %>% 
            data.frame
        colnames(dat) <- c("date", "open", "high", "low", "close")
        dat$date <- as.Date(dat$date, format = "%m/%d/%y")
        dat$open <- as.numeric(dat$open)
        dat$high <- as.numeric(dat$high)
        dat$low <- as.numeric(dat$low)
        dat$close <- as.numeric(dat$close)
        dat$pair <- "STISGD"
        dat %>% 
            select(pair, date, open, high, low, close) %>%            
            arrange(date) %>% 
            U.data2Tibble
    }
    
    
    getRecentPairHistoIBFromFile_Try <- function(this_pair) {
        dat <- ib_histo_file %>% 
            sprintf(DIRECTORY_DATA_HD, this_pair) %>%
            U.tryNull(U.read.csv, .);
        if ("ticker" %in% colnames(dat)) {
            dat <- dat %>% rename(pair = ticker)
        }
        if (max(dat$date) < YESTERDAY) {
            dat <- NULL;
        }
        if (!is.null(dat)) {
            U.printBanner("source: IB", FALSE);
        }
        dat;
    }
    getRecentPairHistoIBFromFile <- function(this_pair) 
        U.tryNull(getRecentPairHistoIBFromFile_Try, this_pair)
    
    getRecentPairHistoOHLC <- function(this_pair) {
        dat <- NULL;
        U.printTickerProgressVerbose(this_pair, pair_list)    
        dat <- getRecentPairHistoIBFromFile(this_pair);    
        
        if (is.null(dat)) {
            if ((this_pair %in% dat_fx_live$pair) & (live_or_close == "live")) {
                U.printBanner("source: fx live", FALSE);
                dat <- dat_histo_ohlc %>% 
                    filter(pair == this_pair) %>% 
                    tail(5) %>%
                    rbind(filter(dat_fx_live, pair == this_pair));
            }
            else if ((this_pair == "STISGD") & (live_or_close == "close")) {
                U.printBanner("source: wsj");
                dat <- getSTIPriceFromWSJ()
            }
            else if ((this_pair %in% yahoo_pairs) & is.null(dat)) {
                U.printBanner("source: yahoo");
                dat <- getRecentPairHistoYahoo(this_pair);
            }
        }
        
        if (is.null(dat)) {
            U.printBanner("source: Manual", FALSE);
            getInvesting <- function(this_pair) {
                this_pair %>% 
                    genInvestingURL %>% 
                    retrieveWebPage %>% 
                    formatData(this_pair)
            }
            
            dat <- U.try(getInvesting, dat_histo_ohlc %>% filter(pair == "this_pair"))(this_pair)
        }
        
        if (live_or_close == "live") {
            if (this_pair == "STISGD") {
                U.printBanner("source: Yahoo", FALSE);
                dat_today <- getLivePriceYahoo(this_pair);
                dat <- rbind(dat %>% filter(date < TO_DAY), dat_today);
            }
            dat <- dat %>% 
                tail(3);
        }
        print(tail(dat, 3) %>% data.frame)
        
        if (!is.null(dat)) {
            pairs_done <<- c(pairs_done, this_pair)
        }
        
        dat;
    }
    
    addToExistingHisto <- function(dat_histo_new, dat_histo_old) {
        dat_histo_old %>% 
            anti_join(dat_histo_new, by = c("pair", "date")) %>%
            rbind(dat_histo_new) %>%
            arrange(pair, date) %>%
            U.data2Tibble;
    }
    
    correctBadHighLows <- function(dat_histo_ohlc) {
        if (live_or_close == "close") {
            dat_old <- NULL
            dat_new <- dat_histo_ohlc
        }
        else if (live_or_close == "live") {
            dat_old <- dat_histo_ohlc %>% 
                filter(date < TO_DAY - 7)
            dat_new <- dat_histo_ohlc %>% 
                filter(date >= TO_DAY - 7)
        }
        
        dat_new %>%
            mutate(
                new_high = pmax(open, high, low, close),
                new_low = pmin(open, high, low, close),
                high = new_high,
                low = new_low
                ) %>%
            select(-new_high, -new_low) %>%
            rbind(dat_old, .) %>%
            arrange(pair, date)

    }
    
    finalFormatting <- function(dat_histo_ohlc) {
        if (live_or_close == "close") {
            dat_old <- NULL
            dat_new <- dat_histo_ohlc %>%
                filter(date <= YESTERDAY)
        }
        else if (live_or_close == "live") {
            dat_old <- dat_histo_ohlc %>% 
                filter(date < TO_DAY - 7)
            dat_new <- dat_histo_ohlc %>% 
                filter(date >= TO_DAY - 7)
        }
        
        dat_new <- dat_new %>% 
            arrange(pair, date) %>% 
            mutate(weekday = weekdays(date)) %>% 
            filter(!(weekday %in% c("Saturday", "Sunday"))) %>% 
            select(-weekday) %>% 
            U.data2Tibble
        dat_new <- dat_new[which(!duplicated(select(dat_new, pair, date))),]
        
        rbind(dat_old, dat_new) %>%
            arrange(pair, date)
    }

    saveToFile <- function(dat_histo_ohlc) {
        if (live_or_close == "close") {
            U.printMilestone("Saving To db now") %>%
                save(dat_histo_ohlc, file = file_name)
        }
        dat_histo_ohlc
    }
    
    saveStatusToDB <- function(this_pair) {
        instrument_id <- A.getInstrumentId(this_pair)
        col_name <- switch(live_or_close, "close" = "histo", "live" = "live") %>%
            paste0("_px_last_update")
        additional_line <- ""
        if (live_or_close == "close") {
            additional_line <- ", histo_px_updated = 1"
        }
        "UPDATE status_instrument
        SET %s = '%s' 
        %s
        WHERE instrument_id = %s" %>%
            sprintf(
                col_name,
                format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
                additional_line,
                instrument_id
                ) %>%
            D.SQL
    }
    
    saveToDB <- function(dat_histo_ohlc) {
        if (live_or_close == "close") {
            dat_histo_ohlc %>% 
                left_join(select(INSTRUMENTS, pair, instrument_id), by = "pair") %>%
                select(instrument_id, date, open, high, low, close) %>%
                filter(!is.na(instrument_id)) %>%
                D.replaceDataIntoTable("histo_px_daily", .)
        }
        lapply(pairs_done, saveStatusToDB)
        dat_histo_ohlc;
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    dat_fx_live <- dat_histo_ohlc[0,];
    if (live_or_close == "live") {
        dat_fx_live <- U.try(T.downloadLiveOHLC, dat_fx_live)();
    }
    pair_list %>%
        lapply(getRecentPairHistoOHLC) %>%
        bind_rows %>% U.printMilestone(1) %>%
        addToExistingHisto(dat_histo_ohlc) %>% U.printMilestone(2) %>%
        correctBadHighLows %>% U.printMilestone(3) %>%
        finalFormatting %>% U.debug("Saving To file now") %>% U.printMilestone(4) %>%
        saveToFile %>% U.printMilestone(5) %>%
        saveToDB %>% U.printMilestone("All done") %>% U.printMilestone(6) 
}
T.downloadIntradayFXSpot <-
function (report_date = NULL) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    if (is.null(report_date)) report_date <- YESTERDAY;
    report_date <- as.Date(report_date);
    
    file_path <- paste0(DIRECTORY_DATA_HD, "Spot/Intraday")
    file_name <- paste0(format(report_date, "%d%m%y"), ".zip")
    zip_file <- paste0(file_path, "/", file_name)
    txt_file <- gsub(".zip", ".txt", zip_file)
    fx_url <- "http://www.forexite.com/free_forex_quotes/%s%s"
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    genUrl <- function() {
        fx_url %>% 
            sprintf(format(report_date, "%Y/%m/"), file_name)
    }
    
    retrieveWebFile <- function(this_url) {
        print(this_url)
        this_url %>% 
            download.file(zip_file)
        zip_file
    }
    
    unzipFile <- function(this_zip_file_name) {
        print(this_zip_file_name)
        this_zip_file_name %>% 
            unzip(overwrite = TRUE, exdir = file_path)
        txt_file
    }
    
    readData <- function(this_txt_file_name) {
        dat_intraday <- U.read.csv(this_txt_file_name)
        colnames(dat_intraday) <- c("pair", "date", "time", "open", "high", "low", "close")
        dat_intraday
    }
    
    formatData <- function(dat) {
        dat$date <- as.Date(as.character(dat$date), "%Y%m%d")
        dat$time <- U.right(paste0("000000", dat$time), 6)
        dat$time <- "%s:%s:%s" %>% 
            sprintf(
                substr(dat$time, 1, 2), 
                substr(dat$time, 3, 4), 
                substr(dat$time, 5, 6)
            )
        dat$time_stamp <- "%s %s" %>%
            sprintf(as.character(dat$date), dat$time) %>%
            as.POSIXct(tz = "Asia/Tokyo");
        dat %>% 
            select(pair, date, time_stamp, open, high, low, close) %>% 
            mutate(px = 0.25 * (open + high + low + close))
    }
    
    saveToFile <- function(dat_intraday) {
        file_name_1 <- paste0(file_path, "/Intraday_Latest.RData")
        file_name_2 <- gsub("Latest", format(report_date, "%Y_%m_%d"), file_name_1)
        save(dat_intraday, file = file_name_1)
        save(dat_intraday, file = file_name_2)
        dat_intraday
    }
    
    removeTmpFiles <- function(dat_intraday) {
        force(dat_intraday)
        unlink(zip_file)
        unlink(txt_file)
        dat_intraday
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    
    genUrl() %>%
        retrieveWebFile %>%
        unzipFile %>%
        readData %>%
        formatData %>%
        saveToFile %>%
        removeTmpFiles 
    
}
T.downloadLiveOHLC <-
function () 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    yahoo_live_url <- "https://query1.finance.yahoo.com/v7/finance/quote?formatted=false&symbols="
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    getLiveYahooPrice <- function (yahoo_ticker_list) {
        yahoo_ticker_list <- paste(yahoo_ticker_list, collapse = ",")
        dat_px <- yahoo_live_url %>%
            paste(yahoo_ticker_list, sep = "") %>% 
            fromJSON %>% 
            .$quoteResponse %>% 
            .$result %>% 
            select(
                symbol, 
                regularMarketTime, 
                regularMarketOpen, 
                regularMarketDayHigh, 
                regularMarketDayLow, 
                regularMarketPrice
            );
        names(dat_px) <- c("yahoo_ticker", "date_time", "open", "high", "low", "close");
        dat_px$date_time <- as.POSIXct(dat_px$date_time,  origin = '1970-01-01 00:00:00');
        dat_px;
    }
    
    getFXOpenPricesTodayFromYahoo <- function() {
        VENTURA$assets$pairs_fx %>% 
            paste0("=X") %>%
            getLiveYahooPrice %>%
            mutate(date = as.Date(date_time)) %>%
            filter(date == TO_DAY) %>%
            mutate(pair = substr(yahoo_ticker, 1, 6)) %>% 
            select(pair, date, open) %>%
            filter(pair %in% VENTURA$assets$pairs_fx) %>%
            rename(open_yahoo = open);
    }
    
    readInvestingPage <- function(investing_url) {
        investing_url %>%
            read_html %>%
            html_node("#cr1") %>%
            html_table %>% 
            .[,-1] %>%
            mutate(date = TO_DAY);
    }
    
    formatFXInvestingPanel <- function(dat) {
        dat %>%
            rename(pair = Pair, open = Open, high = High, low = Low, close = Last) %>% 
            select(pair, date, open, high, low, close) %>% 
            mutate(
                pair = gsub("/", "", pair, fixed = TRUE),
                open = as.numeric(gsub(",","", open)),
                high = as.numeric(gsub(",","", high)),
                low = as.numeric(gsub(",","", low)),
                close = as.numeric(gsub(",","", close))
            ) %>%
            U.data2Tibble;
    }
    
    getG7PanelInvestingWithOpenHighLowClose <- function() {
        "https://www.investing.com/currencies/" %>%
            readInvestingPage %>%
            formatFXInvestingPanel;
    }
    
    getCCYPanelInvestingWithoutOpenButWithHighLow <- function() {
        "https://www.investing.com/currencies/streaming-forex-rates-majors" %>%
            readInvestingPage %>% 
            mutate(
                Bid = as.numeric(gsub(",","", Bid)),
                Ask = as.numeric(gsub(",","", Ask)),
                Open = NUM_NA,
                Last = 0.5 * (Bid + Ask)
            ) %>%
            formatFXInvestingPanel %>%
            rename(high_inv = high, low_inv = low, close_inv = close);
        
    }
    
    getCCYPanelInvestingWithoutOpenButWithHighLowUSDCrosses <- function() {
        "https://www.investing.com/currencies/single-currency-crosses" %>%
            readInvestingPage %>% 
            mutate(
                Bid = as.numeric(gsub(",","", Bid)),
                Ask = as.numeric(gsub(",","", Ask)),
                Open = NUM_NA,
                Last = 0.5 * (Bid + Ask)
            ) %>%
            formatFXInvestingPanel %>%
            rename(high_inv2 = high, low_inv2 = low, close_inv2 = close);
    }
    
    
    addColumnsFromAnotherTable <- function(dat, dat_new, col_name, col_name_new) {
        dat_new <- dat_new[c("pair", "date", col_name_new)];
        colnames(dat_new)[3] <- "tmp";
        dat_new <- dat_new %>% 
            group_by(pair, date) %>%
            summarize(tmp = first(tmp)) %>%
            ungroup;
        dat <- dat %>% 
            left_join(dat_new, by = c("pair", "date"));
        pos_add <- which(is.na(dat[col_name] & !is.na(dat$tmp)));
        dat[col_name][pos_add,] <- dat$tmp[pos_add];
        dat %>%
            select(-tmp);
    }
    
    getReutersData_Try <- function(this_pair) {
        dat_reuters <- "https://www.reuters.com/quote/%s" %>%
            sprintf(this_pair) %>%
            read_html;
        
        dat_open <- dat_reuters %>%
            html_node(".Profile-table-21j-B") %>%
            html_text;
        
        dat_px <- dat_reuters %>% 
            html_node(".QuotePage-section-16t8C") %>%
            html_text;
        
        pos_open <- gregexpr("Open", dat_open, fixed = TRUE) %>% U.vectorize %>% head(1);
        pos_bid <- gregexpr("Bid", dat_open, fixed = TRUE) %>% U.vectorize %>% head(1);
        px_open <- substr(dat_open, pos_open + nchar("Open"), pos_bid - 1) %>% 
            gsub(",","",.,fixed = TRUE) %>%
            as.numeric;
        
        pos_last <- gregexpr("Latest Trade", dat_px, fixed = TRUE) %>% U.vectorize %>% head(1);
        pos_change <- gregexpr("Change", dat_px, fixed = TRUE) %>% U.vectorize %>% head(1);
        pos_range <- gregexpr("Today's Range", dat_px, fixed = TRUE) %>% U.vectorize %>% head(1);
        pos_separator <- gregexpr(" - ", dat_px, fixed = TRUE) %>% U.vectorize %>% head(1);
        pos_week <- gregexpr("52 Week Range", dat_px, fixed = TRUE) %>% U.vectorize %>% head(1);
        
        px_last <- substr(dat_px, pos_last + nchar("Latest Trade"), pos_change - 1) %>% 
            gsub(",","",.,fixed = TRUE) %>%
            as.numeric;
        px_hi_lo_1 <- substr(dat_px, pos_range + nchar("Today's Range"), pos_separator - 1) %>% 
            gsub(",","",.,fixed = TRUE) %>%
            as.numeric;
        px_hi_lo_2 <- substr(dat_px, pos_separator + nchar(" - "), pos_week - 1) %>% 
            gsub(",","",.,fixed = TRUE) %>%
            as.numeric;
        
        px_high <- max(c(px_hi_lo_1, px_hi_lo_2));
        px_low <- min(c(px_hi_lo_1, px_hi_lo_2));
        
        data.frame(
            pair = this_pair,
            date = TO_DAY,
            open_reuters = px_open,
            high_reuters = px_high,
            low_reuters = px_low,
            close_reuters = px_last
        ) %>% U.data2Tibble;
        
    }
    
    getReutersData <- function(this_pair) 
        U.try(getReutersData_Try, NULL)(this_pair);
    
    getFXLive <- function() {
        dat_fx <- data.frame(pair = VENTURA$assets$pairs_fx, date = TO_DAY) %>% 
            U.data2Tibble;
        dat_open_yahoo <- getFXOpenPricesTodayFromYahoo();
        dat_g7_panel <- getG7PanelInvestingWithOpenHighLowClose();
        dat_all_ccy_panel <- getCCYPanelInvestingWithoutOpenButWithHighLow();
        dat_usd_ccy_panel <- getCCYPanelInvestingWithoutOpenButWithHighLowUSDCrosses();
        
        dat_fx <- dat_fx %>% 
            left_join(dat_g7_panel, by = c("pair", "date")) %>%
            addColumnsFromAnotherTable(dat_open_yahoo, "open", "open_yahoo") %>% 
            addColumnsFromAnotherTable(dat_all_ccy_panel, "high", "high_inv") %>% 
            addColumnsFromAnotherTable(dat_all_ccy_panel, "low", "low_inv") %>%
            addColumnsFromAnotherTable(dat_all_ccy_panel, "close", "close_inv") %>% 
            addColumnsFromAnotherTable(dat_usd_ccy_panel, "high", "high_inv2") %>% 
            addColumnsFromAnotherTable(dat_usd_ccy_panel, "low", "low_inv2") %>%
            addColumnsFromAnotherTable(dat_usd_ccy_panel, "close", "close_inv2")
        
        missing_pairs <- dat_fx %>% anti_join(na.omit(dat_fx), by = c("pair", "date")) %>% 
            .$pair
        
        dat_reuters <- missing_pairs %>%
            lapply(getReutersData) %>%
            bind_rows
        
        dat_fx %>% 
            addColumnsFromAnotherTable(dat_reuters, "open", "open_reuters") %>% 
            addColumnsFromAnotherTable(dat_reuters, "high", "high_reuters") %>% 
            addColumnsFromAnotherTable(dat_reuters, "low", "low_reuters") %>%
            addColumnsFromAnotherTable(dat_reuters, "close", "close_reuters") %>%
            na.omit
    }
    
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    U.printBanner("Retrieving Web Prices for FX")
    U.try(getFXLive, NULL)()
}
T.estimateFutureSpread <-
function(instrument_list) {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    instrument_id_list <- A.getInstrumentId(instrument_list)
    TIME_DIFFERENCE_THRESHOLD_MINUTES <- 1
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    prepareIndexPrice <- function(instruments_to_do) {
        "SELECT DISTINCT instrument_id, timestamp_px AS timestamp_idx, close AS px_idx
            FROM archive_predict
            WHERE timestamp_px >= '%s'
            AND instrument_id IN (%s)" %>%
            sprintf(
                YESTERDAY,
                paste(instrument_id_list, collapse=",")
            ) %>%
            D.SQL %>%
            mutate(timestamp_idx = as.POSIXct(timestamp_idx))
    }
    
    prepareFuturePrice <- function(instruments_to_do) {
        "SELECT C.instrument_id, L.conid, E.expiry, L.timestamp AS timestamp_fut, L.price as px_fut
        FROM live_px_future L
        LEFT JOIN static_future_expiry E ON E.conid = L.conid
        LEFT JOIN static_future_contract C ON C.future_id = E.future_id
        WHERE L.timestamp >= '%s'
        AND C.instrument_id IN (%s)" %>%
            sprintf(
                YESTERDAY,
                paste(instrument_id_list, collapse=",")
            ) %>% 
            D.SQL %>%
            mutate(
                expiry = as.Date(expiry),
                timestamp_fut = as.POSIXct(timestamp_fut)
                )
    }
    
    estimateSpreads <- function(px_idx, px_fut) {
        px_idx %>% 
            full_join(px_fut, by = "instrument_id") %>%
            mutate(
                time_diff = abs(as.numeric(difftime(timestamp_idx, timestamp_fut, units = "mins"))),
                spread = px_fut - px_idx
            ) %>% 
            filter(
                !is.na(time_diff), 
                time_diff <= ACCEPTABLE_TIMEDIFF_MINUTES
            ) %>% 
            arrange(instrument_id, time_diff) %>% 
            group_by(instrument_id, conid, expiry) %>%
            summarize(spread = 0.5 * (mean(spread, trim = 0.25) + median(spread))) %>%
            ungroup
    }

    ####################################################################################################
    ### Script
    ####################################################################################################

    dat_idx <- prepareIndexPrice() 
    dat_fut <- prepareFuturePrice() 
    
    estimateSpreads(dat_idx, dat_fut)
    
}
T.estimateTodayOHLCIndexPriceWhenNotYetOpenFromLiveFuture <-
function(save_to_db = FALSE) {
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    ACCEPTABLE_TIMEDIFF_MINUTES <- 5
    
    NB_YEARS_CORRELATION <- 5
    
    MAIN_PAIRS_WITH_FUTURE <- c("DAXEUR", "SPXUSD", "NDXUSD", "DJIUSD", "RUTUSD")
    MAIN_INSTRUMENTS_WITH_FUTURE <- A.getInstrumentId(MAIN_PAIRS_WITH_FUTURE)
    
    MAIN_INDEX_EUROPE <- "DAXEUR"
    MAIN_INDEX_AMERICA <- "SPXUSD"
    
    FUTURES <- D.loadTableLocal("future_contract")
    FUTURES_EXP <- D.loadTableLocal("future_expiry")
    INVESTING_MAP <- D.loadTableLocal("investing_map")
    FUTURES_ACTIVE <- D.loadTableLocal("future_active")
    MARKETS <- D.loadTableLocal("market")
    TIME_ZONES <- D.loadTableLocal("time_zone")
    
    CLOSING_TIMES <- INSTRUMENTS %>% 
        left_join(MARKETS, by = "market") %>% 
        left_join(
            rename(TIME_ZONES, tz_close = time_zone, tz_close_id = time_zone_id), 
            by = "tz_close_id"
        ) %>% 
        left_join(
            rename(TIME_ZONES, tz_open = time_zone, tz_open_id = time_zone_id), 
            by = "tz_open_id"
        ) %>%
        select(instrument_id, time_open, time_close, tz_open, tz_close) %>%
        mutate(
            tz_open = case_when(is.na(tz_open) ~ tz_close, TRUE ~ tz_open)
        )
    
    calcTime <- function(col_data, col_tz, today_yesterday) {
        date_time <- paste0(today_yesterday, " ", col_data) %>%
            as.POSIXct(tz = col_tz)
        date_time <- date_time - 1
        date_time %>%
            format(tz = TZ_LOCAL) 
    }
    
    CLOSING_TIMES$time_open_today <- as.POSIXct(mapply(
        calcTime, 
        CLOSING_TIMES$time_open, 
        CLOSING_TIMES$tz_open,
        TO_DAY
    ))
    CLOSING_TIMES$time_close_today <- as.POSIXct(mapply(
        calcTime, 
        CLOSING_TIMES$time_close, 
        CLOSING_TIMES$tz_close,
        TO_DAY
    ))
    CLOSING_TIMES$time_close_yesterday <- as.POSIXct(mapply(
        calcTime, 
        CLOSING_TIMES$time_close, 
        CLOSING_TIMES$tz_close,
        YESTERDAY
    ))
    
    CLOSING_TIMES <- CLOSING_TIMES %>%
        select(instrument_id, time_close_yesterday, time_open_today, time_close_today) %>%
        mutate(
            market_status = case_when(
                ((time_open_today < start_time) & (time_close_today > start_time)) ~ "open",
                TRUE ~ "closed"
            )
        )
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    whichInstrumentsToDo <- function() {
        CLOSING_TIMES %>% 
            filter(start_time <= time_open_today) %>% 
            select(instrument_id) %>%
            U.vectorize %>%
            intersect(MAIN_INSTRUMENTS_WITH_FUTURE)
    }
    
    prepareIndexPrice <- function(instruments_to_do) {
        dat_px <- 
            "SELECT DISTINCT instrument_id, timestamp_px, close AS px_idx
        FROM archive_predict
        WHERE timestamp_px >= '%s'
        AND use_weights = 0
        AND instrument_id IN (%s)" %>%
            sprintf(
                YESTERDAY,
                paste(instruments_to_do, collapse=",")
            ) %>%
            D.SQL
        
        INSTRUMENTS %>%
            filter(instrument_id %in% instruments_to_do) %>%
            mutate(
                check_day_is_tomorrow_hk_time = (as.Date(execution_time) > TO_DAY) + 0,
                timestamp_idx = U.calcPreviousDay(
                    execution_time %m+% days(-check_day_is_tomorrow_hk_time)
                )
            ) %>% 
            select(instrument_id, timestamp_idx) %>%
            full_join(dat_px, by = "instrument_id") %>%
            mutate(
                time_diff = abs(as.numeric(difftime(timestamp_idx, timestamp_px, units = "mins"))),
            ) %>% 
            filter(
                !is.na(time_diff), 
                time_diff <= ACCEPTABLE_TIMEDIFF_MINUTES
            ) %>% 
            arrange(instrument_id, time_diff) %>% 
            group_by(instrument_id) %>%
            summarize(
                timestamp_idx = first(timestamp_px),
                px_idx = first(px_idx)
            ) %>%
            ungroup 
        
    }
    
    prepareFuturePrice <- function(instruments_to_do) {
        "SELECT C.instrument_id, L.timestamp AS timestamp_fut, L.price as px_fut
        FROM live_px_future L
        LEFT JOIN static_future_expiry E ON E.conid = L.conid
        LEFT JOIN static_future_contract C ON C.future_id = E.future_id
        WHERE L.timestamp >= '%s'
        AND C.instrument_id IN (%s)" %>%
            sprintf(
                YESTERDAY,
                paste(instruments_to_do, collapse=",")
            ) %>% 
            D.SQL 
    }
    
    estimateSpreads <- function(px_idx, px_fut) {
        px_idx %>% 
            full_join(px_fut, by = "instrument_id") %>%
            mutate(
                time_diff = abs(as.numeric(difftime(timestamp_idx, timestamp_fut, units = "mins"))),
                spread = px_fut - px_idx
            ) %>% 
            filter(
                !is.na(time_diff), 
                time_diff <= ACCEPTABLE_TIMEDIFF_MINUTES
            ) %>% 
            arrange(instrument_id, time_diff) %>% 
            group_by(instrument_id) %>%
            summarize(spread = first(spread)) %>%
            ungroup
    }
    
    estimateLiveIndexPrice <- function(px_idx, px_fut, px_spread) {
        px_fut_last <- px_fut %>%
            group_by(instrument_id) %>%
            arrange(timestamp_fut) %>%
            summarize(
                timestamp_fut = last(timestamp_fut), 
                px_fut_last = last(px_fut)
            ) %>%
            ungroup
        
        px_idx %>% 
            #  mutate(spread = 0) %>%
            left_join(px_fut_last, by = "instrument_id") %>%
            left_join(px_spread, by = "instrument_id") %>%
            mutate(px_idx_live = px_fut_last - spread) %>%
            filter(!is.na(px_idx_live))
    }
    
    estimateLiveOHLC <- function(px_live, instruments_to_do) {
        dat_vol <- "SELECT instrument_id, value AS vol_st 
            FROM live_technicals_dbl T
            LEFT JOIN static_feature F ON F.feature_id = T.feature_id
            WHERE F.feature = 'vol_st'
            AND T.instrument_id IN (%s)" %>%
            sprintf(paste(instruments_to_do, collapse=",")) %>%
            D.SQL
        
        px_live <- px_live %>% 
            left_join(dat_vol, by = "instrument_id") %>%
            left_join(
                select(CLOSING_TIMES, instrument_id, time_close_today), 
                by = "instrument_id"
            )
        
        dat_expected <- 1:nrow(px_live) %>%
            lapply(
                function(i) {
                    data.frame(
                        instrument_id = px_live$instrument_id[i],
                        T.expectedMinMaxBeforeClose(
                            px_live$px_idx_live[i],
                            px_live$vol_st[i],
                            px_live$timestamp_fut[i],
                            px_live$time_close_today[i]
                        )
                    )
                }
            ) %>%
            bind_rows
        px_live %>%
            mutate(
                open = px_idx_live,
                close = px_idx_live,
                date = TO_DAY
            ) %>%
            left_join(dat_expected, by = "instrument_id") %>%
            rename(
                high = expected_max, 
                low = expected_min,
                timestamp = timestamp_fut
            ) %>%
            select(instrument_id, date, timestamp, open, high, low, close) %>%
            na.omit
    }
    
    estimateOHLCForMainIndicesWithFuture <- function() {
        instruments_to_do <- whichInstrumentsToDo()
    #    instruments_to_do <- 27
        px_idx <- prepareIndexPrice(instruments_to_do) 
        U.printBanner("prices idx")
        print(data.frame(px_idx))
        px_fut <- prepareFuturePrice(instruments_to_do) %>% U.debug("px fut")
        U.printBanner("prices fut")
        print(data.frame(px_fut))
        
        px_spread <- estimateSpreads(px_idx, px_fut) %>% U.debug("px spread")
        dat_live <- estimateLiveIndexPrice(px_idx, px_fut, px_spread) %>% U.debug("px_live")
        dat_open <- dat_live %>% 
            mutate(date = TO_DAY) %>%
            rename(timestamp = timestamp_fut, open = px_idx_live) %>%
            select(instrument_id, date, timestamp, open) %>%
            U.debug("live")
        #        estimateLiveOHLC(dat_live, instruments_to_do) %>% U.debug("expected ohlc")
    }
    
    gethistoOHLCFromDB <- function(this_pair) {
        "SELECT date, open, close
            FROM histo_px_daily
            WHERE instrument_id = %s
            AND date >= '%s'
            ORDER BY date" %>%
            sprintf(
                A.getInstrumentId(this_pair),
                TO_DAY %m+% years(-NB_YEARS_CORRELATION)
            ) %>%
            D.select
    }
    
    estimateOpenPriceForPairFromMainMarket <- function(this_pair, px_benchmark_histo) {
        dat <- gethistoOHLCFromDB(this_pair) %>%
            mutate(x_y = "y") %>%
            select(x_y, date, open, close) 
        
        px_this_pair <- last(dat$close)
        
        rtn_today_bench <- px_benchmark_histo %>%
            tail(2) %>%
            mutate(rtn = log(open / lag(close, 1))) %>%
            .$rtn %>% 
            last 
        
        lm_regression <- rbind(dat, px_benchmark_histo) %>% 
            group_by(x_y) %>%
            mutate(rtn = log(open / lag(close, 1))) %>%
            ungroup %>% 
            select(x_y, date, rtn) %>% 
            spread(x_y, rtn, fill = NA) %>%
            na.omit %>% 
            lm(y ~ x + 0, .)
        
        expected_open_rtn_from_close <- lm_regression %>%
            predict(newdata = data.frame(x = rtn_today_bench)) %>%
            U.vectorize
        
        data.frame (
            pair = this_pair,
            open = px_this_pair * exp(expected_open_rtn_from_close),
            #     intercept = summary(lm_regression)$coefficients[1],
            coef_1 = summary(lm_regression)$coefficients[1],
            r2 = summary(lm_regression)$r.squared
            
        )
    }
    
    estimateOpenForDerivedMarkets <- function(dat_main, this_region) {
        benchmark_idx <- switch(
            this_region, 
            "Europe" = MAIN_INDEX_EUROPE, 
            "America" = MAIN_INDEX_AMERICA
        )
        
        time_open_benchmark <- CLOSING_TIMES %>%
            filter(instrument_id == filter(INSTRUMENTS, pair == benchmark_idx)$instrument_id) %>%
            .$time_open_today
        
        dat_derived <- NULL
        if (start_time < time_open_benchmark) {
            benchmark_timestamp <- dat_main %>%
                filter(instrument_id == A.getInstrumentId(benchmark_idx)) %>%
                .$timestamp 
            if (length(benchmark_timestamp) == 0) {
                benchmark_timestamp <- Sys.time()
            }
            benchmark_timestamp %>% U.debug("timestamp")
            
            pair_list <- INSTRUMENTS %>%
                filter(asset_class == "index", grepl(this_region, market)) %>%
                .$pair %>%
                setdiff(MAIN_PAIRS_WITH_FUTURE)
            
            dat_benchmark_histo <- gethistoOHLCFromDB(benchmark_idx) 
            dat_benchmark_today <- dat_main %>%
                filter(instrument_id == A.getInstrumentId(benchmark_idx)) %>%
                mutate(close = open) %>%
                select(date, open, close)
            dat_benchmark <- rbind(dat_benchmark_histo, dat_benchmark_today) %>%
                mutate(x_y = "x") %>%
                select(x_y, date, open, close)
            
            dat_derived <- pair_list %>%
                lapply(function(this_pair) 
                    estimateOpenPriceForPairFromMainMarket(this_pair, dat_benchmark) 
                ) %>%
                bind_rows %>% 
                U.debug(paste0("Estimated open for ", this_region)) %>%
                mutate(
                    instrument_id = A.getInstrumentId(pair), 
                    date = TO_DAY,
                    timestamp = benchmark_timestamp
                ) %>%
                select(instrument_id, date, timestamp, open) %>% U.debug("With instruments")
        }
        dat_derived
    }
    
    saveToDB <- function(dat) {
        if (save_to_db) {
            U.printBanner("Will save this to DB now", FALSE)
            print(na.omit(dat) %>% data.frame)
            D.replaceDataIntoTable("live_px", na.omit(dat), FALSE)
            U.printBanner("Saving done", FALSE)
        }
        dat
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_main <- estimateOHLCForMainIndicesWithFuture() 
    
    dat_derived_europe <- estimateOpenForDerivedMarkets(dat_main, "Europe") 
    dat_derived_america <- estimateOpenForDerivedMarkets(dat_main, "America")     
    
    rbind(dat_main, dat_derived_europe, dat_derived_america) %>%
        mutate(high = open, low = open, close = open) %>% 
        saveToDB
    
}
T.estimateTradingZones <-
function (fx_pair, dat_ohlc) 
{
    #    dat_histo_ohlc %>% 
    #        filter(pair == "USDJPY") %>% 
    #        select(-pair) %>% 
    #        filter(date >= "2018-01-30") %>% 
    #        ggplot(aes(x = date, y = close)) + 
    #        geom_candlestick(aes(open = open, high = high, low = low, close = close)) + 
    #        geom_hline(yintercept = filter(a.1$levels, weight >= 150, price <= 111, price >= 105)$price, 
    #                   alpha = 0.25)
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    weights <- list(
        D = 1,
        W = sqrt(5),
        M = sqrt(21),
        Q = sqrt(65),
        Y = sqrt(260)
    )
    
    weights_zigzag <- list(
        big = 50,
        medium = 25,
        small = 5
    )
    
    width <- list(
        D = 0.01 / (5 * sqrt(260)),
        W = 0.01 / (5 * sqrt(52)),
        M = 0.01 / (5 * sqrt(12)),
        Q = 0.01 / (5 * sqrt(4)),
        Y = 0.01 / 5
    )
    
    width_zigzag <- list(
        big = 0.01 / 4,
        medium = 0.01 / (4 * sqrt(4)),
        small = 0.01 / (4 * sqrt(12))
    )
    
    keep_last_rows <- list(
        D = 1000,
        W = 400,
        M = 60,
        Q = 25,
        Y = 10
    );
    
    threshold_quantile <- 0.90
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    prepareHistoData <- function() {
        dat_ohlc %>% 
            filter(pair == fx_pair) %>% 
            select(-pair);
    }
    
    preparePeriodicData <- function(dat_histo, period_type) {
        dat_histo %>% 
            T.dfToXts %>%
            T.dailyToPeriod(period_type) %>%
            T.xtsToDf %>%
            mutate(
                weight = weights[[period_type]],
                width = close * width[[period_type]]
            ) %>%
            tail(keep_last_rows[[period_type]]) %>% 
            U.data2Tibble;
    }
    
    aggregateZones <- function(dat) {
        dat %>%
            select(-date) %>%
            gather(ohlc, price, -weight, -width) %>% 
            select(weight, width, price);
    }
    
    calcAvgVol <- function(dat) {
        dat %>% 
            T.dfToXts %>% 
            volatility(n = 260, calc = "yang.zhang") %>%
            na.omit %>%
            mean;
        
    }
    
    calcZigZagZones <- function(dat) {
        dat_xts <- T.dfToXts(dat);
        
        dat_xts_hl <- dat_xts[,c("High", "Low")];
        dat_xts$zigzag_big <- ZigZag(dat_xts_hl, change = 1.5*vol*100);
        dat_xts$zigzag_medium <- ZigZag(dat_xts_hl, change = 1*vol*100);
        dat_xts$zigzag_small <- ZigZag(dat_xts_hl, change = 0.5*vol*100);
        
        
        
        dat_z <- dat_xts %>%
            T.xtsToDf %>%
            mutate(
                top_big = (high == zigzag_big) + 0,
                bottom_big = (low == zigzag_big) + 0,
                top_medium = (high == zigzag_medium) + 0,
                bottom_medium = (low == zigzag_medium) + 0,
                top_small = (high == zigzag_small) + 0,
                bottom_small = (low == zigzag_small) + 0
            )
        
        dat_big <- dat_z %>%
            filter(top_big + bottom_big > 0) %>%
            mutate(
                weight = weights_zigzag[["big"]],
                width = width_zigzag[["big"]]
            );
        pos_top <- which(dat_big$top_big == 1);
        pos_bottom <- which(dat_big$bottom_big == 1);
        dat_big$close[pos_top] <- dat_big$high[pos_top];
        dat_big$close[pos_bottom] <- dat_big$low[pos_bottom];
        
        
        dat_medium <- dat_z %>%
            filter(top_medium + bottom_medium > 0) %>%
            mutate(
                weight = weights_zigzag[["medium"]],
                width = width_zigzag[["medium"]]
            );
        pos_top <- which(dat_medium$top_medium == 1);
        pos_bottom <- which(dat_medium$bottom_medium == 1);
        dat_medium$close[pos_top] <- dat_medium$high[pos_top];
        dat_medium$close[pos_bottom] <- dat_medium$low[pos_bottom];
        
        
        dat_small <- dat_z %>%
            filter(top_small + bottom_small > 0) %>%
            mutate(
                weight = weights_zigzag[["small"]],
                width = width_zigzag[["small"]]
            );
        pos_top <- which(dat_small$top_small == 1);
        pos_bottom <- which(dat_small$bottom_small == 1);
        dat_small$close[pos_top] <- dat_small$high[pos_top];
        dat_small$close[pos_bottom] <- dat_small$low[pos_bottom];
        
        rbind(dat_big, dat_medium, dat_small) %>%
            select(date, close, weight, width);
        
    }
    
    calcTotalWeight <- function(this_px, dat) {
        filter(dat,
               this_px >= px_dn,
               this_px <= px_up
        )$weight %>%
            sum;
    }
    
    calcZones <- function(dat) {
        zones <- data.frame(price = seq(min(dat$price), max(dat$price), length.out = 2000)) %>%
            U.data2Tibble;
        
        dat <- dat %>% 
            mutate(
                px_up = price + width / 2,
                px_dn = price - width / 2
            );
        
        calcTotalWeight_Local <- function(this_px) 
            calcTotalWeight(this_px, dat)
        
        zones$weight <- U.sapply(zones$price, calcTotalWeight_Local);
        zones <- zones %>% 
            arrange(weight)
        threshold <- quantile(zones$weight, threshold_quantile) %>% U.vectorize;
        zones %>% 
            filter(weight >= threshold)
        
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    
    dat <- prepareHistoData();
    vol <- calcAvgVol(dat);
    dat_d <- preparePeriodicData(dat, "D");
    dat_w <- preparePeriodicData(dat, "W");
    dat_m <- preparePeriodicData(dat, "M");
    dat_q <- preparePeriodicData(dat, "Q");
    dat_y <- preparePeriodicData(dat, "Y");
    dat_zigzag <- calcZigZagZones(dat);
    
    dat_zones <- aggregateZones(dat_d) %>% 
        rbind(aggregateZones(dat_w)) %>% 
        rbind(aggregateZones(dat_m)) %>% 
        rbind(aggregateZones(dat_q)) %>%
        rbind(aggregateZones(dat_y)) %>%
        rbind(aggregateZones(dat_zigzag));
    
    dat_levels <- calcZones(dat_zones);
    
    list(
        daily = dat_d,
        weekly = dat_w,
        monthly = dat_m,
        quarterly = dat_q,
        yearly = dat_y,
        zigzag = dat_zigzag,
        zones = dat_zones,
        levels = dat_levels
    )    
}
T.expectedMinMaxBeforeClose <-
function (px, vol, time_now, time_close) 
{
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    # Formula expected maximum of geometric brownian motion
    # https://www.ntu.edu.sg/home/nprivault/MA5182/maximum-brownian-motion.pdf
    # page 19
    
    t_exp <- pmax(0, as.numeric(difftime(time_close, time_now, units = "days")) / 365)
    s2t <- t_exp * (vol ** 2)
    sqrt_s2t <- sqrt(s2t)
    
    ####################################################################################################
    ### Sub Routines
    ####################################################################################################
    calcExpectedMax <- function(S) {
        S * (2 * (1 + 0.25 * s2t) * pnorm(0.5 * sqrt_s2t) + sqrt_s2t / sqrt(2*pi) * exp(-s2t / 8))
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    expected_max <- U.try(calcExpectedMax, NUM_NA)(px)
    expected_min <- 1 / U.try(calcExpectedMax, NUM_NA)(1/px)
    
    data.frame(px, vol, expected_min, expected_max)
}
T.fillMissingHighAndLowWithClose <-
function(dat) {
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    dat$high_na <- dat$high;
    dat$low_na <- dat$low;
    
    pos_na_hi <- which(is.na(dat$high));
    if (length(pos_na_hi) > 0) {
        dat$high_na[pos_na_hi] <- dat$close[pos_na_hi];    
    }
    
    pos_na_lo <- which(is.na(dat$low));
    if (length(pos_na_lo) > 0) {
        dat$low_na[pos_na_lo] <- dat$close[pos_na_lo];
    }
    dat;
}
T.fillMissingOpenWithPreviousClose <-
function(dat) {
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat$open_na <- dat$open;
    pos_na_open <- which(is.na(dat$open)) %>% 
        setdiff(1);
    if (length(pos_na_open) > 0) {
        dat$open_na[pos_na_open] <- dat$close[pos_na_open - 1];
    }
    dat
}
T.getDailyHistoDataYahoo <-
function (yahoo_ticker_list, date_from = NULL) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    date_from <- date_from %>%
        is.null %>%
        U.ifelse("2007-01-01", date_from) %>%
        as.Date;

    ####################################################################################################
    ### Script sub functions
    ####################################################################################################
    getYahooHisto_Try <- function(yahoo_ticker) {
        dat_histo <- yahoo_ticker %>%
                getSymbols.yahoo(auto.assign = FALSE, from = date_from) %>%
                .[,-5];
        colnames(dat_histo) <- c("Open", "High", "Low", "Close", "Adjusted");
        dat_histo %>%
            adjustOHLC(use.Adjusted = TRUE) %>%
            .[,-5] %>%
            na.omit;
    }
    getYahooHisto <- function(yahoo_ticker) U.tryNull(getYahooHisto_Try, yahoo_ticker)
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    res <- yahoo_ticker_list %>%
        lapply(getYahooHisto);
    names(res) <- yahoo_ticker_list;
    if (length(yahoo_ticker_list) == 1)
        res[[1]]
    else
        res
}
T.getHistoPrices <-
function (pair_list = NULL) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    
    if (is.null(pair_list)) {
        pair_list <- INSTRUMENTS$pair
    }
  #  pair_list <- setdiff(
  #      pair_list, 
  #      c("XPDUSD", "XPTUSD")
  # )
    
    file_name <- paste0(DIRECTORY_DATA_HD, "Spot/Daily/Histo_OHLC.RData")
    
    dat_histo_db <- "SELECT * FROM histo_px_daily WHERE date >= '%s'" %>%
        sprintf(TO_DAY %m+% months(-2)) %>%
        D.select %>%
        left_join(INSTRUMENTS, by = "instrument_id") %>%
        select(pair, date, open, high, low, close)
    
    
    yahoo_pairs <- INSTRUMENTS %>% filter(asset_class =="bond", is_etf==1) %>% .$pair
    wsj_pairs <- ""
    if (INVESTING_COM_IS_BLOCKED) {
        wsj_pairs <- c("BTCUSD", "JPYKRW", "USDVND", "USDTHB", "USDPHP", "USDINR", "USDIDR", "USDTWD",
                       "USDBRL", "USDCLP", "USDKRW", "USDMYR", "USDTRY")        
    }

    fx_pairs_investing_because_ib_bad <- INSTRUMENTS %>% 
        filter(
            asset_class == "fx_dm",
            grepl("SEK", pair) | grepl("NOK", pair) | grepl("GBP", pair)
        ) %>% 
        .$pair
        
    
    pairs_done <<- NULL
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    getRecentPairHistoOHLC <- function(this_pair) {
        dat <- NULL
        U.printTickerProgressVerbose(this_pair, pair_list)
      #  if (this_pair == "STISGD") {
      #      dat <- T.getSTIPriceFromWSJ()
      #  }
        if (this_pair %in% yahoo_pairs) {
            dat <- T.getPricesYahoo(this_pair, "close")
        }
        else if (this_pair %in% wsj_pairs) {
            dat <- T.getPricesWSJ(this_pair)
        }
        else if (!(this_pair %in% fx_pairs_investing_because_ib_bad)) {
            dat <- T.getPricesIB(this_pair, "close")
        }
        if (!U.dfContainsData(dat)) {
            dat <- T.getPricesInvesting(this_pair, "close")
        }
        if (U.dfContainsData(dat)) {
            pairs_done <<- c(pairs_done, this_pair)
        }
        
        dat %>%
            U.noData2Null
    }
    
    addToExistingHisto <- function(dat_histo_new, dat_histo_old) {
        dat_histo_old %>% 
            anti_join(dat_histo_new, by = c("pair", "date")) %>%
            rbind(dat_histo_new) %>%
            arrange(pair, date) %>%
            U.data2Tibble
    }
    
    finalFormatting <- function(dat) {
        dat <- dat %>%
            filter(date <= YESTERDAY) %>%
            mutate(weekday = weekdays(date)) %>% 
            filter(!(weekday %in% c("Saturday", "Sunday"))) %>% 
            select(-weekday) %>% 
            U.data2Tibble
        dat[which(!duplicated(select(dat, pair, date))),]
    }
    
    saveToFile <- function(dat) {
        load(file_name)
        dat_histo_ohlc <- dat_histo_ohlc %>% 
            anti_join(dat, by = c("pair", "date")) %>%
            rbind(dat) %>%
            arrange(pair, date)
        save(dat_histo_ohlc, file = file_name)
        dat
    }
    
    saveStatusToDB <- function(this_pair) {
        "UPDATE status_instrument
        SET histo_px_last_update = '%s', histo_px_updated = 1
        WHERE instrument_id = %s" %>%
            sprintf(
                format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                A.getInstrumentId(this_pair)
            ) %>% 
            D.SQL 
    }
    
    saveToDB <- function(dat) {
        U.printBanner("Now saving to DB", FALSE)
        dat %>% 
            left_join(select(INSTRUMENTS, pair, instrument_id), by = "pair") %>%
            select(instrument_id, date, open, high, low, close) %>%
            filter(!is.na(instrument_id)) %>%
            D.replaceDataIntoTable("histo_px_daily", .)
        lapply(pairs_done, saveStatusToDB)
        dat
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    U.printBanner("Doing historics - start")
    pair_list %>%
        lapply(getRecentPairHistoOHLC) %>%
        bind_rows %>% 
        addToExistingHisto(dat_histo_db) %>% 
        finalFormatting %>% 
        saveToFile %>% 
        saveToDB %>% U.printMilestone("Histo All Done")
}
T.getHistoPx <-
function (instrument = NULL, date_from = NULL, date_to = NULL)
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    instrument_id <- A.getInstrumentId(instrument);
    if (is.null(date_from)) date_from <- "1800-01-01"
    if (is.null(date_to)) date_to <- TO_DAY + 100
    date_from <- as.Date(date_from)
    date_to <- as.Date(date_to)
    
    ####################################################################################################
    ### Sub routines
    ####### ############################################################################################
    getHistoFromDB_Try <- function() {
        sql_q <- "SELECT * FROM histo_px_daily WHERE date BETWEEN '%s' AND '%s' " %>%
            sprintf(date_from, date_to) 
        if (is.null(instrument)) {}
        else if (instrument == "train") {
            sql_q <- sql_q %>% 
                paste0(
                    "AND instrument_id IN (
                    SELECT instrument_id FROM static_instrument WHERE use_for_training = 1
                    )"
                )
        }
        else {
            sql_q <- sql_q %>% 
                paste0(" AND instrument_id = ", instrument_id)
        }
        sql_q %>%
            D.select(TRUE, FALSE) %>%
            arrange(instrument_id, date)
    }
    getHistoFromDB <- function(instrument_id)
        U.try(getHistoFromDB_Try, NULL)()
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    getHistoFromDB();
    
}
T.getLivePriceFuture <-
function () 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    url_investing <- "https://www.investing.com/indices/%s"
    url_index <- sprintf(url_investing, "major-indices")
    url_future <- sprintf(url_investing, "indices-futures")
    
    FUTURES <- D.loadTableLocal("future_contract")
    FUTURES_EXP <- D.loadTableLocal("future_expiry")
    INVESTING_MAP <- D.loadTableLocal("investing_map")
    FUTURES_ACTIVE <- D.loadTableLocal("future_active")
    MARKETS <- D.loadTableLocal("market")
    TIME_ZONES <- D.loadTableLocal("time_zone")
    
    DATE_IN_NY_NOW <- as.Date(format(Sys.time(), tz = TZ_AMERICA))
    
    CLOSING_TIMES <- INSTRUMENTS %>% 
        left_join(MARKETS, by = "market") %>% 
        left_join(
            rename(TIME_ZONES, tz_close = time_zone, tz_close_id = time_zone_id), 
            by = "tz_close_id"
        ) %>% 
        left_join(
            rename(TIME_ZONES, tz_open = time_zone, tz_open_id = time_zone_id), 
            by = "tz_open_id"
        ) %>%
        select(instrument_id, time_open, time_close, tz_open, tz_close) %>%
        mutate(
            tz_open = case_when(is.na(tz_open) ~ tz_close, TRUE ~ tz_open)
        )
    
    calcTime <- function(col_data, col_tz, today_yesterday) {
        date_time <- paste0(today_yesterday, " ", col_data) %>%
            as.POSIXct(tz = col_tz)
        date_time <- date_time - 1
        date_time %>%
            format(tz = TZ_LOCAL) 
    }
    
    CLOSING_TIMES$time_open_today <- as.POSIXct(mapply(
        calcTime, 
        CLOSING_TIMES$time_open, 
        CLOSING_TIMES$tz_open,
        TO_DAY
    ))
    CLOSING_TIMES$time_close_today <- as.POSIXct(mapply(
        calcTime, 
        CLOSING_TIMES$time_close, 
        CLOSING_TIMES$tz_close,
        TO_DAY
    ))
    CLOSING_TIMES$time_close_yesterday <- as.POSIXct(mapply(
        calcTime, 
        CLOSING_TIMES$time_close, 
        CLOSING_TIMES$tz_close,
        YESTERDAY
    ))
    
    CLOSING_TIMES <- CLOSING_TIMES %>%
        select(instrument_id, time_close_yesterday, time_open_today, time_close_today) %>%
        mutate(
            market_status = case_when(
                ((time_open_today < Sys.time()) & (time_close_today > Sys.time())) ~ "open",
                TRUE ~ "closed"
            )
        )
    
    ####################################################################################################
    ### Script sub functions
    ####################################################################################################
    
    findCorrespondingConid <- function(this_future_id, this_expiry_month) {
        FUTURES_EXP %>% 
            filter(future_id == this_future_id) %>%
            mutate(expiry_month = month(expiry)) %>%
            filter(expiry_month == this_expiry_month) %>%
            arrange(expiry) %>%
            .$conid %>%
            first
    }
    
    getMainLivePriceTable <- function() {
        dat <- url_future %>%
            read_html %>%
            html_nodes(".datatable_table--narrow__25R8G") %>%
            .[[1]] %>%
            html_table
        dat0 <- dat
        cols_keep <- c("Index", "Month", "Last", "High", "Low", "Time")
        dat <- dat[, which(colnames(dat) %in% cols_keep)]
        colnames(dat) <- tolower(colnames(dat))
        
        for (j in 1:ncol(dat)) {
            dat[,j] <- U.vectorize(dat[,j])
        }
        
        #dat$date <- as.Date(dat$date, format = "%b %d, %Y")
        for (this_col in c("last", "high", "low")) {
            dat[[this_col]] <- as.numeric(gsub(",", "", dat[[this_col]]))
        }
        
        dat %>% 
            rename(
                future_label = index,
                price = last
            ) %>%
            mutate(
                future_label = gsub("derived", "", future_label),
                month = gsub("Ex.", "", month)
            ) %>%
            left_join(select(INVESTING_MAP, instrument_id, future_label), by = "future_label") %>%
            left_join(select(FUTURES, future_id, instrument_id), by = "instrument_id") %>%
            filter(!is.na(future_id)) %>% 
            mutate(
                time = case_when(
                    grepl("/", time) ~ "00:00:00",
                    TRUE ~ time
                ),
                expiry_month = month(as.Date(month, format("%b %d"))),
                timestamp = as.POSIXct(
                    paste0(DATE_IN_NY_NOW, " ", time), 
                    tz = TZ_AMERICA
                    ) - 1,
                timestamp = format(timestamp, tz = TZ_LOCAL)
            ) %>% 
            select(future_id, timestamp, expiry_month, price)
    }
    
    getFuturePriceFromItsPage <- function(this_future_id) {
        dat_web <- "https://www.investing.com/%s" %>%
            sprintf(filter(FUTURES, future_id == this_future_id)$investing_url) %>%
            read_html %>% 
            html_nodes(".text-2xl")
        price <- dat_web %>%
            .[[3]] %>%
            html_text %>%
            gsub(",", "", .) %>% 
            as.numeric
        
        contract_label <- dat_web %>% 
            .[[2]] %>%
            html_text
        
        expiry_month <- INT_NA
        if (grepl("Jan", contract_label)) expiry_month <- 1
        if (grepl("Feb", contract_label)) expiry_month <- 2
        if (grepl("Mar", contract_label)) expiry_month <- 3
        if (grepl("Apr", contract_label)) expiry_month <- 4
        if (grepl("May", contract_label)) expiry_month <- 5
        if (grepl("Jun", contract_label)) expiry_month <- 6
        if (grepl("Jul", contract_label)) expiry_month <- 7
        if (grepl("Aug", contract_label)) expiry_month <- 8
        if (grepl("Sep", contract_label)) expiry_month <- 9
        if (grepl("Oct", contract_label)) expiry_month <- 10
        if (grepl("Nov", contract_label)) expiry_month <- 11
        if (grepl("Dec", contract_label)) expiry_month <- 12
        
        if (is.na(expiry_month)) {
            this_conid <- filter(FUTURES_ACTIVE, future_id == this_future_id)$conid
            expiry_month <- month(filter(FUTURES_EXP, conid == this_conid)$expiry)
        }
        
        data.frame(
            future_id = this_future_id,
            timestamp = format(Sys.time(), tz = TZ_LOCAL),
            expiry_month,
            price
        )
    }
    
    getRemainingPrices <- function(dat_main) {
        FUTURES_NOT_ON_MAP <- FUTURES %>% 
            left_join(
                select(INSTRUMENTS, instrument_id, asset_class), 
                by = "instrument_id"
            ) %>%
            left_join(CLOSING_TIMES, by = "instrument_id") %>%
            filter(
                tradable == 1, 
                investing_url != "", 
                asset_class == "index",
                market_status == "open"
            ) %>%
            anti_join(dat_main, by = "future_id")
        
        FUTURES_NOT_ON_MAP$future_id %>% 
            lapply(getFuturePriceFromItsPage) %>%
            bind_rows
    }
    
    combineWithTablesAndPrepareTableForExport <- function(dat) {
        dat$conid <- mapply(findCorrespondingConid, dat$future_id, dat$expiry_month)
        
        dat %>% 
            left_join(FUTURES_EXP, by = c("future_id", "conid")) %>%
            filter(!is.na(conid)) %>%
            select(conid, timestamp, price)
    }
    
    exportToDB <- function(dat) {
        U.printBanner("Saving this to DB", FALSE)
        print(dat %>% as_tibble)
        D.replaceDataIntoTable("live_px_future", dat, FALSE)
        U.printBanner("Saving to DB - done", FALSE)
        dat
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    
    dat_main <- getMainLivePriceTable()
    dat_per_page <- getRemainingPrices(dat_main)
    res <- rbind(dat_main, dat_per_page) %>%
        combineWithTablesAndPrepareTableForExport %>%
        exportToDB
    
    res_display <- res %>% 
        left_join(FUTURES_EXP, by = "conid") %>%
        left_join(FUTURES, by = "future_id") %>%
        select(future_id, ib_symbol, timestamp, conid, price) %>%
        arrange(ib_symbol)
    print(data.frame(res_display))
    
    res
}
T.getLivePrices <-
function (pair_list = NULL) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    if (is.null(pair_list)) {
        pair_list <- filter(INSTRUMENTS, use_for_trading == 1)$pair
    }
    
    yahoo_pairs <- c("STISGD")
    pairs_done <<- NULL
    
    dat_fx_live <- data.frame(pair="EURUSD", date=TO_DAY, open=0, high=0, low=0, close=0) %>%
        filter(close != 0) %>%
        U.data2Tibble
    
    ### Activate this if investing.com is blocked for fx:
    if (INVESTING_COM_IS_BLOCKED) {
        pair_list <- setdiff(
            pair_list, 
            c("BTCUSD", "JPYKRW", "USDVND", "USDTHB", "USDPHP", "USDINR", "USDIDR", "USDTWD",
              "USDBRL", "USDCLP", "USDKRW", "USDMYR", "USDTRY", "XPDUSD", "XPTUSD")
        )
    }
    else {
        if (is.na(A.getExecutionTimeId())) {
            dat_fx_live <- T.downloadLiveOHLC()
        }
    }
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    getRecentPairHistoOHLC <- function(this_pair) {
        dat <- NULL
        U.printTickerProgressVerbose(this_pair, pair_list)
        if (this_pair %in% yahoo_pairs) {
            dat <- T.getPricesYahoo(this_pair, "live")
        }
        
        if (!U.dfContainsData(dat)) {
            dat <- T.getPricesIB(this_pair, "live")
        }
        if (!U.dfContainsData(dat) & U.dfContainsData(dat_fx_live)) {
            dat <- filter(dat_fx_live, pair == this_pair)
            if (U.dfContainsData(dat)) {
                U.printBanner("From Web Live", FALSE)
                print(dat)
            }
        }
        if (!U.dfContainsData(dat)) {
            dat <- T.getPricesInvesting(this_pair, "live")
        }
        if (U.dfContainsData(dat)) {
            pairs_done <<- c(pairs_done, this_pair)
        }
        
        dat %>%
            U.noData2Null
    }
    
    saveStatusToDB <- function(this_pair) {
        "UPDATE status_instrument
        SET live_px_last_update = '%s' 
        WHERE instrument_id = %s" %>%
            sprintf(
                format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
                A.getInstrumentId(this_pair)
            ) %>%
            D.SQL
    }
    
    saveToDB <- function(dat_live) {
        dat_live_db <- dat_live %>%
            arrange(pair, date) %>%
            group_by(pair) %>%
            summarize(
                date = last(date),
                open = last(open),
                high = last(high),
                low = last(low),
                close = last(close)
            ) %>%
            ungroup %>%
            mutate(timestamp = Sys.time()) %>%
            left_join(select(INSTRUMENTS, pair, instrument_id), by = "pair") %>%
            select(instrument_id, date, timestamp, open, high, low, close) %>%
            filter(!is.na(instrument_id))
    #    D.truncateTable("live_px")
        D.replaceDataIntoTable("live_px", dat_live_db, FALSE)
        U.printBanner("Prices saved to DB: done", FALSE)
        lapply(pairs_done, saveStatusToDB)
        dat_live
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################

    pair_list %>%
        lapply(getRecentPairHistoOHLC) %>%
        bind_rows %>% 
        saveToDB
}
T.getPricesIB <-
function (pair_list, live_or_close = "close") 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    ib_histo_file <- paste0(DIRECTORY_DATA_HD, "Spot/IB/%s_histo.csv")
    max_lag_minutes <- 60*6*100 
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    testFileRecentEnough <- function(file_name) {
        test <- FALSE
        file_time <- file.mtime(file_name) 
        if (!is.na(test)) {
            test <- (file_time >= (Sys.time() - 60*max_lag_minutes))
        }
        test
    }

    getIBFile_Try <- function(this_pair) {
        file_name <- sprintf(ib_histo_file, this_pair)
        dat <- NULL
        if (testFileRecentEnough(file_name)) {
            dat <- file_name %>%
                U.read.csv %>%
                U.data2Tibble %>%
                T.keepOnlyRelevantHistoOHLCDates(live_or_close) %>%
                T.correctHistoBadHighLows(live_or_close == "live")
            if (U.dfContainsData(dat)) {
                U.printBanner("Getting price history from IB recently saved files", FALSE)
                print(tail(dat, 3) %>% data.frame)
            }
        }
        dat %>%
            U.noData2Null
    }
    getIBFile <- function(this_pair) 
        U.tryNull(getIBFile_Try, this_pair)
    
    ####################################################################################################
    ### Script 
    ####################################################################################################

    pair_list %>%
        setdiff(c("XAUUSD", "XAGUSD")) %>% # spot basis, edited Jan26
        lapply(getIBFile) %>%
        bind_rows
}
T.getPricesInvesting <-
function (pair_list, live_or_close = "close") 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    url_investing <- "https://www.investing.com/"
    
    url_fx <- paste0(url_investing, "currencies/%s-%s-historical-data")
    url_idx <- paste0(url_investing, "indices/%s-historical-data")
    url_yield <- paste0(url_investing, "rates-bonds/%s-year-bond-yield-historical-data")
    url_crypto <- paste0(url_investing, "crypto/%s/historical-data")
    url_etf <- paste0(url_investing, "etfs/%s-historical-data")
    url_btc <- sprintf(url_crypto, "bitcoin")
    
    idx_urls <- D.loadTableLocal("URL_INVESTING")
    
    pairs_etf <- filter(INSTRUMENTS, is_etf==1)$pair
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    formatDate <- function(dat) {
        date_format <- "%m/%d/%Y"
        if (grepl(",", dat$date[1])) date_format <- "%B %d, %Y"
        dat %>% 
            mutate(
                date = as.Date(date, format = date_format)
            )
    }
    
    genInvestingURL <- function(this_pair) {
        ccy_1 <- tolower(substr(this_pair, 1, 3));
        ccy_2 <- tolower(substr(this_pair, 4, 6));
        
        asset_class <- filter(INSTRUMENTS, pair == this_pair)$asset_class
        
        url_investing <- CHAR_NA;
        if (this_pair == "BTCUSD") {
            url_investing <- url_btc
        }
        else if (this_pair %in% pairs_etf) {
            url_investing <- url_etf %>%
                sprintf(filter(idx_urls, pair == this_pair)$value)
        }
        else if (asset_class %in% c("fx_dm", "fx_em", "metal")) {
            url_investing <- sprintf(url_fx, ccy_1, ccy_2)
        }
        else if (asset_class == "index") {
            url_investing <- url_idx %>%
                sprintf(filter(idx_urls, pair == this_pair)$value)
        }
        else if (asset_class == "yield") {
            url_investing <- url_yield %>% 
                sprintf(filter(idx_urls, pair == this_pair)$value)
            if (this_pair == "A30AUD") {
                url_investing <- gsub("bond-yield-", "", url_investing)
            }
        }
        url_investing 
    }
    
    retrieveWebPage <- function(this_url) {
        print(this_url)
        time_start <- Sys.time()
        time_max <- time_start + 5+0*10*60
        dat <- NULL
        
        while (is.null(dat) & (Sys.time() < time_max)) {
            dat <- U.tryNull(read_html, this_url)
            if (is.null(dat))
                Sys.sleep(10)
        }
        dat
    }
    
    tryNode_Try <- function(dat, node_name) {
        dat %>% 
            html_node(node_name) %>% 
            html_table 
    }
    
    tryNode <- function(dat, node_name)
        U.try(tryNode_Try, dat)(dat, node_name)

    formatData <- function(dat, this_pair) {
        dat <- dat %>% 
            tryNode(".datatable_table--freeze-column__2e8u1") %>% 
            tryNode(".datatable_table--freeze-column__7YoIE") %>% 
            tryNode(".datatable_table--freeze-column__XKTDf") %>% 
            tryNode(".mt-6") %>%
            tryNode("#curr_table")
        colnames(dat)[1:5] <- c("date", "close", "open", "high", "low")
        dat <- formatDate(dat)

        for (this_col in c("open", "high", "low", "close")) {
            dat[[this_col]] <- as.numeric(U.vectorize(gsub(",", "", dat[[this_col]])))
        }
        
        dat %>% 
            mutate(pair = this_pair) %>%
            select(pair, date, open, high, low, close) %>%
            arrange(date) %>%
            U.data2Tibble %>%
            T.keepOnlyRelevantHistoOHLCDates(live_or_close) %>%
            T.correctHistoBadHighLows(live_or_close == "live")
    }
    
    getInvesting_Try <- function(this_pair) {
        dat <- this_pair %>% 
            genInvestingURL %>% 
            retrieveWebPage %>% 
            formatData(this_pair)
        if (U.dfContainsData(dat)) {
            U.printBanner("Getting price history from investing.com", FALSE)
            print(tail(dat, 3) %>% data.frame)
        }
        dat
    }
    getInvesting <- function(this_pair)
        U.try(getInvesting_Try)(this_pair)
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    pair_list %>%
        lapply(getInvesting) %>%
        bind_rows
}
T.getPricesWSJ <-
function(fx_pair) 
{
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    dat <- "https://www.wsj.com/market-data/quotes/fx/%s/historical-prices" %>% 
        sprintf(fx_pair) %>%
        read_html %>%
        html_node("#cr_historical_page") %>% 
        html_text
    dat <- trim(substr(dat, gregexpr("CLOSE", dat)[[1]][1] + 5, nchar(dat))) %>% 
        strsplit(" ") %>% 
        U.vectorize %>% 
        matrix(ncol = 5, byrow = TRUE) %>% 
        data.frame
    colnames(dat) <- c("date", "open", "high", "low", "close")
    dat$date <- as.Date(dat$date, format = "%m/%d/%y")
    
    for (this_col in c("open", "high", "low", "close")) {
        dat[[this_col]] <- as.numeric(gsub(",","", U.vectorize(dat[[this_col]])))
    }
    
    dat$pair <- fx_pair
    dat <- dat %>% 
        select(pair, date, open, high, low, close) %>%            
        arrange(date) %>% 
        U.data2Tibble %>%
        T.keepOnlyRelevantHistoOHLCDates("close") %>%
        T.correctHistoBadHighLows(FALSE) %>%
        tail(3)
    if (U.dfContainsData(dat)) {
        U.printBanner("Getting price histo from WSJ", FALSE)
        print(tail(dat, 3) %>% data.frame)
    }
}
T.getPricesYahoo <-
function (pair_list, live_or_close = "close") 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    yahoo_url <- "https://query1.finance.yahoo.com/v7/finance/quote?formatted=false&symbols="
    
    yahoo_index_tickers <- D.loadTableLocal("instrument_attribute_chr") %>%
        left_join(D.loadTableLocal("instrument_attribute_type"), by = "attribute_id") %>%
        filter(attribute == "yahoo_ticker") %>%
        rename(yahoo_ticker = value) %>%
        left_join(INSTRUMENTS, ., by = "instrument_id") %>%
        na.omit %>%
        select(pair, yahoo_ticker) 
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    genYahooSymbol_Try <- function(this_pair) {
        ccy_1 <- tolower(substr(this_pair, 1, 3));
        ccy_2 <- tolower(substr(this_pair, 4, 6));
        
        assets_fx <- filter(INSTRUMENTS, asset_class %in% c("fx_em", "fx_dm"))$pair
        assets_idx <- filter(INSTRUMENTS, asset_class %in% c("index"))$pair
        assets_bond <- filter(INSTRUMENTS, asset_class %in% c("bond"))$pair
        
        yahoo_url <- CHAR_NA
        if (this_pair %in% assets_fx) {
            yahoo_url <- toupper(sprintf("%s%s=X", ccy_1, ccy_2))
        }
        else if (this_pair %in% assets_idx) {
            yahoo_url <- filter(yahoo_index_tickers, pair == this_pair)$yahoo_ticker
            if (length(yahoo_url) == 0)
                yahoo_url <- CHAR_NA
        }
        else if (this_pair %in% assets_bond) {
            yahoo_url <- filter(yahoo_index_tickers, pair == this_pair)$yahoo_ticker
            if (length(yahoo_url) == 0)
                yahoo_url <- CHAR_NA
        }
        yahoo_url %>% U.debug("url")
    }
    genYahooSymbol <- function(this_pair) 
        U.try(genYahooSymbol_Try, NULL)(this_pair)

    getYahooPrice <- function(this_pair) {
        dat <- data.frame(
            pair = this_pair, 
            yahoo_ticker = genYahooSymbol(this_pair)
            ) 
        dat <- switch(
            live_or_close,
            "live" = T.getPricesYahooLive(dat),
            "close" = T.getPricesYahooHisto(dat)
            ) 
        
        print(tail(dat, 3) %>% data.frame)
        dat %>% 
            U.noData2Null
    }

    ####################################################################################################
    ### Script 
    ####################################################################################################
    U.printBanner("Getting price history from IB recently saved files")
    pair_list %>%
        lapply(getYahooPrice) %>%
        bind_rows
}
T.getPricesYahooHisto <-
function (dat_pair) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    pair_list <- dat_pair$pair
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    getHistoYahoo_Try <- function(this_pair) {
        yahoo_ticker <- filter(dat_pair, pair == this_pair)$yahoo_ticker
        dat_histo <- yahoo_ticker %>%
            getSymbols.yahoo(auto.assign = FALSE, from = TO_DAY %m+% months(-1)) %>% 
            .[,1:4] %>% 
            T.xtsToDf
        
        colnames(dat_histo) <- yahoo_ticker %>%
            gsub("=", ".", .) %>%
            paste0(".") %>% 
            tolower %>%
            gsub("", tolower(colnames(dat_histo)))
        
        dat_histo %>%
            mutate(pair = this_pair) %>%
            select(pair, date, open, high, low, close) %>%
            U.data2Tibble %>%
            T.keepOnlyRelevantHistoOHLCDates("close") %>%
            T.correctHistoBadHighLows(FALSE)
    }
    getHistoYahoo <- function(this_pair) 
        U.try(getHistoYahoo_Try, NULL)(this_pair)
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    U.printBanner("Getting price history from yahoo")
    pair_list %>%
        lapply(getHistoYahoo) %>%
        bind_rows
}
T.getPricesYahooLive <-
function (dat_pair) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    yahoo_url <- "https://query1.finance.yahoo.com/v7/finance/quote?formatted=false&symbols="
    pair_list <- dat_pair$pair
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    getLivePriceYahoo_Try <- function(this_pair) {
        dat_pair %>%
            filter(pair == this_pair) %>%
            .$yahoo_ticker %>%
            paste0(yahoo_url, .) %>%
            fromJSON %>%
            .$quoteResponse %>%
            .$result %>%
            select(
                regularMarketOpen, 
                regularMarketDayHigh, 
                regularMarketDayLow, 
                regularMarketPrice
            ) %>%
            data.frame(
                pair = this_pair,
                date = TO_DAY,
                .
            ) %>% 
            U.data2Tibble %>%
            rename(
                open = regularMarketOpen,
                high = regularMarketDayHigh,
                low = regularMarketDayLow,
                close = regularMarketPrice
            ) %>%
            T.correctHistoBadHighLows(TRUE)
    }
    getLivePriceYahoo <- function(this_pair) 
        U.try(getLivePriceYahoo_Try, NULL)(this_pair)
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    U.printBanner("Getting price live from yahoo")
    pair_list %>%
        lapply(getLivePriceYahoo) %>%
        bind_rows
}
T.getSTIPriceFromWSJ <-
function() 
{
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    U.printBanner("Getting price histo for STISGD from WSJ")
    
    dat <- "https://www.wsj.com/market-data/quotes/index/SG/STI/historical-prices" %>% 
        read_html %>%
        html_node("#cr_historical_page") %>% 
        html_text
    dat <- trim(substr(dat, gregexpr("CLOSE", dat)[[1]][1] + 5, nchar(dat))) %>% 
        strsplit(" ") %>% 
        U.vectorize %>% 
        matrix(ncol = 5, byrow = TRUE) %>% 
        data.frame
    colnames(dat) <- c("date", "open", "high", "low", "close")
    dat$date <- as.Date(dat$date, format = "%m/%d/%y")
    
    for (this_col in c("open", "high", "low", "close")) {
        dat[[this_col]] <- as.numeric(gsub(",","", U.vectorize(dat[[this_col]])))
    }
    
    dat$pair <- "STISGD"
    dat %>% 
        select(pair, date, open, high, low, close) %>%            
        arrange(date) %>% 
        U.data2Tibble
}
T.getTechnicals <-
function (fx_pair = NULL, i_strat = NULL, import_training_tech_from_file = FALSE, 
          keep_all_features = FALSE)
{
    ####################################################################################################
    ### Script_variables
    ####################################################################################################
    instrument_id <- A.getInstrumentId(fx_pair);
    if (!is.null(i_strat)) {
        n_days_trade_id <- filter(STRATEGIES, strategy_id == i_strat)$max_duration_id
        bb_width_id <- filter(STRATEGIES, strategy_id == i_strat)$bb_width_id
    }
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    prepareQueryFeaturesAllInstrumentsAllFeatures <- function(int_or_dbl) {
        "SELECT T.instrument_id, T.date, F.feature, T.value 
        FROM histo_technicals_%s T
        LEFT JOIN static_feature F ON F.feature_id = T.feature_id" %>%
            sprintf(int_or_dbl)
    }
    
    prepareQueryFeaturesOneInstrumentAllFeatures <- function(int_or_dbl) {
        "SELECT T.instrument_id, T.date, F.feature, T.value 
        FROM histo_technicals_%s T
        LEFT JOIN static_feature F ON F.feature_id = T.feature_id
        WHERE T.instrument_id = %s" %>%
            sprintf(int_or_dbl, instrument_id)
    }
    
    prepareQueryFeaturesAllTrainInstrumentsAllFeatures <- function(int_or_dbl) {
        "SELECT T.instrument_id, T.date, F.feature, T.value 
        FROM histo_technicals_%s T
        LEFT JOIN static_feature F ON F.feature_id = T.feature_id
        WHERE T.instrument_id IN (
                SELECT instrument_id FROM static_instrument WHERE use_for_training = 1
                )" %>%
            sprintf(int_or_dbl)
    }
    
    prepareQueryFeaturesAllInstrumentsStratFeatures <- function(int_or_dbl) {
        "SELECT T.instrument_id, T.date, F.feature, T.value 
        FROM histo_technicals_%s T
        LEFT JOIN static_feature F ON F.feature_id = T.feature_id
        WHERE T.feature_id IN (
            SELECT feature_id
            FROM strategy_feature
            WHERE strategy_id = %s
        )" %>%
            sprintf(int_or_dbl, i_strat)
    }
    
    prepareQueryFeaturesOneInstrumentStratFeatures <- function(int_or_dbl) {
        "SELECT T.instrument_id, T.date, F.feature, T.value 
        FROM histo_technicals_%s T
        LEFT JOIN static_feature F ON F.feature_id = T.feature_id
        WHERE T.instrument_id = %s
        AND T.feature_id IN (
            SELECT feature_id
            FROM strategy_feature
            WHERE strategy_id = %s
        )" %>%
            sprintf(int_or_dbl, instrument_id, i_strat)
    }
    
    prepareQueryFeaturesAllTrainInstrumentsStratFeatures <- function(int_or_dbl) {
        "SELECT T.instrument_id, T.date, F.feature, T.value 
        FROM histo_technicals_%s T
        LEFT JOIN static_feature F ON F.feature_id = T.feature_id
        WHERE T.instrument_id IN (
                SELECT instrument_id FROM static_instrument WHERE use_for_training = 1
                )
        AND T.feature_id IN (
            SELECT feature_id
            FROM strategy_feature
            WHERE strategy_id = %s
        )" %>%
            sprintf(int_or_dbl, i_strat)
    }
    
    retrieveFeatures <- function(int_or_dbl) {
        sql_q <- 
            if (is.null(i_strat)) {
                if (is.null(fx_pair)) 
                    prepareQueryFeaturesAllInstrumentsAllFeatures(int_or_dbl)
                else if (fx_pair == "train")
                    prepareQueryFeaturesAllTrainInstrumentsAllFeatures(int_or_dbl)
                else 
                    prepareQueryFeaturesOneInstrumentAllFeatures(int_or_dbl)
            } else {
                if (is.null(fx_pair)) 
                    prepareQueryFeaturesAllInstrumentsStratFeatures(int_or_dbl)
                else if (fx_pair == "train")
                    prepareQueryFeaturesAllTrainInstrumentsStratFeatures(int_or_dbl)
                else 
                    prepareQueryFeaturesOneInstrumentStratFeatures(int_or_dbl)
            }
        D.select(sql_q, TRUE, FALSE)
    }
    
    formatTechData <- function(dat) {
        dat %>%
            filter(!(
                feature %in% 
                    c(
                        "t_up", "t_dn", "hit_hi", "hit_lo", 
                        "trade_outcome_id", "tgt", "duration", "px_exit", "date_exit", 
                        "rtn"
                    ))
            ) %>%
            spread(feature, value)
    }
    
    retrieveTechDataFromFile <- function() {
        strategy_features <- "SELECT F.feature
            FROM strategy_feature S
            LEFT JOIN static_feature F ON F.feature_id = S.feature_id
            WHERE S.strategy_id = %s" %>%
            sprintf(i_strat) %>%
            D.select %>%
            U.vectorize
        
        load(paste0(DIRECTORY_DATA_SD, "Technicals/Technicals.RData"))
        
        dat <- dat %>%
            semi_join(filter(INSTRUMENTS, use_for_training == 1), by = "instrument_id") 
        
        if (!keep_all_features) {
            cols_to_keep <- c("instrument_id", "date", strategy_features) %>%
                intersect(colnames(dat))
            dat <- dat %>% 
                select(all_of(cols_to_keep))
        }
        
        dat[,setdiff(colnames(dat), "asset_class")]
    }   
    
    retrieveTechDataFromDB <- function() {
        retrieveFeatures("int") %>%
            rbind(retrieveFeatures("dbl")) %>%
            formatTechData
    }
    
    retrieveTechData <- function() {
        if (import_training_tech_from_file) {
            retrieveTechDataFromFile()
        } else {
            retrieveTechDataFromDB()
        }
    }
    
    prepareOutcomeQueryForAllInstruments <- function() {
        "SELECT *
            FROM histo_trade_outcome T
            WHERE T.max_duration_id = %s AND T.bb_width_id = %s" %>%
            sprintf(n_days_trade_id, bb_width_id);
    }
    
    prepareOutcomeQueryForTrainInstruments <- function() {
        "SELECT *
            FROM histo_trade_outcome T
            WHERE T.max_duration_id = %s AND T.bb_width_id = %s 
            AND T.instrument_id IN (
                SELECT instrument_id FROM static_instrument WHERE use_for_training = 1
                )" %>%
            sprintf(n_days_trade_id, bb_width_id);
    }
    
    prepareOutcomeQueryForOneInstrument <- function() {
        "SELECT *
            FROM histo_trade_outcome T
            WHERE T.max_duration_id = %s AND T.bb_width_id = %s AND
            T.instrument_id = %s" %>%
            sprintf(n_days_trade_id, bb_width_id, instrument_id);
    }
    
    retrieveTechOutcome <- function() {
        sql_q <- NULL;
        if (!is.null(i_strat)) {
            if (is.null(fx_pair)) 
                sql_q <- prepareOutcomeQueryForAllInstruments()
            else if (fx_pair == "train")
                sql_q <- prepareOutcomeQueryForTrainInstruments()
            else 
                sql_q <- prepareOutcomeQueryForOneInstrument()
        }
        
        res <- NULL
        if (!is.null(i_strat)) {
            res <- sql_q %>%
                D.select(TRUE, FALSE) %>%
                left_join(TRADE_OUTCOMES, by = "outcome_id") %>%
                select(-max_duration_id, -outcome_id, -bb_width_id) %>%
                rename(tgt = outcome) %>%
                filter(tgt %in% c("up", "flat", "down"))
        }
        res
    }
    
    combineData <- function(dat_px, dat_tech, dat_trd) {
        dat_combined <- dat_px %>% 
            select(-open, -high, -low, -close) %>%
            left_join(select(INSTRUMENTS, instrument_id, asset_class), by = "instrument_id") %>% 
            left_join(dat_tech, by = c("instrument_id", "date")) %>%
            mutate(
                asset_class = factor(
                    asset_class, 
                    levels = c("fx_dm", "fx_em", "index", "metal", "yield", "bond")
                )
            )
        if (!is.null(dat_trd)) {
            dat_combined <- dat_combined %>%
                left_join(dat_trd, by = c("instrument_id", "date")) %>%
                mutate(
                    tgt = factor(tgt, levels = c("up", "flat", "down"))
                ) 
        }
        
        if ("ccy_1_reference" %in% colnames(dat_combined)) {
            dat_combined <- dat_combined %>%
                select(-ccy_1_reference) %>%
                T.addFXPriority
        } 
        
        dat_combined %>% 
            select(setdiff(colnames(dat_combined), "asset_class_id")) %>%
            arrange(instrument_id, date) %>%
            U.noData2Null
    }
    
    loadFile_Try <- function() {
        dat_px <- T.getHistoPx(instrument_id)
        dat_tech <- retrieveTechData()
        dat_trd <- retrieveTechOutcome()
        combineData(dat_px, dat_tech, dat_trd)
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    res <- U.try(loadFile_Try, NULL )()
    gc()
    res
}
T.getTechnicalsLive <-
function (scenario_bump = NULL, any_time = FALSE) 
{
    ####################################################################################################
    ### Script_variables
    ####################################################################################################
    FEATURES <- D.select("SELECT feature_id, feature FROM static_feature") %>%
        filter(
            feature != "asset_class_id",
            feature != "ccy_1_reference"
        )
    if (!is.null(scenario_bump)) {
        bump_id <- D.SQL(
            paste0("SELECT bump_id FROM static_scenario_bump WHERE bump = ", scenario_bump)
        )$bump_id
    }
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    prepareInstrumentsToDo <- function() {
        INSTRUMENTS %>% 
            filter(use_for_trading == 1) %>% 
            select(instrument_id) %>% 
            U.vectorize;
    }
    
    combineData <- function(dat_px, dat_tech) {
        dat_px %>% 
            select(-open, -high, -low, -close) %>%
            left_join(select(INSTRUMENTS, instrument_id, asset_class), by = "instrument_id") %>%
            left_join(dat_tech, by = c("instrument_id", "date")) %>%
            T.addFXPriority %>%
            mutate(
                asset_class = factor(
                    asset_class, 
                    levels = c("fx_dm", "fx_em", "index", "metal", "yield", "bond")
                )
            ) %>% 
            arrange(instrument_id, date) %>%
            U.noData2Null
    }
    
    getTechnicalsLive <- function() {
        sql_q <- "SELECT instrument_id, date, timestamp_px, feature_id, value FROM live_technicals_%s" 
        D.SQL(sprintf(sql_q, "int")) %>%
            rbind(D.SQL(sprintf(sql_q, "dbl")))
    }
    getTechnicalsScenario <- function() {
        sql_q <- 
            "SELECT bump_id, instrument_id, date, feature_id, value 
        FROM scenario_technicals_%s
        WHERE bump_id = %s"
        
        D.SQL(sprintf(sql_q, "int", bump_id)) %>%
            rbind(D.SQL(sprintf(sql_q, "dbl", bump_id))) %>% 
            mutate(timestamp_px = NA)
    }
    
    keepOnlyMostRecent <- function(dat) {
        if (U.variableExists(start_time)) {
            dat <- dat %>% 
                filter(timestamp_px >= start_time - 15*60)
        }
        dat
    }
    
    getTechnicals_Try <- function() {
        instruments_list <- prepareInstrumentsToDo()
        
        dat_px <- "SELECT instrument_id, date, open, high, low, close FROM live_px" %>%
            D.select %>%
            filter(instrument_id %in% instruments_list)
        
        dat_tech <- (
            if (is.null(scenario_bump)) 
            { getTechnicalsLive() } 
            else 
            { getTechnicalsScenario() }
        ) %>% 
            mutate(
                date = as.Date(date),
                timestamp_px = as.POSIXct(timestamp_px, tz = TZ_LOCAL)
            ) %>% 
            filter(instrument_id %in% instruments_list) %>% 
            keepOnlyMostRecent %>%
            left_join(FEATURES, by = "feature_id") %>% 
            filter(!is.na(feature_id), !is.na(feature)) %>% 
            select(-feature_id) %>%
            spread(feature, value)
        
        combineData(dat_px, dat_tech)
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    U.try(getTechnicals_Try, NULL )()
    
}
T.histoPXvsUSD <-
function (only_last = FALSE, min_date = NULL) 
{
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    if (is.null(min_date)) {
        min_date <- YESTERDAY - 10
    }
    min_date <- as.Date(min_date)
    
    date_list <- seq(min_date, TO_DAY, 1)
    date_list_extra <- seq(min_date - 14, TO_DAY, 1)
    
    dat_px_histo <- D.select(paste0("SELECT * FROM histo_px_daily WHERE date >= ", min_date))
    dat_px_live <- D.loadTable("live_px") %>%
        anti_join(dat_px_histo, by = c("instrument_id", "date")) %>%
        select(-timestamp)
    
    dat_px <- rbind(dat_px_histo, dat_px_live) %>%
        left_join(select(INSTRUMENTS, instrument_id, pair), by = "instrument_id")
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    invertFXifNecessary <- function(dat_fx, ccy_1) {
        if (ccy_1 == "USD") {
            dat_fx <- dat_fx %>%
                mutate(fx = 1 / fx)
        }
        dat_fx
    }
    
    prepareHistoFXPair <- function(this_pair) {
        ccy <- gsub("USD", "", this_pair)
        ccy_1 <- substr(this_pair, 1, 3)
        ccy_2 <- substr(this_pair, 4, 6)
        
        if (this_pair == "USDCNY") {
            dat_fx <- prepareHistoFXPair("USDCNH") %>%
                mutate(pair = "USDCNY")
        }
        else if (this_pair %in% paste0("USD", c("CZK", "DKK", "HUF", "PLN"))) {
            dat_fx_eur <- prepareHistoFXPair("EURUSD") %>% 
                rename(EUR = fx) %>%
                select(-pair)
            dat_fx_ccy <- prepareHistoFXPair(paste0("EUR", ccy_2)) %>%
                mutate(pair = this_pair);
            dat_fx <- dat_fx_ccy %>%
                left_join(dat_fx_eur, by = "date") %>%
                mutate(fx = EUR / fx) %>%
                select(-EUR)
        }
        else {
            dat_pair <- dat_px %>% 
                filter(pair == this_pair) %>% 
                arrange(date) %>%
                select(date, close) %>%
                rename(fx = close)
            
            dat_fx <- data.frame(
                pair = this_pair, 
                date = date_list_extra
            ) %>% 
                U.dataFrame %>%
                left_join(dat_pair, by = "date") %>%
                mutate(fx = na.locf0(fx)) %>%
                mutate(fx = na.locf0(fx, fromLast = TRUE)) %>%
                filter(date >= min_date) %>% 
                invertFXifNecessary(ccy_1)
        }
        
        dat_fx
    }
    
    prepareHistoIndex <- function(this_idx, dat_histo_fx) {
        
        ccy_idx <- substr(this_idx, 4, 6);
        if (ccy_idx == "CNY") {
            ccy_idx <- "CNH"
        }
        
        dat_histo_idx <- dat_px %>% 
            filter(pair == this_idx) %>% 
            arrange(date) %>%
            select(date, close) %>%
            rename(px = close)
        
        dat_histo_idx <- data.frame(
            pair = this_idx, 
            date = date_list_extra
        ) %>%
            U.dataFrame %>% 
            left_join(dat_histo_idx, by = "date") %>%
            mutate(px = na.locf0(px)) %>%
            mutate(px = na.locf0(px, fromLast = TRUE)) %>%
            filter(date >= min_date) %>%
            mutate(ccy = ccy_idx) %>%
            left_join(dat_histo_fx, by = c("ccy", "date")) %>%
            mutate(
                ccy = substr(pair, 1, 3),
                fx = px * fx
            ) %>% 
            select(ccy, date, fx)
        
        if (this_idx == "OMXDKK") {
            dat_histo_idx$ccy <- "OMD"
        }
        dat_histo_idx
        
    }
    
    prepareHistoFX <- function() {
        dat_fx <- c(
            "EURUSD", "AUDUSD", "USDJPY", "NZDUSD", "USDMXN", "USDBRL", "USDCAD", "USDCHF",
            "USDCZK", "USDDKK", "GBPUSD", "USDHUF", "USDNOK", "USDPLN", "USDSEK", "USDHKD",
            "USDKRW", "USDCNH", "USDCNY", "USDSGD", "USDILS", "USDCLP", "USDIDR", "USDINR",
            "USDMYR", "USDPHP", "USDRUB", "USDTHB", "USDTRY", "USDTWD", "USDVND", "USDZAR",
            "XAUUSD", "XAGUSD", "XPTUSD", "XPDUSD",
            "BTCUSD"
        ) %>%
            lapply(prepareHistoFXPair) %>%
            bind_rows %>%
            U.data2Tibble %>%
            mutate(pair = gsub("USD", "", pair)) %>%
            rename(ccy = pair);
        
        dat_fx_usd <- dat_fx %>% 
            filter(ccy == "JPY") %>%
            mutate(
                ccy = "USD",
                fx = 1
            );
        
        rbind(dat_fx, dat_fx_usd) %>%
            arrange(ccy, date)
    }
    
    prepareHistoIndices <- function(dat_fx) {
        prepareHistoIndex_local <- function(this_pair) prepareHistoIndex(this_pair, dat_fx);
        dat_idx <- filter(INSTRUMENTS, asset_class %in% c("index", "bond"))$pair %>% 
            lapply(prepareHistoIndex_local) %>%
            bind_rows %>%
            U.data2Tibble
    }
    
    prepareHisto <- function() {
        dat_fx <- prepareHistoFX()
        dat_idx <- prepareHistoIndices(dat_fx)
        
        dat_fx <- rbind(dat_fx, dat_idx) %>% 
            arrange(ccy, date)
        
        if (only_last) {
            dat_fx <- dat_fx %>%
                group_by(ccy) %>%
                summarize(fx = last(fx)) %>%
                ungroup
        }
        
        dat_fx
    }
    
    saveToDB <- function(dat) {
        dat %>%
            rename(code = ccy) %>%
            left_join(ASSETS, by = "code") %>%
            select(asset_id, date, fx) %>%
            D.replaceDataIntoTable("histo_fx_close_vs_usd", .)
        NULL
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    prepareHisto() %>%
        saveToDB
    
}
T.importInvestingComHistoFile <-
function (this_pair, save_to_db=FALSE) 
{
    
    # for (pair in filter(INSTRUMENTS, use_for_trading + use_for_training >= 1)$pair %>% tail(-10)) {
    #    T.plotPriceSeries(pair, 2022);
    #    Sys.sleep(2)
    # }
    
    #dat <- filter(INSTRUMENTS, use_for_trading + use_for_training >= 1) %>% 
    #    arrange(asset_class, pair) %>% 
    #    filter(use_for_trading + use_for_training >= 1)
    
    
    #i <- i+1; 
    #i;
    #fx_pair <- dat$pair[i];
    #dat[i,];
    #list.files("/home/fls/Downloads", full.names = TRUE) %>% 
    #    keep(~ str_detect(.x, "Historical Data") && str_detect(.x, paste0(U.left(fx_pair,3), "_", U.right(fx_pair, 3))) && str_ends(.x, ".csv")) %>% 
    #    unlink
    #T.plotPriceSeries(fx_pair, 2020);
    
    # Manually get file /Downloads/EUR_USD Historical Data.csv starting 2y ago
    
    #instrument_id <- filter(INSTRUMENTS, pair == fx_pair)$instrument_id; print(instrument_id);
    #"DELETE FROM histo_px_daily WHERE instrument_id = %s AND date >= '2024-01-01'" %>% sprintf(instrument_id) %>% D.SQL;
    #T.importInvestingComHistoFile(fx_pair, TRUE);
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    file_name <- "/home/fls/Downloads/%s_%s Historical Data.csv"
    file_path <- "/home/fls/Downloads/"
    
    dat_histo_ohlc <- D.loadTable("histo_px_daily") %>%
        left_join(select(INSTRUMENTS, pair, instrument_id), by = "instrument_id") %>% 
        select(pair, date, open, high, low, close)
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    renameCols <- function(dat) {
        colnames(dat) <- tolower(colnames(dat))
        dat
    }
    
    formatDate <- function(dat) {
        date_format <- "%m/%d/%Y"
        if (grepl(",", dat$Date[1])) date_format <- "%B %d, %Y"
        dat %>% 
            mutate(
                Date = as.Date(Date, format = date_format)
            )
    }
    
    readFileAndFormatDate <- function(file_name) {
        file_path %>% 
            paste0(file_name) %>% 
            read.csv(stringsAsFactors = FALSE) %>% 
            formatDate
    }
    
    fileOpen <- function() {
        seek_expr <- paste0(substr(this_pair, 1, 3), "_", substr(this_pair, 4, 6))
        file_path %>%
            list.files %>% 
            .[which(U.sapply(., function(x) gregexpr(seek_expr, x)[1] != -1))] %>%
            lapply(readFileAndFormatDate) %>% 
            bind_rows %>% 
            unique %>% 
            arrange(Date)
    }
    
    reformatDataInvesting <- function(dat) {
        dat %>% 
            mutate(
                pair = this_pair
            ) %>% 
            rename(Close = Price) %>% 
            select(pair, Date, Open, High, Low, Close) %>% 
            mutate(
                Open = as.numeric(U.vectorize(gsub(",", "", Open))),
                High = as.numeric(U.vectorize(gsub(",", "", High))),
                Low = as.numeric(U.vectorize(gsub(",", "", Low))),
                Close = as.numeric(U.vectorize(gsub(",", "", Close)))
            ) %>% 
            U.data2Tibble %>%
            renameCols %>% 
            filter(date <= YESTERDAY) %>%
            arrange(pair, date)
    }
    
    reformatDataTradingView <- function(dat) {
        dat %>% 
            rename(
                date = time
            ) %>%
            mutate(
                date = as.Date(substr(date, 1, 10)),
                pair = this_pair
            ) %>% 
            select(pair, date, open, high, low, close) %>% 
            mutate(
                open = as.numeric(U.vectorize(gsub(",", "", open))),
                high = as.numeric(U.vectorize(gsub(",", "", high))),
                low = as.numeric(U.vectorize(gsub(",", "", low))),
                close = as.numeric(U.vectorize(gsub(",", "", close)))
            ) %>% 
            U.data2Tibble %>%
            renameCols %>% 
            filter(date < YESTERDAY) %>%
            arrange(pair, date)
    }
    
    reformatData <- function(dat) {
        data_source <- "investing";
        if (colnames(dat)[1] == "time") {
            data_source <- "tradingview";    
        }
        switch(
            data_source,
            "investing" = reformatDataInvesting(dat),
            "tradingview" = reformatDataTradingView(dat)
        )
    }
    
    removeDataAlreadyIn <- function(dat) {
        dat %>%
            anti_join(dat_histo_ohlc, by = c("pair", "date")) %>%
            U.debug("New Data")
    }
    
    correctBadHighLows <- function(dat_histo_ohlc) {
        maxOHLC <- function(a, b, c, d) max(a,b,c,d);
        minOHLC <- function(a, b, c, d) min(a,b,c,d);
        dat_histo_ohlc$new_high <- mapply(
            maxOHLC, 
            dat_histo_ohlc$open, 
            dat_histo_ohlc$high, 
            dat_histo_ohlc$low, 
            dat_histo_ohlc$close
        );
        dat_histo_ohlc$new_low <- mapply(
            minOHLC, 
            dat_histo_ohlc$open, 
            dat_histo_ohlc$high, 
            dat_histo_ohlc$low, 
            dat_histo_ohlc$close
        );
        dat_histo_ohlc$high <- dat_histo_ohlc$new_high;
        dat_histo_ohlc$low <- dat_histo_ohlc$new_low;
        dat_histo_ohlc %>% 
            select(-new_high, -new_low)
    }
    
    finalFormatting <- function(dat_histo_ohlc) {
        dat_histo_ohlc <- dat_histo_ohlc %>% 
            arrange(pair, date) %>% 
            mutate(weekday = weekdays(date)) %>% 
            filter(!weekday %in% c("Saturday", "Sunday")) %>%
            select(-weekday) %>%
            U.data2Tibble
        dat_histo_ohlc[which(!duplicated(select(dat_histo_ohlc, pair, date))),]
    }
    combineWithExistingData <- function(dat) {
        dat_histo_ohlc <- dat_histo_ohlc %>% 
            rbind(dat) %>% 
            arrange(pair, date) %>%
            correctBadHighLows %>%
            finalFormatting
        U.printBanner("Summary of holes in data")
        print(
            dat_histo_ohlc %>% 
                group_by(pair) %>% 
                mutate(diffdays = date - lag(date,1)) %>% 
                ungroup %>% 
                group_by(pair) %>% 
                summarize(
                    max_days = max(diffdays, na.rm = TRUE), 
                    max_day = date[which.max(diffdays)],
                    start_date = min(date),
                    end_date = max(date)
                ) %>% 
                ungroup %>%
                left_join(select(INSTRUMENTS, pair, asset_class), by="pair") %>% 
                arrange(asset_class, pair) %>% 
                select(asset_class, pair, max_days, max_day, start_date, end_date) %>%
                #     filter(start_date > "2022-01-01") %>%
                data.frame
        )
        
        dat_histo_ohlc
    }
    
    saveToDB <- function(dat_histo_ohlc) {
        if (save_to_db) {
            this_instrument_id <- A.getInstrumentId(this_pair)
            dat_histo_ohlc <- dat_histo_ohlc %>% 
                filter(pair == this_pair) %>% 
                mutate(instrument_id = this_instrument_id) %>% 
                select(instrument_id, date, open, high, low, close) %>% 
                D.replaceDataIntoTable("histo_px_daily", ., TRUE)
            
            T.plotPriceSeries(this_pair, year(Sys.Date())-2, year(Sys.Date()))
        }
        dat_histo_ohlc
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    fileOpen() %>% 
        reformatData %>%
        removeDataAlreadyIn %>%
        combineWithExistingData %>%
        saveToDB
    
}
T.instrumentOpenCloseTimes <-
function (instrument_list)
{
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    instrument_id_list <- U.sapply(instrument_list, A.getInstrumentId)
    
    res <- INSTRUMENTS %>% 
        filter(instrument_id %in% instrument_id_list) %>% 
        select(instrument_id, market)
    
    market_list <- res$market %>% U.vectorizeUnique
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    market_hours <- market_list %>% 
        lapply(T.marketOpenCloseTimes) %>%
        bind_rows
    
    res %>% 
        left_join(market_hours, by = "market") %>%
        select(-market)
    
    
}
T.keepOnlyRelevantHistoOHLCDates <-
function(dat, live_or_close) {
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    pickLastRowAndMarkDateAsTodayInCaseSourceHasAlreadySwitchedDayOrNotOpenYet <- function(dat) {
        dat <- dat %>%
            filter(date >= YESTERDAY) %>%
            group_by(pair) %>% 
            tail(1) %>%
            ungroup %>%
            mutate(
                open = case_when(date == YESTERDAY ~ close, TRUE ~ open),
                high = case_when(date == YESTERDAY ~ close, TRUE ~ high),
                low = case_when(date == YESTERDAY ~ close, TRUE ~ low),
                date = TO_DAY
            )
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    switch(
        live_or_close,
        "live" = pickLastRowAndMarkDateAsTodayInCaseSourceHasAlreadySwitchedDayOrNotOpenYet(dat),
        "close" = filter(dat, date < TO_DAY)
    )
}
T.loadPrecomputedTradingData <-
function (dat_ohlc, this_strat = VENTURA$strats$strat_1) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    n_days <- this_strat$trading_param$n_days_trade;
    bb_width <- this_strat$trading_param$bb_band_target_width;
    this_pair <- dat_ohlc$pair[1];
    file_dir <- paste0(DIRECTORY_DATA_HD, "Spot/Technicals_Trading_Data/");
    file_name <- "%s/%s_%s_%s.csv" %>%
        sprintf(file_dir, this_pair, n_days, 100 * bb_width);
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    loadTradingDataOnePair_Try <- function(dat) {
        dat_trading <- U.read.csv(file_name);
        dat %>% 
            select(-t_up, -t_dn, -hit_hi, -hit_lo, -tgt, -duration, -px_exit, -date_exit, -rtn) %>%
            left_join(dat_trading, by = c("pair", "date"));
    }
    
    loadTradingDataOnePair <- function(dat) 
        U.try(loadTradingDataOnePair_Try, dat)(dat)
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    loadTradingDataOnePair(dat_ohlc)
    
    
}
T.loadTechFile <-
function (fx_pair, this_strat = VENTURA$strats$strat_1) 
{
    ####################################################################################################
    ### Script_variables
    ####################################################################################################
    this_strat_param <- this_strat$trading_param
    main_strat_param <- VENTURA$strats$strat_1$trading_param
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    fillMissingHighAndLowWithClose <- function(dat) {
        dat$high_na <- dat$high
        dat$low_na <- dat$low
        
        pos_na_hi <- which(is.na(dat$high))
        if (length(pos_na_hi) > 0) {
            dat$high_na[pos_na_hi] <- dat$close[pos_na_hi]
        }
        
        pos_na_lo <- which(is.na(dat$low))
        if (length(pos_na_lo) > 0) {
            dat$low_na[pos_na_lo] <- dat$close[pos_na_lo]
        }
        dat;
    }
    
    fillMissingOpenWithPreviousClose <- function(dat) {
        dat$open_na <- dat$open;
        pos_na_open <- which(is.na(dat$open)) %>% setdiff(1)
        if (length(pos_na_open) > 0) {
            dat$open_na[pos_na_open] <- dat$close[pos_na_open - 1]
        }
        dat
    }
    
    finalCleanData <- function(dat) {
        dat <- U.dataFrame(dat)
            
        dat0 <- dat_histo_ohlc %>%
            filter(pair == fx_pair) %>%
            rename(open_f = open, high_f = high, low_f = low) %>%
            select(date, open_f, high_f, low_f)
        
        dat <- dat %>%
            left_join(dat0, by = "date") %>%
            mutate(open = open_f, high = high_f, low = low_f) %>%
            select(-open_f, -high_f, -low_f)
        
        dat <- dat[,setdiff(colnames(dat), c("open_na", "low_na", "high_na"))]
        
        for (j in 1:ncol(dat)) {
            dat[which(is.nan(dat[,j])),j] <- NA
            dat[which(is.infinite(dat[,j])),j] <- NA
        }
        dat <- T.addAssetClass(dat)
            
        dat$tgt <- as.factor(dat$tgt)
        dat
    }
    
    
    loadFile_Try <- function(fx_pair) {
        load(sprintf(VENTURA$technical_param$tech_file_name, fx_pair))
        
        do_we_need_to_recompute_trading_data <- (
            (this_strat_param$bb_band_target_width != main_strat_param$bb_band_target_width) |
                (this_strat_param$n_days_trade != main_strat_param$n_days_trade)
        )   
        
        if (do_we_need_to_recompute_trading_data) {
            dat <- T.loadPrecomputedTradingData(dat, this_strat)
        }
        
    #    dat0 <- dat;
    #    dat <- dat %>% 
    #        fillMissingHighAndLowWithClose %>%
    #        fillMissingOpenWithPreviousClose %>%
    #        T.addRecentHiLo;
        #        addMADVol %>%
        #        fillMissingHighAndLowWithClose %>%
        #        fillMissingOpenWithPreviousClose %>%
        #        T.addMACDFLS %>% 
        #        T.addSimpleStuff %>%
        #        T.addLTVolAverage %>% 
        #        T.addRecentHiLo %>%
        #        T.addMACDFLS %>%
        #        T.addPareto %>%
        #        select(-open_na, -high_na, -low_na);
      #  dat$open <- dat0$open;
      #  dat$high <- dat0$high;
      #  dat$low <- dat0$low;
        
        #    dat1 <- "%sSpot/SuperExponentiality/%s.csv" %>%
        #        sprintf(DIRECTORY_DATA, fx_pair) %>% 
        #        U.read.csv %>%
        #        select(date, sup_exp_close, sup_exp_high, sub_exp_low) %>%
        #        mutate(
        #            sup_exp_close_lagged = lag(sup_exp_close, 5),
        #            sup_exp_close_chg = sup_exp_close - lag(sup_exp_close_lagged, 5)
        #       )
        cols_keep <- setdiff(
            colnames(dat), 
            colnames(dat)[grepl("nb_x_slope_chg_", colnames(dat))]
        );
        
        #  dat <- dat[,cols_keep];
        dat <- dat[,setdiff(colnames(dat), c("open_na", "high_na", "low_na"))];
        T.addAssetClass(dat)
            
        #    T.addCandleShadowsMeasure;
        #            T.addSplineDivergence;
        
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    #       U.printBanner(fx_pair, FALSE)
    U.try(loadFile_Try, NULL )(fx_pair)
}
T.marketOpenCloseTimes <-
function (market) 
{
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    getMarketHours <- function() {
        dat <- "SELECT T1.time_zone AS tz_open, T2.time_zone as tz_close, M.time_open, M.time_close
        FROM static_market M
        LEFT JOIN static_time_zone T1 ON T1.time_zone_id = M.tz_open_id
        LEFT JOIN static_time_zone T2 ON T2.time_zone_id = M.tz_close_id
        WHERE M.market = '%s'" %>% 
            sprintf(market) %>%
            D.SQL %>%
            mutate(
                tz_open = case_when(is.na(tz_open) ~ tz_close, TRUE ~ tz_open),
                time_open = case_when(is.na(time_open) ~ time_close, TRUE ~ time_open)
            )
        
        time_close <- paste0(TO_DAY, " ", dat$time_close) %>%
            as.POSIXct(tz = dat$tz_close)
        
        time_close_yesterday <- paste0(YESTERDAY, " ", dat$time_close) %>%
            as.POSIXct(tz = dat$tz_close)
        
        time_open <- paste0(TO_DAY, " ", dat$time_open) %>%
            as.POSIXct(tz = dat$tz_open)
        
        if (time_open == time_close) {
            time_open <- time_close %m+% days(-1)
        }
        else if (time_open <= time_close_yesterday) {
            time_open <- time_close_yesterday
        }
        
        data.frame(market, time_open, time_close)
    }
    

    
    ####################################################################################################
    ### Script
    ####################################################################################################
    U.try(getMarketHours, NULL)()
}
T.observeTradingPoint <-
function (dat_trd, date_trade) 
{
    date_trade <- as.Date(date_trade);
    date_start_plot <- date_trade %m+% months(-3);
    date_end_plot <- date_trade %m+% months(6);
    
    plot_data_1 <- dat_trd %>% 
        filter(date >= date_start_plot, date <= date_end_plot) %>% 
        select(date, close, bb_up_d, bb_dn_d, sma20_d, sma20_w, sma20_m) %>% 
        gather(type, price, -date); 
    plot_1 <- ggplot(plot_data_1, aes(x = date, y = price, col = type)) + 
        geom_line(
            data = filter(plot_data_1, type == "close"),
            size = 2
        ) + 
        geom_line(
            data = filter(plot_data_1, substr(type, 1, 2) == "bb"),
            size = 0.5,
            linetype = "dashed"
        ) + 
        geom_ribbon(
            data = filter(plot_data_1, substr(type, 1, 2) == "bb") %>%
                spread(type, price),
            aes(x = date, ymin=bb_dn_d, ymax=bb_up_d, fill = "lightsalmon"),
            alpha = 0.25,
            inherit.aes = FALSE
        ) + 
        geom_line(
            data = filter(plot_data_1, substr(type, 1, 3) == "sma"),
            size = 0.5
        ) + 
        geom_point(
            data = filter(plot_data_1, type == "close", date == date_trade),
            size = 5,
            col = "red"
        ) +
        geom_vline(xintercept = date_trade, size = 0.25, col = "red", linetype = "dashed") +
        theme(
            legend.position = "bottom",
            axis.text=element_text(size=12), 
            axis.title.y = element_blank(),
            axis.title.x = element_blank()
        )  
    
    plot_data_2 <- dat_trd %>% 
        filter(date >= date_start_plot, date <= date_end_plot) %>% 
        select(date, starts_with("macdzl")) %>% 
        gather(type, macd, -date);
    
    plot_2 <- ggplot(plot_data_2, aes(x = date, y = macd, col = type)) + 
        geom_line() + 
        geom_vline(xintercept = date_trade, size = 0.25, col = "red", linetype = "dashed") +
        geom_hline(yintercept = 0, size = 0.25) +
        theme(
            legend.position = "bottom",
            axis.text=element_text(size=12), 
            axis.title.y = element_blank(),
            axis.title.x = element_blank()
        )  
    
    plot_data_3 <- dat_trd %>% 
        filter(date >= date_start_plot, date <= date_end_plot) %>% 
        select(date, bb_up_slope_d, bb_dn_slope_d) %>% 
        gather(type, bb_slope, -date) %>% U.debug("ZZZ")
    
    plot_3 <- ggplot(plot_data_3, aes(x = date, y = bb_slope, col = type)) + 
        geom_line() + 
        geom_vline(xintercept = date_trade, size = 0.25, col = "red", linetype = "dashed") +
        geom_hline(yintercept = 0, size = 0.25) +
        theme(
            legend.position = "bottom",
            axis.text=element_text(size=12), 
            axis.title.y = element_blank(),
            axis.title.x = element_blank()
        )  
    
    g1 <- ggplotGrob(plot_1)
    g2 <- ggplotGrob(plot_3)
    g <- rbind(g1, g2, size = "first")
    g$widths <- unit.pmax(g1$widths, g2$widths)
    grid.newpage()
    grid.draw(g)
}
T.optimumDaily <-
function (df_ohlc_daily) 
{
    ####################################################################################################
    ### Script sub functions
    ####################################################################################################
    
    buildPeriodicScheduleIncludingIntraPeriodDate <- function(date_to, df_periodic) {
        date_to <- as.Date(date_to);
        dat <- df_periodic %>%
            filter(date <= date_to);
        
        if (tail(dat$date, 1) < date_to) {
            dat_last <- tail(dat, 1);
            last_periodic_date <- dat_last$date;
            dat_last$date <- date_to;
            
            dat_daily <- df_daily %>%
                filter(
                    date <= date_to,
                    date > last_periodic_date
                )
            dat_last$open <- dat_daily$open[1];
            dat_last$high <- max(dat_daily$high);
            dat_last$low <- min(dat_daily$low);
            dat_last$close <- tail(dat_daily$close, 1);
            dat <- rbind(dat, dat_last);
        }
        dat;
    }
    
    addBBSlopes <- function(dat_df) {

        dat_df$bb_up_slope <- tanh(rollapply(
            data = select(dat_df, date, bb_up),
            width = 5,
            function(df) {
                df <- U.dataFrame(df);
                df$date <- as.Date(df$date);
                df$bb_up <- as.numeric(df$bb_up);
                if (any(is.na(df$bb_up))) {
                    NUM_NA
                } 
                else {
                    lm(bb_up ~ date, data = df)$coefficients[["date"]]
                }
                
            },
            align = "right",
            by.column = FALSE, 
            fill = NUM_NA
        ) / dat_df$vol)


        dat_df$bb_dn_slope <- tanh(rollapply(
            data = select(dat_df, date, bb_dn),
            width = 5,
            function(df) {
                df <- U.dataFrame(df);
                df$date <- as.Date(df$date);
                df$bb_dn <- as.numeric(df$bb_dn);
                if (any(is.na(df$bb_dn))) {
                    NUM_NA
                } 
                else {
                    lm(bb_dn ~ date, data = df)$coefficients[["date"]]    
                }
            },
            align = "right",
            by.column = FALSE, 
            fill = NUM_NA
        ) / dat_df$vol);
        dat_df %>% T.dfToXts;
    }
    
    
    addDailyTechnicals <- function(xts_period, period_type) {
        if (period_type == "D") {
            dat <- xts_period %>% 
                T.addTechnicals;
            dat$vol <- volatility(xts_period, n = 130, calc = "yang.zhang");
            dat %>%
                T.xtsToDf %>%
                addBBSlopes;
        } 
        else {
            df_period <- T.xtsToDf(xts_period);
            
            min_date <- df_period$date[1];
            dat <- df_daily %>%
                select(date) %>%
                left_join(df_period, by = "date");
            pos_first_non_na <- which(!is.na(dat$open))[1];
            
            dat <- dat[pos_first_non_na:nrow(dat),];
            dat$SMA7 <- NUM_NA;
            dat$SMA20 <- NUM_NA;
            dat$SMA50 <- NUM_NA;
            dat$bb_up <- NUM_NA;
            dat$bb_dn <- NUM_NA;
            dat$macdzl <- NUM_NA;
            
            for (i in 1:nrow(dat)) {
                
                dat_period_this_date <- dat$date[i] %>%
                    buildPeriodicScheduleIncludingIntraPeriodDate(df_period) %>%
                    T.dfToXts %>%
                    T.addTechnicals %>%
                    tail(1);
                for (j in 1:ncol(dat_period_this_date)) {
                    dat[i, j+1] <- dat_period_this_date[1,j];    
                }
            }
            dat;
        }
    }
    
    preparePeriod <- function(df_ohlc, period_type) {
        df_ohlc %>%
            T.dfToXts %>%
            T.dailyToPeriod(period_type) %>% 
            addDailyTechnicals(period_type);
    }
    
    formatDFBeforeCombining <- function(df, period_type) {
        df <- df %>% 
            select(-open, -high, -low, -close);
        cols_used <- 2:ncol(df);
        colnames(df)[cols_used] <- paste0(colnames(df)[cols_used], "_", period_type);
        df;
    }
    
    changeColNamesToLower <- function(df) {
        colnames(df) <- tolower(colnames(df));
        df;
    }
    
    
    
    
    combineTechnicals <- function(df_daily, df_weekly, df_monthly, dat_daily) {
        df_daily <- formatDFBeforeCombining(df_daily, "D");
        df_weekly <- formatDFBeforeCombining(df_weekly, "W");
        df_monthly <- formatDFBeforeCombining(df_monthly, "M");
        dat <- df_daily %>% 
            left_join(df_weekly, by = "date") %>%
            left_join(df_monthly, by = "date") %>%
            changeColNamesToLower %>%
            select(-starts_with("sma7")) %>%
            select(-starts_with("sma50")) %>% U.debug("Z1") %>%
            mutate(
                slope_speed_daily = vol_d / sqrt(260),
                slope_speed_weekly = vol_d / sqrt(365/7),
                slope_speed_monthly = vol_d / sqrt(12),
                signal_mm20_daily = ((sma20_d - lag(sma20_d, 1)) / (2 * slope_speed_daily)),
                signal_mm20_weekly = ((sma20_w - lag(sma20_w, 1)) / (2 * slope_speed_weekly)),
                signal_mm20_monthly = ((sma20_m - lag(sma20_m, 1)) / (2 * slope_speed_monthly))
            )  
        dat$signal_mm20 <- (sign(dat$signal_mm20_daily) == sign(dat$signal_mm20_weekly)) & 
            (sign(dat$signal_mm20_daily) == sign(dat$signal_mm20_monthly)) & 
            (abs(dat$signal_mm20_daily) >= 0.5) & 
            (abs(dat$signal_mm20_weekly) >= 0.5) & 
            (abs(dat$signal_mm20_monthly) >= 0.5);
        dat$signal_macd <- (sign(dat$macdzl_d) == sign(dat$macdzl_w)) &
            (sign(dat$macdzl_d) == sign(dat$macdzl_m))
        dat <- dat %>% 
            mutate(macd_d_crossing = sign(macdzl_d) * sign(lag(macdzl_d, 1)));
        
        dat$optimum <- dat$signal_mm20 & 
            dat$signal_macd &
            (sign(dat$macdzl_d) == sign(dat$signal_mm20_daily)) &
            (dat$macd_d_crossing == -1) & 
            (abs(dat$bb_up_slope_d) <= 1) &
            (abs(dat$bb_dn_slope_d) <= 1)
        dat$buy_sell <- 0;
        pos_optimum <- which(dat$optimum)
        dat$buy_sell[pos_optimum] <- sign(dat$signal_mm20_daily[pos_optimum]);
        
        dat_rtn <- dat_daily %>%
            select(date, close) %>% 
            mutate(
                rtn_5 = lead(close, 5) / close - 1,
                rtn_10 = lead(close, 10) / close - 1,
                rtn_20 = lead(close, 20) / close - 1,
                rtn_60 = lead(close, 60) / close - 1
            )
        
        
        
        dat <- dat %>% 
            left_join(dat_rtn, by = "date");
        dat$below_mm20 <- 0;
        dat 
        dat$below_mm20 <- (dat$buy_sell == -1) * (dat$close > dat$sma20_d) + 
            (dat$buy_sell == 1) * (dat$close < dat$sma20_d)
        dat$optimum <- dat$optimum * (dat$below_mm20 == 1);
        dat %>%
            mutate(
                pnl_5 = buy_sell * rtn_5,
                pnl_10 = buy_sell * rtn_10,
                pnl_20 = buy_sell * rtn_20,
                pnl_60 = buy_sell * rtn_60
            )
        
    }
    
    
    testMACDCross_Try <- function(df_ohlc) {
        
    }
    testMACDCross <- function(df_ohlc)
        U.try(testMACDCross_Try, df_ohlc)(df_ohlc)
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    dat_daily <- preparePeriod(df_ohlc_daily,"D") %>% U.debug("1")
    df_daily <- T.xtsToDf(dat_daily[,1:4]) %>% U.debug("2")
    dat_weekly <- preparePeriod(df_ohlc_daily, "W") %>% U.debug("3")
    dat_monthly <- preparePeriod(df_ohlc_daily, "M") %>% U.debug("4")
    dat_daily <- dat_daily %>% T.xtsToDf;
    dat <- combineTechnicals(dat_daily, dat_weekly, dat_monthly, df_daily) %>% U.debug("5")
    list(daily = dat_daily, weekly = dat_weekly, monthly = dat_monthly, combined = dat);
    
    
}
T.optimumDailyNew <-
function (fx_pair_list = NULL) 
{
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    if (is.null(fx_pair_list)) {
        fx_pair_list <- dat_histo_ohlc$pair %>% U.vectorizeUnique;
    }
    fx_pair_list <- U.vectorizeUnique(fx_pair_list)
    res_daily <- NULL;
    res_weekly <- NULL;
    res_monthly <- NULL;
    res_trades <- NULL;
    
    
    ####################################################################################################
    ### Script sub functions
    ####################################################################################################
    
    changeColNamesToLower <- function(df) {
        colnames(df) <- tolower(colnames(df));
        df;
    }
    
    buildPeriodicScheduleIncludingIntraPeriodDate <- function(date_to, df_periodic) {
        date_to <- as.Date(date_to);
        dat <- df_periodic %>%
            filter(date <= date_to);
        
        if (tail(dat$date, 1) < date_to) {
            dat_last <- tail(dat, 1);
            last_periodic_date <- dat_last$date;
            dat_last$date <- date_to;
            
            dat_daily <- dat_daily %>%
                filter(
                    date <= date_to,
                    date > last_periodic_date
                )
            dat_last$open <- dat_daily$open[1];
            dat_last$high <- max(dat_daily$high);
            dat_last$low <- min(dat_daily$low);
            dat_last$close <- tail(dat_daily$close, 1);
            dat <- rbind(dat, dat_last);
        }
        dat;
    }
    
    addBBSlopes <- function(dat_df) {
        
        dat_df$bb_up_slope <- tanh(rollapply(
            data = select(dat_df, date, bb_up),
            width = 5,
            function(df) {
                df <- U.dataFrame(df);
                df$date <- as.Date(df$date);
                df$bb_up <- as.numeric(df$bb_up);
                if (any(is.na(df$bb_up))) {
                    NUM_NA
                } 
                else {
                    lm(bb_up ~ date, data = df)$coefficients[["date"]]
                }
                
            },
            align = "right",
            by.column = FALSE, 
            fill = NUM_NA
        ) / (2*dat_df$vol))
        
        
        dat_df$bb_dn_slope <- tanh(rollapply(
            data = select(dat_df, date, bb_dn),
            width = 5,
            function(df) {
                df <- U.dataFrame(df);
                df$date <- as.Date(df$date);
                df$bb_dn <- as.numeric(df$bb_dn);
                if (any(is.na(df$bb_dn))) {
                    NUM_NA
                } 
                else {
                    lm(bb_dn ~ date, data = df)$coefficients[["date"]]    
                }
            },
            align = "right",
            by.column = FALSE, 
            fill = NUM_NA
        ) / (2*dat_df$vol));
        
        
        
        dat_df %>%
            mutate(
                bb_width = tanh((bb_up - bb_dn) / (2*close * vol)),
                bb_closed = ((bb_up_slope - bb_dn_slope) <= 0.5) * 
                    (bb_width <= 0.5) * 
                    (abs(bb_up_slope) <= 0.5) * 
                    (abs(bb_dn_slope) <= 0.5) 
            ) %>% 
            select(-bb_dn_slope, -bb_up_slope) %>%
            T.dfToXts
    }
    
    
    addDailyTechnicals <- function(xts_period, period_type) {
        dat <- xts_period;
        if (period_type == "D") {
            dat$vol <- volatility(dat, n = 130, calc = "yang.zhang");
            dat <- dat %>% 
                T.xtsToDf %>%
                addBBSlopes;
        }
        dat;
    }
    
    addExtraTechnicals <- function(dat, period_type) {
        dat <- dat %>%
            rename(
                mm20 = sma20
            ) %>%
            mutate(
                macd_cross = sign(sign(macdzl) - sign(lag(macdzl, 1))),
                slope_mm20 = log(mm20 / lag(mm20, 1)),
            );
        if (period_type == "D") {
            dat <- dat %>%
                mutate(
                    good_side_of_mm20 = (sign(close - mm20) == -sign(slope_mm20)) + 0
                )
        }
        dat;
    }
    
    removeUnusedTechnicals <- function(dat) {
        dat[,which(
            (colnames(dat) != "sma7") & 
                (colnames(dat) != "sma50")
        )];
    }
    
    addFXPair <- function(dat) {
        dat %>%
            mutate(pair = fx_pair) %>%
            select(pair, colnames(dat))
    }
    
    addScoringDaily <- function(dat, period_type) {
        if (period_type == "D") {
            dat <- dat %>%
                mutate(
                    score_1_mm20_macdzl_same_direction = (sign(slope_mm20) == sign(macdzl)),
                    score_2_bb_closed = bb_closed,
                    score_3_macd1d_cross = abs(macd_cross),
                    score_4_good_side_of_mm20 = good_side_of_mm20
                ) %>%
                select(-good_side_of_mm20)
        }
        dat;
    }
    
    preparePeriod <- function(df_ohlc, period_type) {
        df_ohlc %>%
            T.dfToXts %>% 
            T.dailyToPeriod(period_type) %>% 
            T.addTechnicals %>%
            addDailyTechnicals(period_type) %>% 
            T.xtsToDf %>%
            addExtraTechnicals(period_type) %>% 
            removeUnusedTechnicals %>% 
            addFXPair %>%
            addScoringDaily(period_type)
    }
    
    searchDailyForPotentialOptimum <- function(dat_daily) {
        dat_daily %>%
            mutate(
                score = score_1_mm20_macdzl_same_direction +
                    score_3_macd1d_cross 
                #     score_4_good_side_of_mm20
            ) %>%
            filter(
                score == 2
            )
    }
    
    addWeeklyAndMonthlyIndicatorsDate <- function(dat_potential) {
        dat <- dat_potential %>%
            rename(
                mm20_d = mm20,
                macdzl_d = macdzl,
                macd_cross_d = macd_cross,
                slope_mm20_d = slope_mm20
            ) 
        
        dat$mm20_w <- NUM_NA;
        dat$macdzl_w <- NUM_NA;
        dat$slope_mm20_w <- NUM_NA;
        
        dat$mm20_m <- NUM_NA;
        dat$macdzl_m <- NUM_NA;
        dat$slope_mm20_m <- NUM_NA;
        
        for (i in 1:nrow(dat)) {
            this_date <- dat$date[i];
            dat_local_weekly <- 
                dat_weekly %>% 
                buildPeriodicScheduleIncludingIntraPeriodDate(this_date, .);
            
            dat$mm20_w[i] <- tail(dat_local_weekly$mm20, 1);
            dat$macdzl_w[i] <- tail(dat_local_weekly$macdzl, 1);
            dat$slope_mm20_w[i] <- tail(dat_local_weekly$slope_mm20, 1);
            
            dat_local_monthly <- dat_monthly %>% 
                buildPeriodicScheduleIncludingIntraPeriodDate(this_date, .);
            dat$mm20_m[i] <- tail(dat_local_monthly$mm20, 1);
            dat$macdzl_m[i] <- tail(dat_local_monthly$macdzl, 1);
            dat$slope_mm20_m[i] <- tail(dat_local_monthly$slope_mm20, 1);
        }
        dat;
    }
    
    addScoringTotal <- function(dat_potential) {
        dat_potential %>%
            mutate(
                score_5_mm20_d_w_same_direction = (sign(slope_mm20_d) == sign(slope_mm20_w)),
                score_6_mm20_d_m_same_direction = (sign(slope_mm20_d) == sign(slope_mm20_m)),
                score_7_macd_d_w_same_direction = (sign(macdzl_d) == sign(macdzl_w)),
                score_8_macd_d_m_same_direction = (sign(macdzl_d) == sign(macdzl_m))
            ) %>%
            mutate(
                score_optimum = 
                    score_1_mm20_macdzl_same_direction + 
                    score_3_macd1d_cross + 
                    #    score_4_good_side_of_mm20 +
                    score_5_mm20_d_w_same_direction +
                    score_6_mm20_d_m_same_direction + 
                    score_7_macd_d_w_same_direction + 
                    score_8_macd_d_m_same_direction,
                score_optinano = 
                    score_1_mm20_macdzl_same_direction + 
                    score_3_macd1d_cross + 
                    #    score_4_good_side_of_mm20 +
                    score_5_mm20_d_w_same_direction +
                    score_7_macd_d_w_same_direction
            ) %>%
            mutate(
                buy_sell = sign(slope_mm20_d)
            )
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    
    for (fx_pair in fx_pair_list) {
        U.printTickerProgressVerbose(fx_pair, fx_pair_list);
        if ((fx_pair != "USDIDR") & (fx_pair != "USDTHB")) {
            df_ohlc_daily <- dat_histo_ohlc %>%
                filter(pair == fx_pair) %>%
                select(-pair);
            
            dat_daily <- preparePeriod(df_ohlc_daily,"D");
            dat_weekly <- preparePeriod(df_ohlc_daily, "W");
            dat_monthly <- preparePeriod(df_ohlc_daily, "M");
            
            dat_trades <- searchDailyForPotentialOptimum(dat_daily) %>%
                addWeeklyAndMonthlyIndicatorsDate %>%
                addScoringTotal
            
            res_daily <- rbind(res_daily, dat_daily);
            res_weekly <- rbind(res_weekly, dat_weekly);
            res_monthly <- rbind(res_monthly, dat_monthly);
            res_trades <- rbind(res_trades, dat_trades);
        }
   
    }
    
    list(
        daily = res_daily,
        weekly = res_weekly,
        monthly = res_monthly,
        trades = res_trades
    )
    
}
T.optimumFLS <-
function (fx_pair_list = NULL) 
{
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    if (is.null(fx_pair_list)) {
        fx_pair_list <- dat_histo_ohlc$pair %>% U.vectorizeUnique;
    }
    fx_pair_list <- U.vectorizeUnique(fx_pair_list)
    res_daily <- NULL;
    res_weekly <- NULL;
    res_monthly <- NULL;
    res_trades <- NULL;
    
    n_d <- 500
    n_w <- 250
    n_m <- 100
    
    ####################################################################################################
    ### Script sub functions
    ####################################################################################################
    
    changeColNamesToLower <- function(df) {
        colnames(df) <- tolower(colnames(df));
        df;
    }
    
    addTechFLS_dat <- function(dat, period_type) {
        
        
        if (period_type == "D") {
            df_fast <- 50;
            df_medium <- 20; 
            df_slow <- 10;
        }
        
        if (period_type == "W") {
            df_fast <- 40;
            df_medium <- 15; 
            df_slow <- 7;
        }
        
        if (period_type == "M") {
            df_fast <- 20;
            df_medium <- 10; 
            df_slow <- 5;
        }
        
        n_spline <- switch(period_type,
                           "D" = 500,
                           "W" =  250,
                           "M" = 100)
        
        dat_tail <- dat %>% 
            tail(n_spline);
        
        dat_tail$spline_slow <- dat_tail %>%
            select(date, close) %>%
            smooth.spline(df = df_slow) %>%
            predict %>%
            .$y;
        
        dat_tail$spline_medium <- dat_tail %>%
            select(date, close) %>%
            smooth.spline(df = df_medium) %>%
            predict %>%
            .$y;
        
        dat_tail$spline_fast <- dat_tail %>%
            select(date, close) %>%
            smooth.spline(df = df_fast) %>%
            predict %>%
            .$y;
        
        dat %>% 
            select(-spline_slow, -spline_medium, -spline_fast, -macdfls, -macdfls_cross, 
                   -slope_slow ,-slope_medium, -slope_fast,
                   -reverse_slope_slow, -reverse_slope_medium, -reverse_slope_fast) %>%
            left_join(select(dat_tail, date, starts_with("spline")), by = "date") %>% 
            mutate(
                macdfls = (spline_fast - spline_medium),
                macdfls_cross = sign(macdfls - lag(macdfls, 1)),
                slope_slow = spline_slow - lag(spline_slow, 1),
                slope_medium = spline_medium - lag(spline_medium, 1),
                slope_fast = spline_fast - lag(spline_fast, 1),
                reverse_slope_slow = sign(slope_slow) - sign(lag(slope_slow, 1)),
                reverse_slope_medium = sign(slope_medium) - sign(lag(slope_medium, 1)),
                reverse_slope_fast = sign(slope_fast) - sign(lag(slope_fast, 1)),
            );
        
    }
    
    addTechFLS <- function(dat, period_type) {
        dat$spline_slow <- NUM_NA;
        dat$spline_medium <- NUM_NA;
        dat$spline_fast <- NUM_NA;
        dat$macdfls <- NUM_NA;
        dat$macdfls_cross <- NUM_NA;
        dat$slope_slow <- NUM_NA;
        dat$slope_medium <- NUM_NA;
        dat$slope_fast <- NUM_NA;
        dat$reverse_slope_slow <- NUM_NA;
        dat$reverse_slope_medium <- NUM_NA;
        dat$reverse_slope_fast <- NUM_NA;
        
        n_spline <- switch(period_type,
                           "D" = 500,
                           "W" =  250,
                           "M" = 100)
        
        for (i in n_spline:nrow(dat)) {
            U.printTickerProgressVerbose(i, 1:nrow(dat))
            dat_i <- dat[1:i,] %>%
                addTechFLS_dat(period_type) ;
            dat$spline_slow[i] <- tail(dat_i$spline_slow, 1);
            dat$spline_medium[i] <- tail(dat_i$spline_medium, 1);
            dat$spline_fast[i] <- tail(dat_i$spline_fast, 1);
            dat$macdfls[i] <- tail(dat_i$macdfls, 1);
            dat$macdfls_cross[i] <- tail(dat_i$macdfls_cross, 1);
            dat$slope_slow[i] <- tail(dat_i$slope_slow, 1);
            dat$slope_medium[i] <- tail(dat_i$slope_medium, 1);
            dat$slope_fast[i] <- tail(dat_i$slope_fast, 1);
            dat$reverse_slope_slow[i] <- tail(dat_i$reverse_slope_slow, 1);
            dat$reverse_slope_medium[i] <- tail(dat_i$reverse_slope_medium, 1);
            dat$reverse_slope_fast[i] <- tail(dat_i$reverse_slope_fast, 1);
            
        }
        dat;
    }
    
    buildPeriodicScheduleIncludingIntraPeriodDate <- function(date_to, df_periodic) {
        date_to <- as.Date(date_to);
        dat <- df_periodic %>%
            filter(date <= date_to);
        
        if (tail(dat$date, 1) < date_to) {
            dat_last <- tail(dat, 1);
            last_periodic_date <- dat_last$date;
            dat_last$date <- date_to;
            
            dat_daily <- dat_daily %>%
                filter(
                    date <= date_to,
                    date > last_periodic_date
                )
            dat_last$open <- dat_daily$open[1];
            dat_last$high <- max(dat_daily$high);
            dat_last$low <- min(dat_daily$low);
            dat_last$close <- tail(dat_daily$close, 1);
            dat <- rbind(dat, dat_last);
        }
        dat;
    }
    
    addBBSlopes <- function(dat_df) {
        
        dat_df$bb_up_slope <- tanh(rollapply(
            data = select(dat_df, date, bb_up),
            width = 5,
            function(df) {
                df <- U.dataFrame(df);
                df$date <- as.Date(df$date);
                df$bb_up <- as.numeric(df$bb_up);
                if (any(is.na(df$bb_up))) {
                    NUM_NA
                } 
                else {
                    lm(bb_up ~ date, data = df)$coefficients[["date"]]
                }
                
            },
            align = "right",
            by.column = FALSE, 
            fill = NUM_NA
        ) / (2*dat_df$vol))
        
        
        dat_df$bb_dn_slope <- tanh(rollapply(
            data = select(dat_df, date, bb_dn),
            width = 5,
            function(df) {
                df <- U.dataFrame(df);
                df$date <- as.Date(df$date);
                df$bb_dn <- as.numeric(df$bb_dn);
                if (any(is.na(df$bb_dn))) {
                    NUM_NA
                } 
                else {
                    lm(bb_dn ~ date, data = df)$coefficients[["date"]]    
                }
            },
            align = "right",
            by.column = FALSE, 
            fill = NUM_NA
        ) / (2*dat_df$vol));
        
        
        
        dat_df %>%
            mutate(
                bb_width = tanh((bb_up - bb_dn) / (2*close * vol)),
                bb_closed = ((bb_up_slope - bb_dn_slope) <= 0.5) * 
                    (bb_width <= 0.5) * 
                    (abs(bb_up_slope) <= 0.5) * 
                    (abs(bb_dn_slope) <= 0.5) 
            ) %>% 
            select(-bb_dn_slope, -bb_up_slope) %>%
            T.dfToXts
    }
    
    
    addDailyTechnicals <- function(xts_period, period_type) {
        dat <- xts_period;
        if (period_type == "D") {
            dat$vol <- volatility(dat, n = 130, calc = "yang.zhang");
            dat <- dat %>% 
                T.xtsToDf %>%
                addBBSlopes;
        }
        dat;
    }
    
    addExtraTechnicals <- function(dat, period_type) {
        dat <- dat %>%
            rename(
                mm20 = sma20
            ) %>%
            mutate(
                macd_cross = sign(sign(macdzl) - sign(lag(macdzl, 1))),
                slope_mm20 = log(mm20 / lag(mm20, 1)),
            );
        if (period_type == "D") {
            dat <- dat %>%
                mutate(
                    good_side_of_mm20 = (sign(close - mm20) == -sign(slope_mm20)) + 0
                )
        }
        dat;
    }
    
    removeUnusedTechnicals <- function(dat) {
        dat[,which(
            (colnames(dat) != "sma7") & 
                (colnames(dat) != "sma20") & 
                (colnames(dat) != "sma50") &
                (colnames(dat) != "macdzl")
        )];
    }
    
    addFXPair <- function(dat) {
        dat %>%
            mutate(pair = fx_pair) %>%
            select(pair, colnames(dat))
    }
    
    addScoringDaily <- function(dat, period_type) {
        if (period_type == "D") {
            dat <- dat %>%
                mutate(
                    score_1_mm20_macdzl_same_direction = (sign(slope_mm20) == sign(macdzl)),
                    score_2_bb_closed = bb_closed,
                    score_3_macd1d_cross = abs(macd_cross),
                    score_4_good_side_of_mm20 = good_side_of_mm20
                ) %>%
                select(-good_side_of_mm20)
        }
        dat;
    }
    
    preparePeriod <- function(df_ohlc, period_type) {
        df_ohlc %>% U.debug("A1") %>%
            T.dfToXts %>% U.debug("A2") %>%
            T.dailyToPeriod(period_type) %>% U.debug("A3") %>% 
            T.addTechnicals %>%U.debug("A4") %>%
            addDailyTechnicals(period_type) %>% U.debug("A5") %>%
            T.xtsToDf %>% U.debug("A6") %>%
            addTechFLS(period_type) %>% U.debug("A7") %>%
            removeUnusedTechnicals %>% U.debug("A7") %>%
            addFXPair #%>%
        #addScoringDaily(period_type)
    }
    
    searchDailyForPotentialOptimum <- function(dat_daily) {
        dat_daily %>%
            mutate(
                score = score_1_mm20_macdzl_same_direction +
                    score_3_macd1d_cross 
                #     score_4_good_side_of_mm20
            ) %>%
            filter(
                score == 2
            )
    }
    
    addWeeklyAndMonthlyIndicatorsDate <- function(dat_potential) {
        dat <- dat_potential %>%
            rename(
                mm20_d = mm20,
                macdzl_d = macdzl,
                macd_cross_d = macd_cross,
                slope_mm20_d = slope_mm20
            ) 
        
        dat$mm20_w <- NUM_NA;
        dat$macdzl_w <- NUM_NA;
        dat$slope_mm20_w <- NUM_NA;
        
        dat$mm20_m <- NUM_NA;
        dat$macdzl_m <- NUM_NA;
        dat$slope_mm20_m <- NUM_NA;
        
        for (i in 1:nrow(dat)) {
            this_date <- dat$date[i];
            dat_local_weekly <- 
                dat_weekly %>% 
                buildPeriodicScheduleIncludingIntraPeriodDate(this_date, .);
            
            dat$mm20_w[i] <- tail(dat_local_weekly$mm20, 1);
            dat$macdzl_w[i] <- tail(dat_local_weekly$macdzl, 1);
            dat$slope_mm20_w[i] <- tail(dat_local_weekly$slope_mm20, 1);
            
            dat_local_monthly <- dat_monthly %>% 
                buildPeriodicScheduleIncludingIntraPeriodDate(this_date, .);
            dat$mm20_m[i] <- tail(dat_local_monthly$mm20, 1);
            dat$macdzl_m[i] <- tail(dat_local_monthly$macdzl, 1);
            dat$slope_mm20_m[i] <- tail(dat_local_monthly$slope_mm20, 1);
        }
        dat;
    }
    
    addScoringTotal <- function(dat_potential) {
        dat_potential %>%
            mutate(
                score_5_mm20_d_w_same_direction = (sign(slope_mm20_d) == sign(slope_mm20_w)),
                score_6_mm20_d_m_same_direction = (sign(slope_mm20_d) == sign(slope_mm20_m)),
                score_7_macd_d_w_same_direction = (sign(macdzl_d) == sign(macdzl_w)),
                score_8_macd_d_m_same_direction = (sign(macdzl_d) == sign(macdzl_m))
            ) %>%
            mutate(
                score_optimum = 
                    score_1_mm20_macdzl_same_direction + 
                    score_3_macd1d_cross + 
                    #    score_4_good_side_of_mm20 +
                    score_5_mm20_d_w_same_direction +
                    score_6_mm20_d_m_same_direction + 
                    score_7_macd_d_w_same_direction + 
                    score_8_macd_d_m_same_direction,
                score_optinano = 
                    score_1_mm20_macdzl_same_direction + 
                    score_3_macd1d_cross + 
                    #    score_4_good_side_of_mm20 +
                    score_5_mm20_d_w_same_direction +
                    score_7_macd_d_w_same_direction
            ) %>%
            mutate(
                buy_sell = sign(slope_mm20_d)
            )
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    
    for (fx_pair in fx_pair_list) {
        U.printTickerProgressVerbose(fx_pair, fx_pair_list);
        if ((fx_pair != "USDIDR") & (fx_pair != "USDTHB")) {
            print(1)
            df_ohlc_daily <- dat_histo_ohlc %>%
                filter(pair == fx_pair) %>%
                select(-pair);
            print(2)
            dat_daily <- preparePeriod(df_ohlc_daily,"D");
            print(3)
            dat_weekly <- preparePeriod(df_ohlc_daily, "W");
            print(4)
            dat_monthly <- preparePeriod(df_ohlc_daily, "M");
            print(5)
            
            #    dat_trades <- searchDailyForPotentialOptimum(dat_daily) %>%
            #        addWeeklyAndMonthlyIndicatorsDate %>%
            #        addScoringTotal
            
            res_daily <- rbind(res_daily, dat_daily);
            res_weekly <- rbind(res_weekly, dat_weekly);
            res_monthly <- rbind(res_monthly, dat_monthly);
            #    res_trades <- rbind(res_trades, dat_trades);
        }
        
    }
    
    list(
        daily = res_daily,
        weekly = res_weekly,
        monthly = res_monthly
        #trades = res_trades
    )
    
}
T.optiNanoDaily <-
function (dat_ohlc) 
{
    ####################################################################################################
    ### Script sub functions
    ####################################################################################################
    prepareDaily <- function(dat_ohlc) 
        U.tryNull(T.addTechnicals, dat_ohlc)

    prepareWeeklyOrMonthly_Try <- function(dat_ohlc, week_or_month) {
        dat_ohlc_period <- switch(
            tolower(U.left(week_or_month, 1)),
            "w" = to.weekly(dat_ohlc),
            "m" = to.monthly(dat_ohlc)
            );
        colnames(dat_ohlc_period) <- c("Open", "High", "Low", "Close");
        T.addTechnicals(dat_ohlc_period);
    }
    prepareWeeklyOrMonthly <- function(dat_ohlc, week_or_month) 
        U.try(prepareWeeklyOrMonthly_Try)(dat_ohlc, week_or_month)
    
    testMACDCross_Try <- function(dat_ohlc) {
        
    }
    testMACDCross <- function(dat_ohlc)
        U.try(testMACDCross_Try, dat_ohlc)(dat_ohlc)
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    dat_daily <- dat_ohlc;
    dat_weekly <- prepareWeeklyOrMonthly(dat_ohlc, "w");
    dat_monthly <- prepareWeeklyOrMonthly(dat_ohlc, "m");
    
    
}
T.plotBacktesting <-
function () 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    load("~/Mount/GDrv/Model/FXO/Code/20191130_AllFeatures.RData")
  #  load("~/Mount/GDrv/Model/FXO/Code/20191130_LessFeatures.RData")
  #  load("~/Mount/GDrv/Model/FXO/Code/20191130_LesserFeatures.RData")
    load("~/Mount/GDrv/Model/FXO/Code/20191130_NewFeatures.RData")
  #  load("/home/fls/Mount/GDrv/Model/FXO/Code/res_2019-12-03_09h59m12s.RData")
    load("/home/fls/Mount/GDrv/Model/FXO/Code/res_old_20191212.RData")
    load("/home/fls/Mount/GDrv/Model/FXO/Code/res_new_20191212.RData")
    
    start_date <- as.Date(paste0(year(min(res_new$date)), "-01-01"));
    end_date <- as.Date(paste0(year(max(c(res_new$date, res_old$date))), "-12-31"));
    
    date_max_plot <- as.Date(paste0(year(max(res_new$date)), "-12-31"));

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    resultsStats <- function(dat) {
        dat %>% 
            filter(predict != "flat") %>%
            mutate(
                month = as.character(date - day(date) + 1),
                win = (pnl >= 0) + 0,
                pnl_positive = pnl * win,
                pnl_negative = pnl * (1-win),
                is_fx = 0 + (asset_class == "fx"),
                is_index = 0 + (asset_class == "index"),
                is_metal = 0 + (asset_class == "metal"),
                is_long = 0 + (predict == "up")
            ) %>% 
            group_by(year) %>%
            summarize(
                N = n(),
                pnl_per_trade = round(100*mean(pnl), 2),
                stdev = round(100*sd(pnl), 2),
                duration_days = round(mean(duration), 2),
                win_ratio_N = round(100 * sum(win) / N, 2),
                win_ratio_pnl = round(-mean(pnl_positive) / mean(pnl_negative), 2),
                nb_fx = round(100*sum(is_fx) / N,2),
                nb_index = round(100*sum(is_index) / N,2),
                nb_metal = round(100*sum(is_metal) / N,2),
                long_short = round(100*sum(is_long) / N,2),
                pnl_annualized = pnl_per_trade * 260 / duration_days
            ) %>% 
            ungroup;
    }
    
    resultsSummary <- function(dat) {
        dat_1 <- dat %>% 
            mutate(year = year(date)) %>%
            resultsStats;
        
        dat_2 <- dat %>% 
            mutate(year = "Total") %>%
            resultsStats;
        
        rbind(dat_1, dat_2)
    }
    
    plotStats <- function(dat) {
        dat %>%
            group_by(date) %>%
            summarize(pnl = 100000 * mean(pnl)) %>%
            ungroup %>% 
            mutate(pnl = cumsum(pnl)) %>%
            left_join(data.frame(date = seq(start_date, end_date, 1)) %>% U.dataFrame, ., by = "date") %>%
            na.locf %>%
            left_join(data.frame(date = seq(start_date, end_date, 1)) %>% U.dataFrame, ., by = "date") %>%
            U.dfReplaceNAColumnsWithZero("pnl");
    }
    
    plotSummary <- function() {
        dat_all <- plotStats(res_all_features) %>% 
            mutate(Strategy = "All")
    #    dat_less <- plotStats(res_less_features) %>% 
    #        mutate(Strategy = "Less");
    #    dat_lesser <- plotStats(res_lesser_features) %>% 
    #        mutate(Strategy = "Lesser");
        dat_best <- plotStats(res_new_features) %>% 
            mutate(Strategy = "Best_Now");
    #    dat_auto <- plotStats(res) %>% 
    #        mutate(Strategy = "Automatic");
        dat_new <- plotStats(res_new) %>% 
            mutate(Strategy = "New")
        dat_old <- plotStats(res_old) %>% 
            mutate(Strategy = "Old")
        
        dat_all %>%
        #    rbind(dat_less) %>%
            rbind(dat_best) %>%
            rbind(dat_new) %>%
        #    rbind(dat_auto) %>%
            rbind(dat_old) %>%
            filter(date <= date_max_plot) %>%
            mutate(Strategy = as.factor(Strategy)) %>%
            ggplot(aes(x = date, y = pnl, col = Strategy)) + 
            geom_line() +
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                legend.position = c(0.05, 0.98),
                legend.justification = c("left", "top")
            ) +
            scale_y_continuous(labels = comma) +
            scale_x_date(date_breaks = "years", date_labels = "%y")
    }
    
    pnlStats <- function(dat) {
        dat %>%
            group_by(date) %>%
            summarize(pnl = 100000 * mean(pnl)) %>%
            ungroup %>% 
            mutate(year = year(date)) %>% 
            group_by(year) %>% 
            summarize(pnl = sum(pnl)) %>%
            ungroup;
        
    }
    
    pnlSummary <- function() {
        dat_all <- pnlStats(res_all_features) %>% 
            mutate(Strategy = "All");
    #    dat_less <- pnlStats(res_less_features) %>% 
    #        mutate(Strategy = "Less");
    #    dat_lesser <- pnlStats(res_lesser_features) %>% 
    #        mutate(Strategy = "Lesser");
        dat_best <- pnlStats(res_new_features) %>% 
            mutate(Strategy = "Best_Now");
    #    dat_auto <- pnlStats(res) %>% 
    #        mutate(Strategy = "Automatic");
        dat_new <- pnlStats(res_new) %>% 
            mutate(Strategy = "New");
        dat_old <- pnlStats(res_old) %>% 
            mutate(Strategy = "Old");

        dat_year <- dat_all %>%
    #        rbind(dat_less) %>%
            rbind(dat_best) %>%
            rbind(dat_new) %>%
    #        rbind(dat_auto) %>%
            rbind(dat_old) %>%
            group_by(year, Strategy) %>%
            summarize(pnl = sum(pnl)) %>%
            ungroup %>% 
            spread(Strategy, pnl);
  
        dat_total <- dat_all %>%
    #        rbind(dat_less) %>%
            rbind(dat_best) %>%
            rbind(dat_new) %>%
    #        rbind(dat_auto) %>%
            rbind(dat_old) %>%
            mutate(year = "Total") %>%
            group_by(year, Strategy) %>%
            summarize(pnl = sum(pnl)) %>%
            ungroup %>% 
            spread(Strategy, pnl);

        rbind(dat_year, dat_total) %>% 
            tail(20) %>% 
            mutate(Difference = New - Old)
        
    }
    
    assetSummary <- function(dat) {
        dat_year <- dat %>% 
            mutate(year = paste0("Y", year(date))) %>%
            group_by(pair, year) %>%
            summarize(N = n()) %>% 
            ungroup %>% 
            filter(N > 0) %>%
            spread(year, N, fill = 0) %>% 
            data.frame;
        
        dat_total <- dat %>% 
            mutate(
                year = paste0("Y", year(date)),
                long = 0 + (predict == "up")
                ) %>%
            group_by(pair) %>%
            summarize(
                Total = n(), 
                long_short = round(100 * sum(long) / Total, 1),
                avg_pnl_trd = round(100 * mean(pnl), 2)
                )%>% 
            ungroup %>% 
            filter(Total > 0) %>%
            data.frame;
        left_join(dat_year, dat_total, by = "pair") %>%
            arrange(-Total) %>% 
            head(25)
        

    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    U.printBanner("All Features")
    print(resultsSummary(res_all_features))
    U.printBanner("Less Features")
  #  print(resultsSummary(res_less_features))
  #  U.printBanner("Lesser Features")
  #  print(resultsSummary(res_lesser_features))
    U.printBanner("New Method")
    print(resultsSummary(res_new))
    U.printBanner("Old Method")
    print(resultsSummary(res_old))
    print(plotSummary())
    U.printBanner("Annual PnL")
    print(pnlSummary())
    U.printBanner("Trades by asset - new method")
    print(assetSummary(res_new))
    
}
T.plotPriceSeries <-
function (instrument, year_start = 1800, year_end = 2100)
{
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    instrument <- A.getInstrumentId(instrument)
    fx_pair <- filter(INSTRUMENTS, instrument_id == instrument)$pair
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    year_start <- min(c(year_start, year(TO_DAY) - 1))
    year_end <- min(c(year_end, year(TO_DAY)))
    
    date_from <- as.Date(paste0(year_start, "-01-01"))
    date_to <- min(c(as.Date(paste0(year_end, "-12-31")), TO_DAY))
    
    dat <- T.getHistoPx(instrument, date_from, date_to)
    
    year_start <- min(dat$year)
    year_end <- max(dat$year)
    duration_series <- (year_end - year_start)
    
    max_hole <- max(diff(dat$date))
    
    plot_breaks <- 1
    if (duration_series > 50) plot_breaks <- 2
    
    plot_breaks <- paste0(plot_breaks, " years")
    
    print("%s - max_date: %s - max_hole: %s" %>% sprintf(fx_pair, max(dat$date), max_hole))
    
    plot_1 <- dat %>% 
        ggplot(aes(x = date, y = close)) + 
        geom_line(color = "lightseagreen") + 
        geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
        scale_x_date(date_breaks = plot_breaks, date_labels = "%y", limits = c(date_from, date_to)) + 
        theme(
            legend.position = "bottom",
            axis.text=element_text(size = 10), 
            axis.title.y = element_blank(),
            axis.title.x = element_blank()
        ) +
        ggtitle(fx_pair)
    print(plot_1)
    plot_1
}
T.predictRFToday <-
function (model_rf, specific_pair = NULL) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    prepareLivePrice <- function() {
        U.printBanner("Getting live prices")
        dat <- F.downloadHistoFXOHLC("live");
        U.printBanner("Preparing technicals for each pair")
        dat;
    }

    prepareTechnicals_Try <- function(fx_pair) {
        U.printBanner(fx_pair, FALSE)
        dat_histo_live %>% 
            filter(pair == fx_pair) %>%
            tail(800) %>%
            T.addTechnicalsFLS(TO_DAY, "D", TRUE) %>%
            filter(date >= YESTERDAY) %>%
            tail(1)
    }
    prepareTechnicals <- function(fx_pair)
        U.try(prepareTechnicals_Try)(fx_pair)
    
    prepareTrade <- function(dat) {
        U.printBanner("Predicting trades")
        
        dat$asset_class <- factor(
            U.vectorize(dat$asset_class), 
            levels = c("fx_dm", "fx_em", "index", "metal", "yield", "bond")
        );
        dat <- T.randomForestCriteria(dat)

        dat_predict <- dat %>%
            filter(score == 4);
        
        dat_no_predict <- anti_join(dat, dat_predict, by = "pair") %>%
            mutate(predict = "score_off")
        
        if (nrow(dat_predict) > 0) {
            dat_predict$predict <- predict(model_rf, newdata = dat_predict, type = "response");
        }
        
        dat <- dat %>%
            select(pair) %>%
            left_join(rbind(dat_predict, dat_no_predict), by = "pair");

        U.printBanner("All Done");
        dat;
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    dat_histo_live <- prepareLivePrice();
    ccy_pair_list <- U.vectorizeUnique(dat_histo_live$pair);
    ccy_pair_list <- ccy_pair_list[which(gsub("0", "", ccy_pair_list) == ccy_pair_list)];
    
    if (!is.null(specific_pair)) {
        ccy_pair_list <- specific_pair;    
    }
    
    ccy_pair_list %>%
        lapply(prepareTechnicals) %>% 
        bind_rows #%>%
        #prepareTrade;

}
T.prepareLivePrices <-
function () 
{
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
     

    ####################################################################################################
    ### Sub Routines
    ####################################################################################################
    
    dat_actual_live <- "SELECT instrument_id, date, open, high, low, close FROM live_px WHERE date = '%s'" %>%
        sprintf(TO_DAY) %>%
        D.select

    dat_recent_histo <- 
        "SELECT I.instrument_id, F.feature, T.value
        FROM 
        (
            SELECT X.instrument_id 
            FROM static_instrument X
            LEFT JOIN live_px Y ON Y.instrument_id = X.instrument_id
            WHERE X.use_for_trading = 1
            AND Y.date != '%s'
        ) I
        LEFT JOIN live_technicals_dbl T ON T.instrument_id = I.instrument_id
        LEFT JOIN static_feature F ON F.feature_id = T.feature_id
        WHERE F.feature IN ('open', 'high', 'low', 'close')" %>%
        sprintf(TO_DAY) %>%
        D.select %>%
        spread(feature, value) %>%
        mutate(value = TO_DAY) %>%
        select(colnames(dat_live))
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    T.getHistoPx()
    rbind(dat_actual_live, dat_recent_histo) %>%
        arrange(instrument_id)
        
    
}
T.quickPredict <-
function () 
{
    ####################################################################################################
    ### Script
    ####################################################################################################
  #  load("/home/fls/Mount/Glenorchy/FX/Spot/Daily/Histo_OHLC.RData");
    dat_histo_ohlc <<- dat_histo_ohlc;
    
    calibration <- T.randomForest();
    dat <- T.predictRFToday(calibration$model);
    dat_1 <- dat %>%
        filter(
            abs(psi_20) > 0.4 |
                trend_21_r2 >= 0.75 | 
                vol_st_decile >= 0.6
        );
    if (U.dfContainsData(dat_1)) {
        dat_1 <- rbind(
            calibration$set %>% sample_n(5), 
            dat_1
        );
        dat_1$asset_class <- as.factor(U.vectorize(dat_1$asset_class));
        prediction <- try(predict(calibration$model, newdata = dat_1, type = "response"), TRUE);
        if (class(prediction) == "try-error") {
            prediction <- NA;
        }
        dat_1$predict <- prediction;
    }
    else {
        dat_1 <- dat %>%
            head(5) %>%
            mutate(predict = NA)
    }

    res <- dat %>% 
        T.randomForestCriteria %>%
        arrange(-score) %>%
        rename(spot = close) %>%
        left_join(select(dat_1, pair, date, predict), by = c("pair", "date")) %>%
        mutate(
            threshold = spot * exp(sign(psi_20) * (0.55 - abs(psi_20)) * vol_lt),
            tgt_width_pct = 100 * (t_up / spot - 1)
            ) %>%
        select(
            pair, date, spot, 
            psi_20, trend_21_r2, vol_st_decile, 
            s1, s2, s3, score, 
            predict, threshold, tgt_width_pct
            ) %>%
        U.data2Tibble;

    print(res %>%
              filter(score >= 2.60) %>%
              mutate(
                  s1 = round(s1, 2),
                  s2 = round(s2, 2),
                  s3 = round(s3, 2),
                  score = round(score, 3),
                  vol_st_decile = round(100*vol_st_decile, 1)
              ) %>% 
              arrange(predict, -score),
              n = 60
              );
    list(
        model = calibration,
        dat = dat,
        res = res
    )
}
T.reinsertTechnicalFileInDB <-
function () 
{
    ####################################################################################################
    ### Script Parameters
    ####################################################################################################
    load(paste0(DIRECTORY_DATA_SD, "Technicals/Technicals.RData"))
}
T.rfStrategyBacktesting <-
function () 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    load("~/Mount/GDrv/Model/FXO/Code/20191130_AllFeatures.RData")
    load("~/Mount/GDrv/Model/FXO/Code/20191130_LessFeatures.RData")
    load("~/Mount/GDrv/Model/FXO/Code/20191130_LesserFeatures.RData")
    
    start_date <- as.Date(paste0(year(min(res_all_features$date)), "-01-01"));
    end_date <- as.Date(paste0(year(max(c(res_all_features$date, res_all_features$date))), "-12-31"));
    
    nav_0 <- 100;
    
    dat_trd <- res_all_features;

    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    resultsStats <- function(dat) {
        dat %>% 
            filter(predict != "flat") %>%
            mutate(
                month = as.character(date - day(date) + 1),
                win = (pnl >= 0) + 0,
                pnl_positive = pnl * win,
                pnl_negative = pnl * (1-win),
                is_fx = 0 + (asset_class == "fx"),
                is_index = 0 + (asset_class == "index"),
                is_metal = 0 + (asset_class == "metal"),
                is_long = 0 + (predict == "up")
            ) %>% 
            group_by(year) %>%
            summarize(
                N = n(),
                pnl_per_trade = round(100*mean(pnl), 2),
                stdev = round(100*sd(pnl), 2),
                duration_days = round(mean(duration), 2),
                win_ratio_N = round(100 * sum(win) / N, 2),
                win_ratio_pnl = round(-mean(pnl_positive) / mean(pnl_negative), 2),
                nb_fx = round(100*sum(is_fx) / N,2),
                nb_index = round(100*sum(is_index) / N,2),
                nb_metal = round(100*sum(is_metal) / N,2),
                long_short = round(100*sum(is_long) / N,2),
                pnl_annualized = pnl_per_trade * 260 / duration_days
            ) %>% 
            ungroup;
    }
    
    resultsSummary <- function(dat) {
        dat_1 <- dat %>% 
            mutate(year = year(date)) %>%
            resultsStats;
        
        dat_2 <- dat %>% 
            mutate(year = "Total") %>%
            resultsStats;
        
        rbind(dat_1, dat_2)
    }
    
    plotStats <- function(dat) {
        dat %>%
            group_by(date) %>%
            summarize(pnl = 100000 * mean(pnl)) %>%
            ungroup %>% 
            mutate(pnl = cumsum(pnl)) %>%
            left_join(data.frame(date = seq(start_date, end_date, 1)) %>% U.dataFrame, ., by = "date") %>%
            na.locf %>%
            left_join(data.frame(date = seq(start_date, end_date, 1)) %>% U.dataFrame, ., by = "date") %>%
            U.dfReplaceNAColumnsWithZero("pnl");
    }
    
    plotSummary <- function() {
        dat_all <- plotStats(res_all_features) %>% 
            mutate(Strategy = "All")
        dat_less <- plotStats(res_less_features) %>% 
            mutate(Strategy = "Less");
        dat_lesser <- plotStats(res_lesser_features) %>% 
            mutate(Strategy = "Lesser");
        
        dat_all %>%
            rbind(dat_less) %>%
            rbind(dat_lesser) %>%
            mutate(Strategy = as.factor(Strategy)) %>%
            ggplot(aes(x = date, y = pnl, col = Strategy)) + 
            geom_line() +
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                legend.position = "bottom"
            ) +
            scale_y_continuous(labels = comma) +
            scale_x_date(date_breaks = "years", date_labels = "%y")
    }
    
    pnlStats <- function(dat) {
        dat %>%
            group_by(date) %>%
            summarize(pnl = 100000 * mean(pnl)) %>%
            ungroup %>% 
            mutate(year = year(date)) %>% 
            group_by(year) %>% 
            summarize(pnl = sum(pnl)) %>%
            ungroup;
        
    }
    
    pnlSummary <- function() {
        dat_all <- pnlStats(res_all_features) %>% 
            mutate(Strategy = "All")
        dat_less <- pnlStats(res_less_features) %>% 
            mutate(Strategy = "Less");
        dat_lesser <- pnlStats(res_lesser_features) %>% 
            mutate(Strategy = "Lesser");
        dat_year <- dat_all %>%
            rbind(dat_less) %>%
            rbind(dat_lesser) %>%
            group_by(year, Strategy) %>%
            summarize(pnl = sum(pnl)) %>%
            ungroup %>% 
            spread(Strategy, pnl);
        
        dat_total <- dat_all %>%
            rbind(dat_less) %>%
            rbind(dat_lesser) %>%
            mutate(year = "Total") %>%
            group_by(year, Strategy) %>%
            summarize(pnl = sum(pnl)) %>%
            ungroup %>% 
            spread(Strategy, pnl);
        
        rbind(dat_year, dat_total) %>% 
            tail(20) %>% 
            mutate(Difference = Lesser - All)
        
    }
    
    assetSummary <- function(dat) {
        dat_year <- dat %>% 
            mutate(year = paste0("Y", year(date))) %>%
            group_by(pair, year) %>%
            summarize(N = n()) %>% 
            ungroup %>% 
            filter(N > 0) %>%
            spread(year, N, fill = 0) %>% 
            data.frame;
        
        dat_total <- dat %>% 
            mutate(
                year = paste0("Y", year(date)),
                long = 0 + (predict == "up")
                ) %>%
            group_by(pair) %>%
            summarize(
                Total = n(), 
                long_short = round(100 * sum(long) / Total, 1),
                avg_pnl_trd = round(100 * mean(pnl), 2)
                )%>% 
            ungroup %>% 
            filter(Total > 0) %>%
            data.frame;
        left_join(dat_year, dat_total, by = "pair") %>%
            arrange(-Total) %>% 
            head(25)
        

    }
    
    
    pnlInit <- function() {
        dat <- data.frame(
            date = seq(start_date-1, end_date, 1),
            nav_morning = 0,
            nav_evening = 0,
            pnl_day = 0,
            pnl_new = 0,
            pnl_exit = 0,
            pnl_live = 0,
            n_trades_new = 0,
            n_trades_live = 0,
            n_trades_exit = 0
        ) %>%
            U.dataFrame;
        dat$nav_evening[1] <- nav_0;
        dat;
    }
    
    getExitDate <- function(this_pair, this_date, this_duration) {
        dat_histo_ohlc %>%
            filter(
                pair == this_pair,
                date >= this_date
            ) %>%
            head(this_duration+1) %>%
            .$date %>%
            last %>% 
            as.character;
    }
    
    prepareNewTrades_Try <- function(this_date, nav_morning) {
        dat <- dat_trd %>% 
            select(date, pair, buy_sell, close, duration, px_exit) %>%
            filter(date == this_date) %>%
            U.noData2Null;
        dat$date_exit <- mapply(getExitDate, dat$pair, dat$date, dat$duration) %>%
            as.Date;
        n_trades <- nrow(dat);
        dat %>%
            rename(
                date_entry = date,
                px_entry = close
            ) %>%
            mutate(
                notional = nav_morning / n_trades
            ) %>%
            select(pair, buy_sell, notional, date_entry, px_entry, date_exit, px_exit);
    }
    prepareNewTrades <- function(this_date, nav_morning) 
        U.try(prepareNewTrades_Try, NULL)(this_date, nav_morning)
    
    getClosingPriceDay <- function(this_date, this_pair) {
        dat_histo_ohlc %>%
            filter(
                pair == this_pair,
                date <= this_date
            ) %>% 
            tail(1) %>%
            .$close;
    }
    
    calcPnlLiveTrades_Try <- function(date_i, date_i_1, trd_live) {
        dat <- trd_live %>%
            mutate(
                date_t = date_i, 
                date_t_1 = date_i_1
            ) %>%
            U.noData2Null;
        
        dat$close_t_1 <- mapply(getClosingPriceDay, dat$date_t_1, dat$pair);
        dat$close_t <- mapply(getClosingPriceDay, dat$date_t, dat$pair);
        
        dat <- dat %>%
            mutate(
                pnl = (close_t / close_t_1 - 1) * buy_sell * notional
            );
        
        sum(dat$pnl);
    }
    calcPnlLiveTrades <- function(date_i, date_i_1, trd_live)
        U.try(calcPnlLiveTrades_Try, 0)(date_i, date_i_1, trd_live)
    
    calcPnlExitTrades_Try <- function(date_i, trd_exit) {
        dat <- trd_exit %>%
            mutate(
                date_t = date_i
            );
        dat$close_t <- mapply(getClosingPriceDay, dat$date_t, dat$pair);
        dat <- dat %>%
            mutate(
                pnl = (px_exit / close_t - 1) * buy_sell * notional
            );
        sum(dat$pnl);
    }
    calcPnlExitTrades <- function(date_i, trd_exit)
        U.try(calcPnlExitTrades_Try, 0)(date_i, trd_exit)
    
    calcPnL <- function(dat_pnl) {
        trd_new <- NULL;
        trd_live <- NULL;
        trd_exit <- NULL;
        for (i in 2:nrow(dat_pnl)) {
            dat_pnl$nav_morning[i] <- dat_pnl$nav_evening[i-1];
            date_i_1 <- dat_pnl$date[i-1];
            date_i <- dat_pnl$date[i];
            print(date_i)
            trd_new <- prepareNewTrades(date_i, dat_pnl$nav_morning[i]);
            if (!is.null(trd_live)) {
                trd_exit <- trd_live %>%
                    filter(date_exit == date_i);
            }

            dat_pnl$pnl_new[i] <- 0;
            dat_pnl$pnl_live[i] <- calcPnlLiveTrades(date_i, date_i_1, trd_live);
            dat_pnl$pnl_exit[i] <- calcPnlExitTrades(date_i, trd_exit);
            dat_pnl$pnl_day[i] <- dat_pnl$pnl_new[i] + dat_pnl$pnl_live[i] + dat_pnl$pnl_exit[i];
           
            dat_pnl$nav_evening[i] <- dat_pnl$nav_morning[i] + dat_pnl$pnl_day[i];
            dat_pnl$n_trades_new[i] <- if (is.null(trd_new)) 0 else nrow(trd_new);
            dat_pnl$n_trades_live[i] <- if (is.null(trd_live)) 0 else nrow(trd_live);
            dat_pnl$n_trades_exit[i] <- if (is.null(trd_exit)) 0 else nrow(trd_exit);
            
            if ((day(date_i+1) == 1) & ((month(date_i) %% 3) == 0)) {
                print(
                    dat_pnl %>%
                        filter(date <= date_i) %>%      
                        ggplot(aes(x = date, y = nav_evening)) +
                        geom_line(color = "lightseagreen") +
                        scale_x_date(
                            date_breaks = "years", 
                            date_labels = "%y",
                            limits = c(start_date, end_date)
                            ) +
                        scale_y_continuous(labels = comma) +
                        theme(
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            legend.position = "bottom"
                        )
                    
                )
            }
            
            trd_live <- rbind(trd_live, trd_new);
            if (!is.null(trd_live)) {
                trd_live <- trd_live %>%
                    filter(date_exit > date_i)
            }
        }
        dat_pnl
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    pnlInit() %>% 
        calcPnL;
    
    
    
    
    
    
    
    
    
    
    
    
    
}
T.rollApply <-
function(this_vector, this_width, this_function) {
    ####################################################################################################
    ### Script
    ####################################################################################################
    rollapply(
        data = this_vector, 
        width = this_width, 
        FUN = this_function, 
        align = "right",
        fill = NUM_NA
    ) %>%
        U.vectorize;
}
T.save <-
function() {
    All_Objects <- ls(".GlobalEnv", pattern = "T.");
    All_Objects <- All_Objects[substr(All_Objects,1,2) == "T."];
    dump(All_Objects, paste0(DIRECTORY_CODE_HD, "/Code/Technicals.R"));
}
T.testAbove <-
function(x, x_lvl, width) {
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    x0 <- x_lvl - width
    x1 <- x_lvl
    
    x[which(x<x0)] <- x0
    x[which(x>x1)] <- x1
    x[which(is.na(x))] <- x0
    (1-cos(pi*(x-x0)/(x1-x0)))/2
    
}
T.testBelow <-
function(x, mid, width) T.testAbove(-x, -mid, width)
T.updateFutureSpreads <-
function () 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    
    pair_list <- c("CACEUR", "DAXEUR")
    
    url_investing <- "https://www.investing.com/%s-historical-data"
    
    FUTURES <- D.loadTableLocal("future_contract")
    FUTURES_EXP <- D.loadTableLocal("future_expiry")
    
    future_id_list <- data.frame(pair = pair_list) %>%
        left_join(INSTRUMENTS, by = "pair") %>%
        left_join(FUTURES, by = "instrument_id") %>%
        select(pair, future_id, instrument_id)
    
    ACCEPTABLE_THRESHOLD_PCT = 0.2 / 100
    
    ####################################################################################################
    ### Script sub functions
    ####################################################################################################
    
    generateInvestingURL <- function(this_future_id) {
        this_url <- NULL
        future_page <- filter(FUTURES, future_id == this_future_id)$investing_url
        if (future_page != "") {
            this_url <- url_investing %>%
                sprintf(future_page)
        }
        this_url
    }
    
    retrieveWebPage <- function(this_url) {
        print(this_url)
        time_start <- Sys.time()
        time_max <- time_start + 10*60
        dat <- NULL
        
        while (is.null(dat) & (Sys.time() < time_max)) {
            dat <- U.tryNull(read_html, this_url);
            if (is.null(dat))
                Sys.sleep(10)
        }
        dat
    }
    
    formatData <- function(dat, this_future_id) {
        dat <- dat %>% 
            html_node("#curr_table") %>% 
            html_table 
        
        colnames(dat)[1:5] <- c("date", "close_fut", "open_fut", "high_fut", "low_fut")
        dat$date <- as.Date(dat$date, format = "%b %d, %Y")
        dat$open_fut <- as.numeric(U.vectorize(gsub(",", "", dat$open_fut)))
        dat$high_fut <- as.numeric(U.vectorize(gsub(",", "", dat$high_fut)))
        dat$low_fut <- as.numeric(U.vectorize(gsub(",", "", dat$low_fut)))
        dat$close_fut <- as.numeric(U.vectorize(gsub(",", "", dat$close_fut)))
        
        dat %>%
            mutate(future_id = this_future_id) %>% 
            select(future_id, date, open_fut, high_fut, low_fut, close_fut) %>%
            arrange(date) %>%
            tail(15) %>%
            U.data2Tibble
        
    }
    
    getHistoFuturePrice <- function(this_future_id) {
        this_future_id %>%
            generateInvestingURL %>%
            retrieveWebPage %>%
            formatData(this_future_id)
    }
    
    loadCorrespondingInstrumentHisto <- function(dat_histo_future, this_future_id) {
        this_instrument_id <- filter(future_id_list, future_id == this_future_id)$instrument_id
        dat <- "SELECT * FROM histo_px_daily WHERE instrument_id = %s AND date >= '%s'" %>%
            sprintf(this_instrument_id, TO_DAY - 21) %>% 
            D.select %>% U.debug("SQL") %>%
            semi_join(dat_histo_future, by = "date") %>%
            left_join(dat_histo_future, by = "date") %>% U.debug("join") %>%
            mutate(
                threshold = close * ACCEPTABLE_THRESHOLD_PCT,
                spread_open = open_fut - open,
                spread_high = high_fut - high,
                spread_low = low_fut - low,
                spread_close = close_fut - close
            ) %>% U.debug("spread")
        
        for (this_col in c("open", "high", "low", "close")) {
            col_name <- paste0("spread_", this_col)
            pos_too_large <- which(abs(dat[col_name]) > dat$threshold)
            print(this_col)
            print(pos_too_large)
            dat[col_name][pos_too_large,] <- NUM_NA
        }
        dat %>% U.debug("fixed spread")

        
        
    }
    
    
    calcSpread <- function(this_future_id) {
        this_future_id %>%
            getHistoFuturePrice %>% U.debug(1) %>%
            loadCorrespondingInstrumentHisto(this_future_id)
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    future_id_list$future_id %>%
        lapply(calcSpread) %>%
        bind_rows
}
T.updateFutureSpreads2 <-
function (pair_list = c("CACEUR", "DAXEUR")) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    
    url_investing <- "https://www.investing.com/indices/%s"
    url_index <- sprintf(url_investing, "major-indices")
    url_future <- sprintf(url_investing, "indices-futures")

    FUTURES <- D.loadTableLocal("future_contract")
    FUTURES_EXP <- D.loadTableLocal("future_expiry")
    
    future_id_list <- data.frame(pair = pair_list) %>%
        left_join(INSTRUMENTS, by = "pair") %>%
        left_join(FUTURES, by = "instrument_id") %>%
        select(pair, future_id, instrument_id)
    
    ACCEPTABLE_THRESHOLD_PCT = 0.2 / 100
    
    INDEX_MAP <- data.frame(
        pair = c(
            1,2,3,4,5
            ),
        index_lbl = c(
            1,2,3,4,5
            ),
        future_lbl = c(
            1,2,3,4,5
            )
    )
    
    ####################################################################################################
    ### Script sub functions
    ####################################################################################################
    
    getLivePrice <- function(this_url) {
        dat <- this_url %>%
            read_html %>%
            html_node(".crossRatesTbl") %>%
            html_table
        cols_keep <- c("Index", "Month", "Last", "High", "Low", "Time")
        dat <- dat[, which(colnames(dat) %in% cols_keep)]
        colnames(dat) <- tolower(colnames(dat))
        
    #    colnames(dat)[1:5] <- c("date", "close_fut", "open_fut", "high_fut", "low_fut")
    #    dat$date <- as.Date(dat$date, format = "%b %d, %Y")
    #    dat$open_fut <- as.numeric(U.vectorize(gsub(",", "", dat$open_fut)))
    #    dat$high_fut <- as.numeric(U.vectorize(gsub(",", "", dat$high_fut)))
    #    dat$low_fut <- as.numeric(U.vectorize(gsub(",", "", dat$low_fut)))
    #    dat$close_fut <- as.numeric(U.vectorize(gsub(",", "", dat$close_fut)))
        
    #    dat %>%
    #        mutate(future_id = this_future_id) %>% 
    #        select(future_id, date, open_fut, high_fut, low_fut, close_fut) %>%
    #        arrange(date) %>%
    #        tail(15) %>%
    #        U.data2Tibble
    }

    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    getLivePrice(url_index)
    getLivePrice(url_future)
}
T.updateLiveTable <-
function (fx_pair, px_open, px_high, px_low, px_close, save_data = TRUE) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    file_name <- paste0(DIRECTORY_DATA_HD, "Spot/Live/px_live_latest.RData")
    print(file_name)
    load(file_name);
    dat <- dat_live;
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    pos_replace <- which((dat$pair == fx_pair) & (dat$date == TO_DAY))
    if (length(pos_replace) == 0) {
        U.printBanner("No data for today yet")
        dat_new <- dat %>% filter(pair == fx_pair) %>% tail(1);
        dat_new$date <- TO_DAY;
        dat_new$open <- px_open;
        dat_new$high <- px_high;
        dat_new$low <- px_low;
        dat_new$close <- px_close;
        print(dat_new %>% U.dataFrame)
        dat <- rbind(dat, dat_new) %>% arrange(pair, date);
    }
    else {
        U.printBanner("Data before update:")
        print(dat[pos_replace,] %>% U.dataFrame)
        dat$open[pos_replace] <- px_open;
        dat$high[pos_replace] <- px_high;
        dat$low[pos_replace] <- px_low;
        dat$close[pos_replace] <- px_close;        
        U.printBanner("Data after update:")
        print(dat[pos_replace,] %>% U.dataFrame)
    }
    dat_live <- dat;
    
    if (save_data) {
        file_name <- paste0(DIRECTORY_DATA_HD, "Spot/Live/px_live_latest.RData")
        save(dat_live, file = file_name);
        file_name_new <- gsub("latest", U.dateTimeFormatForExport(), file_name);
        file.copy(file_name, file_name_new);
    }
    

    
    
    file_name <- paste0(DIRECTORY_DATA_HD, "Spot/Live/technicals_live_latest.RData")
    load(file_name)
    print(file_name)
    
    dat_technicals_pair <- dat_live %>% 
        filter(pair == fx_pair) %>%
        tail(
            21 +
                VENTURA$technical_param$vol$nb_years_vol_lt_decile *
                VENTURA$technical_param$vol$days_vol_lt
        ) %>%
        T.calcTechnicalsLive(TRUE) %>%
        mutate(
            sup_exp_close_lagged = lag(sup_exp_close, 5),
            sup_exp_close_chg = sup_exp_close - lag(sup_exp_close_lagged, 5)
        ) %>%
        filter(date >= YESTERDAY) %>%
        tail(1);
    
    dat_technicals <- dat_technicals %>%
        filter(pair != fx_pair) %>%
        rbind(dat_technicals_pair) %>%
        arrange(pair, date);
    
    if (save_data) {
        file_name <- paste0(DIRECTORY_DATA_HD, "Spot/Live/technicals_live_latest.RData");
        save(dat_technicals, file = file_name);
        file_name_new <- gsub("latest", U.dateTimeFormatForExport(), file_name);
        file.copy(file_name, file_name_new);
    }
    list(dat_live, dat_technicals)
    
}
T.urlAssets <-
"https://docs.google.com/spreadsheets/d/e/2PACX-1vRHXNwpFTyh8tr-ufX3FrCtPnYcta9E8b_GGOir1IzTtgX3dzuyNSGBpuIuEsxPUYwONfde_DSwChcQ/pub?gid=0&single=true"
T.urlAssetsBond <-
"https://docs.google.com/spreadsheets/d/e/2PACX-1vTo5muS4Uv8U-tjS-w0P3BaY5S5PboJCS2Jqu-FCtJQY56uvkfyU_p-9g5eiYwMGqVdTXTSJcXExr4F/pub?gid=0&single=true&output=csv"
T.urlAssetsETF <-
"https://docs.google.com/spreadsheets/d/e/2PACX-1vTLjdVwWtO1Opy8X0aRf0ggxqsA-hnzfayxWqF4UJS_z9v_EBkJODZPiRz8tALeq0NushXnRBMH16B2/pub?gid=0&single=true&output=csv"
T.urlAssetsFXDM <-
"https://docs.google.com/spreadsheets/d/e/2PACX-1vRjR_AwVPKvATuKzPgwq3OKAT_iYyd3d6lZ-UZiAhRgMSZlRxE-73Da6ybi_0soDxIqkVEFm_hnJ25u/pub?gid=0&single=true&output=csv"
T.urlAssetsFXEM <-
"https://docs.google.com/spreadsheets/d/e/2PACX-1vSm9OEklw9WgwmmjTQ5YTz3ba2nGAo3gafRa9Z-S42IWfhmi1b_eomRuhjfDJl2iJ83uLEJv0i7TvYF/pub?gid=0&single=true&output=csv"
T.urlAssetsIndex <-
"https://docs.google.com/spreadsheets/d/e/2PACX-1vQl1XasmoDIzQMFCl-0vkm9LXws9Ea3OMGMGybS4EqG1aD8ySRhJ2Z0Kr1_CgztMXBjE_Tb9QTIabmy/pub?gid=0&single=true&output=csv"
T.urlAssetsMetal <-
"https://docs.google.com/spreadsheets/d/e/2PACX-1vSibmGvb7gzggXrvQKEw6RqsVxgPOYm0VwRFE94G9mW_7PMFuiOQ0o0MqFum93nVxUXXCBfLRERvwyI/pub?gid=0&single=true&output=csv"
T.whichMaxLast <-
function (x) 
{
    ####################################################################################################
    ### Script 
    ####################################################################################################
    pos_max <- which.max(x);
    res <- 1000;
    if (length(pos_max) > 0) {
        if (pos_max > 0) {
            res <- length(x) - pos_max;
        }
    }
    res;
}
T.whichMinLast <-
function (x) 
{
    ####################################################################################################
    ### Script 
    ####################################################################################################
    pos_min <- which.min(x);
    res <- 1000;
    if (pos_min > 0) {
        res <- length(x) - pos_min;
    }
    res;
}
T.xtsToDf <-
function (dat_xts) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    upperCaseFirstLetterLowerCaseRest <- function(str) {
        paste0(
            toupper(substr(str, 1, 1)),
            tolower(substr(str, 2, nchar(str)))
        )
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    colnames(dat_xts) <- tolower(colnames(dat_xts));
    dat <- data.frame(
        date = index(dat_xts),
        dat_xts
    ) %>% 
        U.data2Tibble;
    rownames(dat) <- NULL;
    dat;
}
T.zzzCalcSpline <-
function (fx_pair, only_recent = FALSE) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    period_type <- "D";
    
    nb_years_vol_lt_decile <- filter(TECH_PARAM, technical_param == "nb_years_vol_lt_decile")$value
    days_vol_lt <- filter(TECH_PARAM, technical_param == "vol_days_lt")$value
    bb_width <- filter(TECH_PARAM, technical_param == "bb_width")$value
    spline_nb_points <- filter(TECH_PARAM, technical_param == "spline_nb_points")$value
    n_row <- 21 + nb_years_vol_lt_decile * days_vol_lt
    
    n_vol_annual <- filter(TECH_PARAM, technical_param == "n_periods_year")$value
    recent_days <-  130 + n_row;

    ####################################################################################################
    ### Script sub functions
    ####################################################################################################
    
    finalCleanData <- function(dat) {
        dat <- dat %>% 
            U.dataFrame;
        
        dat0 <- dat_histo %>%
            rename(open_f = open, high_f = high, low_f = low) %>%
            select(date, open_f, high_f, low_f)
        
        dat <- dat %>%
            left_join(dat0, by = "date") %>%
            mutate(open = open_f, high = high_f, low = low_f) %>%
            select(-open_f, -high_f, -low_f)
        
        dat <- dat[,setdiff(colnames(dat), c("open_na", "low_na", "high_na"))]
        
        for (j in 1:ncol(dat)) {
            dat[which(is.nan(dat[,j])),j] <- NA;
            dat[which(is.infinite(dat[,j])),j] <- NA;
        }
        
        dat$asset_class <- factor(
            U.vectorize(dat$asset_class), 
            levels = c("fx_dm", "fx_em", "index", "metal", "yield", "bond")
        );

        dat;
    }
    
    keepOnlyRecentDataIfNecessary <- function(dat) {
        if (only_recent) {
            dat <- dat %>% 
                tail(recent_days)
        }
        dat;
    }
    
    addTechnicals <- function() {
        dat_histo %>% 
            U.printMilestone("Starting...") %>%
            U.printMilestone("Keeping only necessary data...") %>%
            keepOnlyRecentDataIfNecessary %>% U.printMilestone("Filling missing NAs...") %>%
            T.fillMissingOpenWithPreviousClose %>%
            T.fillMissingHighAndLowWithClose %>% U.printMilestone("Adding Volatility...") %>%
            T.addSTVol(period_type) %>%
            T.addLTVol(period_type) %>% 
            U.data2Tibble %>% U.printMilestone("Adding Splines...") %>%
            T.addAllSplines %>%
            finalCleanData %>% U.printMilestone("Finished...");
    }
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    dat_histo <- T.getHistoPx(fx_pair);
    
    addTechnicals();
}
T.zzzCalcSplineFull <-
function (fx_pair) 
{
    
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    tech_file_name <- "%sSpot/Technicals_P/Technicals_%s.RData" %>%
        sprintf(DIRECTORY_DATA_HD, fx_pair);
    
    FEATURES <- D.loadTable("static_feature");
    
    instrument_id = A.getInstrumentId(fx_pair);
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    calcTech <- function() T.calcTechnicals(fx_pair)
    
    saveToFile <- function(dat) {
        save(dat, file = tech_file_name)
        dat
    }

    formatDataForDB <- function(dat) {
        dat %>%
            select(-open, -high, -low, -close) %>%
            gather(feature, value, -instrument_id, -date) %>%
            na.omit %>%
            left_join(FEATURES, by = "feature") %>%
            filter(!is.na(feature_id));
    }
    
    deleteDBTechnicalsBeforeSaving <- function(int_or_dbl) {
        "DELETE
        FROM histo_technicals_%s
        WHERE instrument_id = %s" %>%
            sprintf(int_or_dbl, instrument_id) %>%
            D.SQL;
    }
    
    saveTechnicalsToDB <- function(dat, int_or_dbl) {
        U.printBanner("Now saving to DB", FALSE)
        deleteDBTechnicalsBeforeSaving(int_or_dbl);
        feature_type <- switch(int_or_dbl, "int" = 0, "dbl" = 1);
        db_tbl_name <- paste0("histo_technicals_", int_or_dbl);
        dat %>%
            filter(int_or_dbl == feature_type) %>%
            select(instrument_id, date, feature_id, value) %>%
            D.insertDataIntoTable(db_tbl_name, .)
        U.printBanner("Saving to DB done", FALSE)
        dat;
    }
    
    doTechnicalsFull_Try <- function() {
        calcTech() %>%
            saveToFile %>%
            formatDataForDB %>%
            saveTechnicalsToDB("int") %>%
            saveTechnicalsToDB("dbl");
    }
    doTechnicalsFull <- function()
        U.try(doTechnicalsFull_Try)()
    
    removeFirstPareto <- function() {
        initial_date <- "SELECT MIN(date) AS date FROM histo_px_daily WHERE instrument_id = %s" %>%
            sprintf(instrument_id) %>%
            D.select %>% 
            .$date;
        first_ok_date <- initial_date %m+% years(1);
        features_pareto <- FEATURES %>%
            filter(feature %in% c(
                "pl_alpha_left", "pl_l_left", "pl_alpha_right", "pl_l_right", 
                "pl_l_width", "pl_alpha_ratio"
            )
            ) %>% 
            select(feature_id) %>% 
            U.vectorize;
        
        "DELETE FROM histo_technicals_dbl
        WHERE instrument_id = %s
        AND feature_id IN (%s)
        AND date <= '%s'" %>%
            sprintf(
                instrument_id,
                paste(features_pareto, collapse = ", "),
                first_ok_date
                ) %>%
            D.SQL;
    }
    
    computeAllTradingConfigurations <- function() {
        U.printBanner("Computing trade outcomes", FALSE)
        U.try(T.addTradingDataAllPossibilities)(fx_pair)
    }

    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    doTechnicalsFull();
    removeFirstPareto();
    
    computeAllTradingConfigurations();
    gc();
    NULL;
}
