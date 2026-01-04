####################################################################################################
### Makes model predictions based on latest live prices
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Diagnostic"
max_time_hours <<- 15/60

strats_list <<- switch(
  as.integer(commandArgs(trailingOnly = TRUE)[2]),
  "1" = c(4,1),
  "2" = c(6,2,3,5),
  "3" = c(7,8,9,13),
  "4" = c(11,10,12),
  "5" = 14
)

####################################################################################################
### Script
####################################################################################################
Script <- function() {
  file_path_model <<- paste0(DIRECTORY_ROOT, "Mount/Glenorchy_Fundamentals/Models_FX/model_%s.RData")
  dat_tech <- T.getTechnicalsLive()
  for (strat_id in strats_list) {
    U.printBanner(paste0("Doing strategy ", strat_id))
    # file_name <- sprintf(file_path_model, strat_id)
    # load(file_name)
    # predict_model <- E.modelPredict(dat_tech, dat_trades_models, TRUE) %>%
    #   select(instrument_id, starts_with("predict"))
    # rm(dat_trades_models)
    # gc()
    
    predict_model <- 
      "SELECT P.instrument_id, 
	      O.outcome AS predict, 
      	P.proba_up AS predict_proba_up, 
        P.proba_flat AS predict_proba_flat, 
        P.proba_down AS predict_proba_down
      FROM live_predict P
      LEFT JOIN static_trade_outcome O ON O.outcome_id = P.outcome_id
      WHERE P.strategy_id = %s" %>%
      sprintf(strat_id) %>%
      D.select
    
    file_name <- sprintf(file_path_model, paste0("weights_", strat_id))
    load(file_name)
    predict_model_weights <- E.modelPredict(dat_tech, dat_trades_models, TRUE) %>%
      select(instrument_id, starts_with("predict"))
    rm(dat_trades_models)
    gc()
    colnames(predict_model_weights) <- gsub("predict", "predict_weight", colnames(predict_model_weights))
    
    dat <- dat_tech %>% 
      E.genericScoreFunction(strat_id) %>%
      mutate(strategy_id = strat_id) %>%
      left_join(select(INSTRUMENTS, instrument_id, pair), by = "instrument_id") %>%
      select(instrument_id, strategy_id, pair, date, timestamp_px, score) %>% 
      left_join(predict_model, by = "instrument_id") %>%
      left_join(predict_model_weights, by = "instrument_id") %>%
      select(-instrument_id)
      U.write.csv(dat, sprintf("/home/fls/Mount/Glenorchy/FX/Model_Diagnostic/model_diagnostic_%s_%s.csv", strat_id, U.dateTimeFormatForExport()))
      U.write.csv(dat, sprintf("/home/fls/Mount/Glenorchy/FX/Model_Diagnostic/model_diagnostic_%s_latest.csv", strat_id))
  }
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()