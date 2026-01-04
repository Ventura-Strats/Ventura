####################################################################################################
### Calculates powerlaw parameters
####################################################################################################
start_time <<- Sys.time()
script_name <<- "PowerLaw"
max_time_hours <<- 0.3

script_name <<- paste0(script_name, "_", as.integer(commandArgs(trailingOnly = TRUE)[2]))

####################################################################################################
### Sub routines
####################################################################################################
getInstrumentByChoosingTheLeastPopulatedOne <- function() {
  instruments_ranked_by_population_ratio <- "SELECT instrument_id
  FROM status_instrument
  WHERE instrument_id IN (
	SELECT instrument_id
    FROM static_instrument
    WHERE use_for_training = 1
    )
  ORDER BY pareto_date_coverage_ratio ASC
  LIMIT 20" %>%
    D.select %>%
    U.vectorize

  instruments_where_last_date_is_not_recent <- 
    "SELECT instrument_id 
    FROM status_instrument 
    WHERE pareto_last_update <= '%s'
    AND instrument_id IN (
	    SELECT instrument_id
      FROM static_instrument
      WHERE use_for_training + use_for_trading >= 1
    )
  " %>%
    sprintf(YESTERDAY) %>% 
    D.select %>% 
    U.vectorize
  
  instrument_id <- if (length(instruments_where_last_date_is_not_recent) > 0) {
    instruments_where_last_date_is_not_recent 
  }
  else {
    instruments_ranked_by_population_ratio
  }
  instrument_id %>% U.scrambleVector %>% first
}

####################################################################################################
### Script
####################################################################################################
Script <- function() {
  this_instrument_id <- getInstrumentByChoosingTheLeastPopulatedOne()
  this_pair <- filter(INSTRUMENTS, instrument_id == this_instrument_id)$pair
  U.printBanner(paste0("Doing this instrument:", this_pair)); I.completeStage(1)
  this_instrument_id %>%
    T.getHistoPx %>%
    T.calcPareto(10, TRUE)
  I.completeStage(2)
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()