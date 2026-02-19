####################################################################################################
### Recomputes all technicals history (very slow)
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Full"
max_time_hours <<- 5

####################################################################################################
### Sub routines
####################################################################################################
whichPairsToDo <- function() {
  dat <- A.instrumentsWithBadData()
  dat_no_date <- filter(dat, is.na(max_date_technicals))
  dat_no_full <- dat %>%
    filter(
      is.na(histo_technicals_last_full_recompute),
      asset_class != "bond"
      )
  dat_date_calc <- dat %>% 
    filter(!is.na(histo_technicals_last_full_recompute), asset_class != "bond") %>% 
    arrange(histo_technicals_last_full_recompute)
  dat_date_tech <- dat %>% 
    filter(!is.na(max_date_technicals)) %>% 
    arrange(max_date_technicals)
  
  rbind(dat_no_date, dat_no_full, dat_date_calc, dat_date_tech) %>% 
    head(20) %>% U.debug("List") %>%
    sample_n(1) %>% U.debug("Item") %>%
    .$pair

  
}

calcTech_Try <- function(this_pair) {
  U.printTickerProgressVerbose(this_pair, pair_list)
  T.calcTechnicalsFull(this_pair)
}

calcTech <- function(this_pair) 
  U.try(calcTech_Try, NULL)(this_pair) 

####################################################################################################
### Script
####################################################################################################
Script <- function() {
  pair_list <<- whichPairsToDo(); I.completeStage(1)
  dat_tech <- lapply(pair_list, calcTech); I.completeStage(2)
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()