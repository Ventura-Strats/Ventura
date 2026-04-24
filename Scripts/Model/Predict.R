####################################################################################################
### Makes model predictions based on latest live prices
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Predict"
max_time_hours <<- 30/60

script_arg <<- as.integer(commandArgs(trailingOnly = TRUE)[2])
use_weights <<- script_arg > 4

strats_list <<- switch(
  as.character(script_arg %% 4),
  "0" = c(14, 12, 3),     # 4: V    8: Y
  "1" = c(4, 11, 9),      # 1: W    5: H
  "2" = c(6, 1, 10, 5),   # 2: Z    6: H
  "3" = c(7, 8, 2, 13)    # 3: I    7: Z
)

# So strategies per machine:
# Weights
# H  
# I  2, 7, 8, 13
# V  3, 12, 14
# W  4, 9, 11
# Y  
# Z  1, 5, 6, 10

# No Weights
# H  1, 4, 5, 6, 9, 10, 11
# I  
# V  
# W  
# Y  3, 12, 14
# Z  2, 7, 8, 13

# 14  20014 
# 4   8880  
# 11  6652  
# 6   6474  
# 1   4162  
# 10  3330  
# 13  3130  
# 9   3096  
# 7   2604  
# 8   2533  
# 2   2074  
# 5   1958  
# 12  1882  
# 3   1314  


####################################################################################################
### Sub routines
####################################################################################################
whichPairsToDo <- function(exec_time_id_if_just_before_execution) {
  minute(time_limit) <- U.mround(minute(time_limit), 5)
  print("Time limit")
  print(time_limit)
  dat_pair <- filter(INSTRUMENTS, pair %in% A.filterInstruments("predict"))
  if (!is.na(exec_time_id_if_just_before_execution)) {
    dat_pair <- filter(dat_pair, execution_time_id == exec_time_id_if_just_before_execution)
  }
  dat_pair$pair %>% U.debug("Instruments for predictions:")
}

####################################################################################################
### Script
####################################################################################################
Script <- function() { 
  
  predict_time <- Sys.time()
  second(predict_time) <- 0
  minute(predict_time) <- U.mround(minute(predict_time)+1, 5)
  predict_time <- predict_time %>% 
    as.character %>% 
    as.POSIXct(tz=TZ_LOCAL)
  predict_time <<- predict_time
  
  time_limit <- start_time + 2*60
  minute(time_limit) <- U.mround(minute(time_limit), 5)
  time_limit <<- time_limit
  exec_time_id_if_just_before_execution <- A.getExecutionTimeId(time_limit)
  pair_list <- whichPairsToDo(exec_time_id_if_just_before_execution)
  V.modelPredict(strats_list, "live", predict_time, TRUE, pair_list, use_weights); I.completeStage(1)
  gc()
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()