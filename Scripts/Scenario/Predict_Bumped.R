####################################################################################################
### Makes model predictions bases on bumped prices, 
### so we know ahead of time what trades we might do today, in what range, 
### and to test for prediction stability
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Predict_Bumped"
max_time_hours <<- 1

script_arg <<- as.integer(commandArgs(trailingOnly = TRUE)[2])

strats_list <<- switch(
  script_arg,
  "1" = c(4,2),
  "2" = c(6,1),
  "3" = c(7,8,11),
  "4" = c(9,10,12),
  "5" = c(3,5,13),
  "6" = 14,
  "7" = 14
)

# 1   4162  2
# 2   2074  4
# 3   1314  4
# 4   8880  1
# 5   1958  4
# 6   6474  1
# 7   2604  3
# 8   2533  3
# 9   3096  3
# 10  3330  2
# 11  6652  1
# 12  1882  4
# 13  3130  3
# 14  20014 5

####################################################################################################
### Script
####################################################################################################
Script <- function() {
  bump_list <- D.loadTable("static_scenario_bump")$bump
  if (strats_list[1] == 14) {
    mid_bump <- floor(length(bump_list) / 2)
    bump_list <- switch(
      script_arg,
      "6" = head(bump_list, mid_bump),
      "7" = tail(bump_list, -mid_bump)
    )
  }
  
  V.modelPredictScenario(strats_list, TRUE, bump_list); I.completeStage(1)
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()