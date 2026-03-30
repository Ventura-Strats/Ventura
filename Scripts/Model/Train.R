####################################################################################################
### Prepares models, run after we downloaded latest prices and recomputed technicals
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Train"
max_time_hours <<- 2

script_arg <<- as.integer(commandArgs(trailingOnly = TRUE)[2])
use_weights <- script_arg > 4
script_arg_1 <- as.character(script_arg %% 4)

strats_list <- switch(
  script_arg_1,         #   No Weights      Weights
  "1" = c(4,6,11),      #      V              V
  "2" = c(1,10,13,7,9), #      W              W
  "3" = c(2,3,5,12,8),  #      I              I
  "0" = 14              #      Y              Z
)

# Models by machine
#         Train                               Predict
#    No Weight           Weight          No Weight    Weight
# H                                                   4, 11, 9, 6, 1, 10, 5
# I  2 3 5 12 8         2 3 5 12 8      7, 8, 2, 13
# V  4 6 11             4 6 11          14, 12, 3     
# W  1 10 13 7 9        1 10 13 7 9     4, 11, 9      
# Y  14                                               14, 12, 3
# Z                     14              6, 1, 10, 5   7, 8, 2, 13

# 1   4162  
# 2   2074  
# 3   1314  
# 4   8880  
# 5   1958  
# 6   6474  
# 7   2604  
# 8   2533  
# 9   3096  
# 10  3330  
# 11  6652  
# 12  1882  
# 13  3130  
# 14  20014 

####################################################################################################
### Sub routines
####################################################################################################
copyModelsToLocalFiles <- function(strategy_id, use_weight) {
  
  machine <- if (use_weight) {
    switch(
      strategy_id, 
      "1"="Z", "2"="I", "3"="V", "4"="W", "5"="Z", "6"="Z", "7"="I", 
      "8"="I", "9"="W", "10"="Z", "11"="W", "12"="V", "13"="I", "14"="V"
      )
  } else {
    switch(
      strategy_id, 
      "1"="H", "2"="Z", "3"="Y", "4"="H", "5"="H", "6"="H", "7"="Z", 
      "8"="Z", "9"="H", "10"="H", "11"="H", "12"="Y", "13"="Z", "14"="Y"
    )
  }
  
  ip_address <- switch(machine, "H"= 34, "I"=42, "V"=40, "W"=39, "Y"=41, "Z"=38) %>% 
    paste0("192.168.0.", .)
}

####################################################################################################
### Script
####################################################################################################
Script <- function() {
 # D.waitTillPreviousJobHasFinished("Load_Tech", 3, 1, 60, 25); I.completeStage(2)
  V.prepareMorningModel(strats_list, use_weights); I.completeStage(1)
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()