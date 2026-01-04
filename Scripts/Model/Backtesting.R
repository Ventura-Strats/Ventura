####################################################################################################
### Keeps only a number of features in the model
####################################################################################################
start_time <<- Sys.time()
script_name <<- "Backtesting"
max_time_hours <<- 120

script_args <- commandArgs(trailingOnly = TRUE)
model_to_do <<- as.integer(script_args[2])
use_weights <- as.logical(script_args[3])
start_date <- as.Date(script_args[4])
end_date <- as.Date(script_args[5])


if (is.na(start_date)) start_date <- as.Date("1995-01-01")
if (is.na(end_date)) end_date <- Sys.Date()
if (is.na(use_weights)) use_weights <- FALSE

####################################################################################################
### Script
####################################################################################################
Script <- function() {
  V.backtestModel(model_to_do, start_date, end_date, use_weights); I.completeStage(1)
}

####################################################################################################
### Execution
####################################################################################################
source("Init.R"); I.executeScript()