I.completeStage <-
function (this_stage) { 
    ####################################################################################################
    ### Script
    ####################################################################################################
    if (is.null(this_stage)) {
        this_stage <- 1 + U.ifelse(U.variableExists(completed_stage), completed_stage, 0)
    }
    completed_stage <<- this_stage;
    
    if (U.variableExists(script_id)) {
        "UPDATE status_script 
        SET last_completed_stage = %s 
        WHERE script_id = %s AND start = '%s' AND machine_id = %s AND run_id = %s AND pid = %s" %>%
            sprintf(
                completed_stage, 
                script_id, 
                as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
                machine_id,
                script_run_id,
                script_pid
            ) %>% D.SQL
    }
    U.printBanner(sprintf("Completed stage %s", completed_stage))
    gc()
    NULL
}
I.executeScript <-
function () {
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    char_start_time <- as.character(format(start_time, "%Y-%m-%d %H:%M:%S"))
    
    ####################################################################################################
    ### Sub Routines
    ####################################################################################################
    testAccessHardDrives <- function() {
        test_code_hd <- list.files(DIRECTORY_CODE_HD) %>% U.noData2Null
        test_code_sd <- list.files(DIRECTORY_CODE_SD) %>% U.noData2Null
        test_data_hd <- list.files(DIRECTORY_DATA_HD) %>% U.noData2Null
        test_data_sd <- list.files(DIRECTORY_DATA_SD) %>% U.noData2Null
        (!is.null(test_code_hd) & !is.null(test_code_sd) & !is.null(test_data_hd) & !is.null(test_data_sd))
    }
    
    testCanExecute <- function() {
        D.testWriteDB() & testAccessHardDrives()
    }
    
    getScriptId <- function() {
        D.loadTableLocal("script") %>%
            filter(script == script_name) %>%
            .$script_id %>%
            U.noData2Null
    }
    
    getMachineId <- function() {
        D.loadTableLocal("machine") %>%
            filter(machine == THIS_COMPUTER) %>%
            .$machine_id %>%
            U.noData2Null
    }
    
    getScriptIdFromCommandLineIfThereIsOne <- function() {
        script_run_id <- as.integer(commandArgs(trailingOnly = TRUE)[2])
        if (is.na(script_run_id)) {
            script_run_id <- 0
        }
        if (grepl("PowerLaw", script_name)) {
            script_run_id <- 0
        }
        script_run_id
    }
    
    getScriptRunId <- function() {
        U.try(getScriptIdFromCommandLineIfThereIsOne, 0)()
    }
    
    execution_script <- function() {
        max_time_seconds <- max_time_hours * 3600
        res_script <- withTimeout( 
            try({
                U.printBanner(paste0("Starting Script - timeout = ", Sys.time() + max_time_seconds))
                res <- Script()
                U.printBanner(paste0("Script Complete - ", script_name))
            }, TRUE),
            timeout = max_time_seconds
        )
        if (class(res_script) == "try-error") {
            U.printBanner(paste0("Script Failed - ", script_name))
        }
    }
    
    saveScriptStartDataInDB <- function() {
        "INSERT INTO status_script 
        (script_id, start, complete, machine_id, pid, run_id) 
        VALUES (%s, '%s', 0, '%s', %s, %s)" %>%
            sprintf(script_id, char_start_time, machine_id, script_pid, script_run_id) %>%
            D.SQL
        I.completeStage(0)
    }
    
    saveScriptEndDataInDB <- function() {
        "UPDATE status_script 
        SET complete = 1, end = '%s', last_completed_stage = %s 
        WHERE script_id = %s AND start = '%s' AND machine_id = %s AND run_id = %s AND pid = %s" %>%
            sprintf(
                as.character(format(Sys.time(), "%Y-%m-%d %H:%M:%S")), 
                completed_stage, 
                script_id, 
                char_start_time,
                machine_id,
                script_run_id,
                script_pid
            ) %>% U.debug("Script finish") %>% D.SQL
        gc()
        U.printBanner("Execution done")
    }
    
    ####################################################################################################
    ### Main Script
    ####################################################################################################
    test_can_execute <- testCanExecute()
    U.printBanner(paste0("Good to start script: ", test_can_execute), FALSE)
    if (test_can_execute) {
        script_id <<- getScriptId()
        machine_id <<- getMachineId()
        script_pid <<- Sys.getpid()
        script_run_id <<- getScriptRunId()
        saveScriptStartDataInDB()
        execution_script()
        saveScriptEndDataInDB()
    }
    else {
        U.printBanner("DB not writable - not running the script")
    }
}
I.init <-
function () 
{
    
    ####################################################################################################
    ### Environment Variables
    ####################################################################################################
    DIRECTORY_ROOT <<- substr(getwd(),1, gregexpr("/", getwd(), fixed = TRUE)[[1]][3])
    TZ_ASIA <<- "Asia/Hong_Kong"
    TZ_EUROPE <<- "Europe/London"
    TZ_AMERICA <<- "America/New_York"
    TZ_LOCAL <<- TZ_EUROPE
    
    ####################################################################################################
    ### Script sub functions
    ####################################################################################################
    
    rootDirectoryBase <- function() {
        DIRECTORY_CODE <<- DIRECTORY_ROOT %>%
        paste0("Data/System/Location_Projects.txt") %>%
        readLines %>% 
        paste0("Ventura/")
        DIRECTORY_CODE_SD <<- paste0(DIRECTORY_CODE, "SD/")
        DIRECTORY_CODE_HD <<- paste0(DIRECTORY_CODE, "HD/")
    }
    
    rootDirectoryData <- function() {
      DIRECTORY_DATA <<- DIRECTORY_ROOT %>%
        paste0("Data/System/Location_Data.txt") %>%
            readLines %>% 
            paste0("Ventura/")
      DIRECTORY_DATA_SD <<- paste0(DIRECTORY_DATA, "SD/")
      DIRECTORY_DATA_HD <<- paste0(DIRECTORY_DATA, "HD/")
    }
    
    whichComputer <- function() {
        dat_computer <- read_lines(paste0(DIRECTORY_ROOT, "Data/System/this_computer.txt"))
        U.printBanner(paste0("This computer: ", dat_computer), FALSE)
        dat_computer
    }
    
    eraseEnvironmentData <- function() {
        U.printBanner("Erase all variables", FALSE)
        rm(list=ls(all=TRUE), envir = .GlobalEnv)
    }
    
    startInit <- function() {
        source("Utils.R")
        U.printBanner("Main initialization - Start")
        eraseEnvironmentData()
        if (U.variableExists(script_name))
            U.printBanner(paste0("Script Name: ", script_name), FALSE)
        U.printBanner(paste0("Script PID: ", Sys.getpid()), FALSE)
        options(warn = -1)
        options("getSymbols.warning4.0"=FALSE)
    }
    
    testInitializationAlreadyDone <- function() {
        test_variable <- try(INIT_DONE, TRUE)
        if (class(test_variable) == "try-error")
            FALSE
        else 
            INIT_DONE
    }
    
    setTimeZone <- function() Sys.setenv(TZ=TZ_LOCAL)
    
    loadSourceCodeFiles <- function() {
        U.printBanner("Loading source files", FALSE)
        source_files <- c(
            "Utils.R",
            "Assets.R",
            "Book.R",
            "Communications.R",
            "DB.R",
            "Engine.R",
            "GUI.R",
            "Technicals.R",
            "Ventura.R"
        )
        lapply(source_files, source);
    }
    
    loadLibrary <- function(this_library) 
        suppressMessages(library(this_library, character.only = TRUE));
    
    loadLibraries <- function() {
        U.printBanner("Loading libraries", FALSE);
        libraries <- c(
            "methods", 
            "fmdates",
            "stabledist",
            "googleVis",
            "e1071",
            "pracma",
            "lmtest",
            "tseries",
            "DT",
            "data.table",
            "scales",
            "quantmod",
            "Quandl",
            "parallel",
            "TTR",
            "jsonlite",
            "IBrokers",
            "rootSolve",
            "tools",
            "formattable",
            "R.utils",
            "httr",
            "RCurl",
            "XML",
            "party",
            "poweRlaw",
            "shiny",
            "shinysky",
            "lubridate",
            "tidyverse",
            "dbplyr",
            "rvest",
            "stringi",
            "grid",
            "gtable",
            "tidyquant"
        )
        lapply(libraries, loadLibrary);
        options(dplyr.summarise.inform = FALSE);
    }
    
    loginToQuandl <- function() {
        old_key <- "kGaXWYFsyW16Nt1JhaKv";
        quandl_key <- "bxC3WYhfc1v5B4x6Lxrx"
        rr <- try(Quandl.api_key(quandl_key),1)
        if (class(rr) == "try-error") 
            Quandl.auth(quandl_key)
    }
    
    initializeApplicationVariables <- function() {
        U.printBanner("Initializating application parameters", FALSE)
        
        U.doGreeks()
        THIS_OS <<- Sys.info()[['sysname']]
        THIS_COMPUTER <<- whichComputer()

        SWITCH_NEXT_DAY_NY_TIME <<- 17
        TO_DAY <<- U.dateToday()
        YESTERDAY <<- U.calcPreviousDay(TO_DAY)
        U.printBanner(paste0("System date: ", TO_DAY), FALSE)
        
        TIME_THRESHOLD_EXEC_ROUTINE_MINUTES <<- 7
        
        rootDirectoryBase()
        rootDirectoryData()
        print(DIRECTORY_CODE_HD)
        setwd(paste0(DIRECTORY_CODE_HD, "Code/"))
        U.printBanner(paste0("Current directory: ", getwd()), FALSE)
        
        CHAR_NA <<- as.character(NA)
        INT_NA <<- as.integer(NA)
        NUM_NA <<- as.numeric(NA)
        DATE_NA <<- as.Date(NA)
        TIME_NA <<- as.POSIXct(NA)
        
        if (!U.variableExists(print_diagnostics)) {
            print_diagnostics <<- !U.variableExists(pid)
            print_diagnostics <<- TRUE
        }
        
        INVESTING_COM_IS_BLOCKED <<- FALSE
        
        U.printBanner(paste0("Print Diagnostics: ", print_diagnostics), FALSE)
    }
    
    initializeInstrumentTable <- function() {
        U.printBanner("Loading main data tables", FALSE)
        CURRENCIES <<- D.loadTableLocal("currency")
        ASSETS <<- D.loadTableLocal("asset")
        INSTRUMENTS <<- D.loadTableLocal("INSTRUMENTS") %>%
            mutate(execution_time = as.POSIXct(execution_time, tz = TZ_LOCAL))
        STRATEGIES <<- D.loadTableLocal("strategy")
        TRADE_OUTCOMES <<- D.loadTableLocal("trade_outcome")
        TECH_PARAM <<- D.loadTableLocal("technical_parameters")
        STRATEGY_LIST <<- 1:14
    }
    
    connectToDB <- function() {
        U.printBanner("Connecting to database", FALSE)
        D.connect();
    }
    
    finishInit <- function() {
        INIT_DONE <<- TRUE;
        U.printBanner("Main initialization - Done")
    }

    initializeVenturaVariables <- function() {
        load(paste0(DIRECTORY_CODE_HD, "Code/VenturaStrat.RData"))
        VENTURA$technical_param$max_date <- YESTERDAY
        MAX_TRAIN_DATE <<- YESTERDAY
        VENTURA <<- VENTURA
    }
    
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    startInit();
    
    if (!testInitializationAlreadyDone()) {
        setTimeZone()
        loadSourceCodeFiles()
        loadLibraries()
        loginToQuandl()
        initializeApplicationVariables()
        connectToDB()
        initializeInstrumentTable()
        initializeVenturaVariables()
    }
    finishInit();
    
    .Last <<- function() {
        U.printBanner("Disconnecting from Database...")
        print(dbCon)
        try_disconnect <- U.try(dbDisconnect)(dbCon);
        fail_success <- U.ifelse(is.null(try_disconnect), "Failed", "Success");
        print_message <- "Disconnection: %s" %>%
            sprintf(fail_success);
        U.printBanner(print_message, FALSE);
        U.printBanner("Goodbye...");
        Sys.sleep(1);
    }  
}
I.save <-
function() {
    All_Objects <- ls(".GlobalEnv", pattern = "I.");
    All_Objects <- All_Objects[substr(All_Objects,1,2) == "I."];
    file_path_name <- paste0(DIRECTORY_CODE_HD, "Code/Init.R")
    dump(All_Objects, file_path_name);
    file_path_name %>% 
        readLines %>%
        c("", "I.init();") %>%
        writeLines(file_path_name);
}

I.init();
