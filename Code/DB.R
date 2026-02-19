D.connect <-
function (db_name = "Ventura") {

    ####################################################################################################
    ### Script Variables
    ####################################################################################################

    db_host <- "192.168.0.37";     #DB Machine
    db_host_local <- "127.0.0.1";   #localhost for remote connection

    if (THIS_COMPUTER == "L") {
        db_host <- db_host_local;
    }

    db_user <- "ventura"
    db_password <- "psuY2oF4qq7B$Lw8U!If"
    db_name <- "Ventura"

    ####################################################################################################
    ### Script sub functions
    ####################################################################################################

    poolIsValid <- function() {
        U.variableExists(dbCon) && inherits(dbCon, "Pool") && pool::dbIsValid(dbCon)
    }

    createPool <- function(host) {
        pool::dbPool(
            RMySQL::MySQL(),
            host = host,
            dbname = db_name,
            user = db_user,
            password = db_password,
            minSize = 1,
            maxSize = 1,
            idleTimeout = 3600,         # close idle connections after 1 hour
            validationInterval = 30     # validate connection every 30 seconds before use
        )
    }

    ####################################################################################################
    ### Script
    ####################################################################################################

    if (!poolIsValid()) {
        library(DBI)
        library(RMySQL)
        library(pool)

        dbCon <<- U.try(createPool, NULL)(db_host)

        if (is.null(dbCon)) {
            dbCon <<- createPool(db_host_local)
        }
    }
}
D.createNewTableLikeExisting <-
function (db_table_name) 
{
    tmp_tbl_name <- D.nameTmpTable()
    sql_create_new_tmp_table <- sprintf("CREATE TABLE %s LIKE %s", tmp_tbl_name, db_table_name)
    res <- U.try(D.SQL)(sql_create_new_tmp_table)
    if (is.null(res)) NULL else tmp_tbl_name
}
D.createStockIdTmpTable <-
function (this_df) 
{
    createTbl <- function(this_df) {
        stock_id_list <- this_df %>% select(stock_id) %>% U.dataFrame;
        tmp_tbl_name <- D.nameTmpTable();
        
        "CREATE TABLE %s 
        (stock_id MEDIUMINT UNSIGNED NOT NULL, 
        PRIMARY KEY (stock_id), 
        UNIQUE INDEX stock_id_UNIQUE (stock_id ASC), 
        CONSTRAINT FK_%s_stock_id FOREIGN KEY (stock_id) REFERENCES stocksStockIdList (stock_id) 
        ON DELETE NO ACTION ON UPDATE NO ACTION)" %>%
            sprintf(tmp_tbl_name, tmp_tbl_name) %>%
            D.SQL;
        
        D.writeTable(tmp_tbl_name, stock_id_list);
        tmp_tbl_name;
        
    }
    U.tryNull(createTbl, this_df);

}
D.disconnect <-
function () {
    if (U.variableExists(dbCon) && inherits(dbCon, "Pool") && pool::dbIsValid(dbCon)) {
        pool::poolClose(dbCon)
        rm(dbCon, envir = .GlobalEnv)
    }
}
D.dropTable <-
function (db_table_name) D.SQL(sprintf("DROP TABLE %s", db_table_name))
D.insertDataIntoTable <-
function (db_table_name, data_to_add, format_dataframe = TRUE) 
    D.insertOrReplaceDataIntoTable(db_table_name, data_to_add, "insert", format_dataframe)
D.insertOrReplaceDataIntoTable <-
function (db_table_name, data_to_add, insert_or_replace, format_dataframe = TRUE) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    insert_or_replace <- toupper(insert_or_replace);
    
    if (U.dfContainsData(data_to_add) & format_dataframe) {
        data_to_add <- data_to_add %>% 
            U.dataFrame %>% 
            U.dfNoRows2Null
    }
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    dbUpdate <- function(data_to_add) {
        tmp_tbl_name <- D.createNewTableLikeExisting(db_table_name)
        D.writeTable(tmp_tbl_name, data_to_add);
        dat <- try({
            "%s INTO %s SELECT * FROM %s" %>%
            sprintf(
                insert_or_replace, 
                db_table_name, 
                tmp_tbl_name
            ) %>%
            D.SQL }, 
            TRUE)
        D.dropTable(tmp_tbl_name)
        dat
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    U.tryNull(dbUpdate, data_to_add)
    
}
D.loadTable <-
function(db_table_name, data2Tibble = TRUE, noRowsNull = TRUE) D.select(sprintf("SELECT * FROM %s", db_table_name), data2Tibble, noRowsNull)
D.loadTableLocal <-
function(tbl_name) {
    ####################################################################################################
    ### Script
    ####################################################################################################
    if (!(tbl_name %in% c("INSTRUMENTS", "URL_INVESTING", "ETF"))) {
        tbl_name <- paste0("static_", tbl_name)
    }
    
    "%sTables_Local/%s.csv" %>%
        sprintf(DIRECTORY_DATA_HD, tbl_name) %>% 
        read.csv(stringsAsFactors = FALSE) %>%
        as_tibble
}
D.nameTmpTable <-
function () {
    script_name_part <- U.ifelse(
        U.variableExists(
            script_name), 
        U.right(script_name, 15), 
        "noscript"
        );
    
    time_part <- format(Sys.time(), "%m%d_%H%M")
    stage_part <- U.ifelse(U.variableExists(completed_stage), completed_stage, 1)
    
    "z%s_%s_%s" %>% 
        sprintf(time_part, U.genPassword(6, FALSE), script_name_part, stage_part)
}
D.replaceDataIntoTable <-
function (db_table_name, data_table, format_dataframe = TRUE) {
    print(data_table)
    D.insertOrReplaceDataIntoTable(db_table_name, data_table, "replace", format_dataframe)
    
}
D.save <-
function() {
    All_Objects <- ls(".GlobalEnv", pattern = "D.");
    All_Objects <- All_Objects[substr(All_Objects,1,2) == "D."];
    dump(All_Objects, paste0(DIRECTORY_CODE_HD, "Code/DB.R"));
}
D.select <-
function(sql_query, data2Tibble = TRUE, noRowsNull = TRUE) {
    res <- D.SQL(sql_query) %>% 
        U.dataFrame;
    
    if (data2Tibble) 
        res <- as_tibble(res);
    if (noRowsNull) 
        res <- U.dfNoRows2Null(res);
    
    res;
}
D.SQL <-
function (sqlQuery) dbGetQuery(dbCon, sqlQuery)
D.testWriteDB <-
function () 
{    
    ####################################################################################################
    ### Script description:
    ### Tests whether the database is writable by creating a small table
    ####################################################################################################
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    testTable_Try <- function() {
        tmp_tbl_name <- D.nameTmpTable();
        "CREATE TABLE %s (test_txt CHAR(1) NULL)" %>%
            sprintf(tmp_tbl_name) %>%
            D.SQL;
        tbl_content <- data.frame(test_txt = "A", stringsAsFactors = FALSE);
        dbWriteTable(
            dbCon, 
            tmp_tbl_name, 
            tbl_content, 
            overwrite = FALSE, 
            append = TRUE, 
            row.names = FALSE
            );
        tbl_res <- "SELECT * FROM %s" %>% 
            sprintf(tmp_tbl_name) %>% 
            D.SQL;
        D.dropTable(tmp_tbl_name);
        (nrow(tbl_res) == 1)
    }
    testTable <- function() 
        U.try(testTable_Try, FALSE)()
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    testTable();
}
D.truncateTable <-
function (db_table_name) D.SQL(sprintf("TRUNCATE TABLE %s", db_table_name))
D.waitTillPreviousJobHasFinished <-
function (previous_script_name, 
          last_completed_stage, 
          nb_jobs_total = 1, 
          max_lag_prices_minutes = 5,
          max_wait_time_before_continuing_minutes = 5
) 
{
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    wait_time_between_tests_seconds = 5
    
    SCRIPTS = D.loadTableLocal("script")
    script_id = filter(SCRIPTS, script == previous_script_name)$script_id
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    U.printBanner(sprintf("Waiting till %s has completed: ", previous_script_name))
    price_job_starttime_limit <- format(start_time - 60*max_lag_prices_minutes, "%Y-%m-%d %H:%M:%S") %>%
      as.character
    stop_waiting_time_limit <- start_time + 60*max_wait_time_before_continuing_minutes
    
    nb_jobs_done <- 0
    have_we_waited_too_long <- FALSE
    still_wait <- TRUE
    
    while (still_wait) {
        U.printBanner(sprintf("Waiting... jobs done: %s/%s", nb_jobs_done, nb_jobs_total), FALSE)
        print(1)
        Sys.sleep(wait_time_between_tests_seconds)
        print(2)
        nb_jobs_done <- 
            "SELECT *
            FROM status_script
            WHERE script_id = %s
            AND start >= '%s'
            AND last_completed_stage >= %s" %>%
            sprintf(
                script_id,
                price_job_starttime_limit,
                last_completed_stage
            ) %>% 
            D.SQL %>% 
            nrow
        print(3)
        have_we_waited_too_long <- (Sys.time() > stop_waiting_time_limit)
        print(4)
        still_wait <- ((nb_jobs_done < nb_jobs_total) & !have_we_waited_too_long)
        print(5)
    }
    print(6)
    U.printBanner(sprintf("Waiting finished... jobs done: %s/%s", nb_jobs_done, nb_jobs_total))
    print(7)
    if (have_we_waited_too_long) {
        U.printBanner("WARNING the previous job has not finished and we'll continue with what we have")
    }
    print(8)
}
D.writeTable <-
function(db_table_name, data_table) {
    writeTable_Try <- function() {
        dbWriteTable(dbCon, db_table_name, data_table, overwrite = FALSE, append = TRUE, row.names = FALSE)
    }
    U.try(writeTable_Try)();
}
