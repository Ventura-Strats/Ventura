U.addItemToList <-
function (this_list, item_value, item_name) 
  {
    if (!U.variableExists(this_list)) 
      this_list <- {};
    
    if (is.null(this_list)) 
      this_list <- {};
    
    this_list$tmp_new_item <- item_value;
    
    position_name <- which(names(this_list) == "tmp_new_item");
    names(this_list)[position_name] <- item_name;
    
    this_list;
  }
U.anti_join <-
function (tbl_left, tbl_right, by = NULL) 
  {
    if (is.null(tbl_left) | is.null(tbl_right))
      tbl_left
    else
      anti_join(tbl_left, tbl_right, by)
  }
U.backupDirectory <-
function (dirFrom, dirTo) {
    # Replicates the exact contents of directory 1 into directory 2
    # Both directories have to already exist.
    
    library(tools)
    
    res <- {};
    if (U.right(dirFrom,1) != "/") dirFrom <- paste0(dirFrom, "/"); # Just in case
    if (U.right(dirTo,1) != "/") dirFrom <- paste0(dirTo, "/"); # Just in case
    
    ####################################################################################################
    ### Step 1: Replicate arborescence
    ####################################################################################################
    print(paste0("Replicate arborescence - ", Sys.time()))
    dir_from <- list.dirs(dirFrom, full.names = FALSE, recursive = TRUE);
    dir_from <- dir_from[which(dir_from != "")];
    
    dir_to <- list.dirs(dirTo, full.names = FALSE, recursive = TRUE);
    dir_to <- dir_to[which(dir_to != "")];
    
    dir_matched <- intersect(dir_from, dir_to);
    dir_only_from <- setdiff(dir_from, dir_to);
    dir_only_to <- setdiff(dir_to, dir_from);
    
    # We erase all directories that are only in destination.
    if (length(dir_only_to) > 0) 
      unlink(paste0(dirTo, dir_only_to[order(dir_only_to)]), recursive = FALSE);
    
    # We create the new folders that are only in the origin
    if (length(dir_only_from) > 0) 
      for (this_dir in dir_only_from[order(dir_only_from)]) 
        dir.create(paste0(dirTo, this_dir), showWarnings = FALSE);
    
    res$dir <- {}; 
    res$dir$dir_from <- dir_from; 
    res$dir$dir_to <- dir_to; 
    res$dir$dir_matched <- dir_matched; 
    res$dir$dir_only_from <- dir_only_from; 
    res$dir$dir_only_do <- dir_only_to;
    
    ####################################################################################################
    ### Step 2: Replicate files
    ####################################################################################################
    print(paste0("Replicate files - ", Sys.time()))
    print(paste0("Preparing Files From - ", Sys.time()))
    file_from <- list.files(dirFrom, include.dirs = FALSE, recursive = TRUE);
    file_from <- if (length(file_from) == 0) 
      NULL 
    else 
      data.frame(file = file_from, stringsAsFactors = FALSE);
    
    if (!is.null(file_from)) 
      file_from$checksum <- md5sum(paste0(dirFrom, file_from$file));
    
    print(paste0("Preparing Files To - ", Sys.time()))
    file_to <- list.files(dirTo, include.dirs = FALSE, recursive = TRUE);
    file_to <- if (length(file_to) == 0)
      NULL 
    else 
      data.frame(file = file_to, stringsAsFactors = FALSE);
    
    if (!is.null(file_to)) 
      file_to$checksum <- md5sum(paste0(dirFrom, file_to$file));
    
    print(paste0("Matching files - ", Sys.time()))
    # Step 1:  Matching files
    file_identical <- if (!is.null(file_from) & !is.null(file_to)) 
      semi_join(file_from, file_to, by = c("file", "checksum")) 
    else 
      NULL;
    
    print(paste0("Removing Extra files in To - ", Sys.time()))
    # Step 2: Files that are only in destination directory => We'll delete them
    file_to_only <- if(is.null(file_to)) 
      NULL 
    else if (is.null(file_from)) 
      file_to 
    else 
      anti_join(file_to, file_from, by = c("file", "checksum"));
    if (!is.null(file_to_only)) {
      if (nrow(file_to_only) == 0) 
        file_to_only <- NULL 
      else 
        unlink(paste0(dirTo, file_to_only$file));
    }
    
    
    print(paste0("Copying New files - ", Sys.time()))
    # Step 3: Files that are only in origin directory => We'll copy them
    file_from_only <- if(is.null(file_from)) 
      NULL 
    else if (is.null(file_to)) 
      file_from 
    else 
      anti_join(file_from, file_to, by = c("file", "checksum"));
    
    if (!is.null(file_from_only)) {
      if (nrow(file_from_only) == 0) 
        file_from_only <- NULL 
      else 
        file.copy(
          from = paste0(dirFrom, file_from_only$file), 
          to = paste0(dirTo, file_from_only$file)
        );
    }
    
    
    res$file <- {}; 
    res$file$file_from <- file_from; 
    res$file$file_to <- file_to; 
    res$file$file_identical <- file_identical; 
    res$file$file_only_from <- file_from_only; 
    res$file$file_only_to <- file_to_only;
    
    print(paste0("Done - ", Sys.time()))
    res;
    
  }
U.businessDayFromTime <-
function (time_list) 
  {
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    SWITCH_NEXT_DAY_NY_TIME <<- 17
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    calcBusinessDay_Try <- function(this_time) {
        this_time_ny <- as.POSIXct(this_time, tz = TZ_AMERICA)
        hour_time <- hour(this_time)
        if (hour_time >= SWITCH_NEXT_DAY_NY_TIME) {
            this_time <- U.calcNextDay(this_time)
        }
        format(this_time, tz = TZ_LOCAL)
    }
    calcBusinessDay <- function(this_time)
      U.try(calcBusinessDay_Try, TIME_NA)(this_time)
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    time_list %>%
      U.sapply(calcBusinessDay) %>%
      as.POSIXct(tz = TZ_LOCAL)
  }
U.calcNextDay <-
function (date_list) 
  {    
    ####################################################################################################
    ### Script
    ####################################################################################################
    weekday_list <- weekdays(date_list)
    
    add_days <- rep(1, length(date_list))
    add_days[weekday_list == "Friday"] <- 3
    add_days[weekday_list == "Saturday"] <- 2
    
    date_list %m+% days(add_days)
  }
U.calcPreviousDay <-
function (date_list) 
  {    
    ####################################################################################################
    ### Script
    ####################################################################################################
    weekday_list <- weekdays(date_list)
    
    substract_days <- rep(1, length(date_list))
    substract_days[weekday_list == "Monday"] <- 3
    substract_days[weekday_list == "Sunday"] <- 2
    
    date_list %m+% days(-substract_days)
  }
U.convertDateTimeInSQLTable <-
function (tbl_name, field_name, tz_from, tz_to) 
{
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    U.printBanner("Starting")
    U.printBanner("Download table", FALSE)
    dat_backup <- "SELECT * FROM %s" %>% sprintf(tbl_name) %>% D.SQL
    
    key_fields <- "SHOW KEYS FROM %s WHERE Key_name = 'PRIMARY'" %>% 
        sprintf(tbl_name) %>%
        D.SQL %>% 
        select(Column_name) %>% 
        U.vectorize %>% 
        U.debug("Key fields")
    
    U.printBanner("Preparing new table", FALSE)
    dat <- dat_backup
    dat$zzz_new_col <- dat[[field_name]]
    dat <- dat %>%
        mutate(zzz_new_col = as.POSIXct(zzz_new_col, tz=tz_from)) %>% 
        mutate(zzz_new_col = format(zzz_new_col, tz=tz_to))
    
    tmp_tbl_name <- D.createNewTableLikeExisting(tbl_name) %>% U.debug("tmp name")
    
    "ALTER TABLE %s ADD COLUMN zzz_new_col DATETIME NULL;" %>%
        sprintf(tmp_tbl_name) %>% 
        D.SQL
    
    cols_keep <- c(key_fields, field_name, "zzz_new_col") %>% U.vectorizeUnique 
    cols_drop <- setdiff(colnames(dat), cols_keep) %>% U.vectorizeUnique
    
    dat <- dat %>% select(cols_keep)
    
    dropColumn <- function(col_name) {
        "ALTER TABLE %s DROP COLUMN %s;" %>%
            sprintf(tmp_tbl_name, col_name) %>%
            D.SQL
    }
    lapply(cols_drop, dropColumn)
    U.printBanner("Inserting new data", FALSE)
    print(head(dat))
    D.insertDataIntoTable(tmp_tbl_name, dat, FALSE)

    str_join <- "A.%s = B.%s" %>%
        sprintf(key_fields, key_fields) %>%
        paste(collapse = " AND ") %>%
        paste0("(", ., ")")
    
    U.printBanner("Update field now", FALSE)
    "UPDATE %s A
        INNER JOIN %s B ON %s
        SET A.%s = B.%s" %>%
        sprintf(tbl_name, tmp_tbl_name, str_join, field_name, "zzz_new_col") %>%
        D.SQL
    U.printBanner("Update done", FALSE)
    U.printBanner("Drop tmp table", FALSE)
    D.dropTable(tmp_tbl_name)
    
    dat_new <- "SELECT * FROM %s" %>% sprintf(tbl_name) %>% D.SQL
    U.printBanner("Before:", FALSE)
    print(head(dat_backup))
    U.printBanner("After:", FALSE)
    print(head(dat_new))
    U.printBanner("All Done")
    list(backup = dat_backup, new = dat_new, tmp = dat)
}
U.data2Tibble <-
function (dat) as_tibble(U.unlistDataFrame(dat))
U.dataFrame <-
function(D) U.unlistDataFrame(data.frame(D, stringsAsFactors = FALSE))
U.dateTime <-
function (this_time, TO_DAY, time_zone = TZ_LOCAL) as.POSIXct(paste0(TO_DAY, " ", this_time), tz = time_zone)
U.dateTimeFormatForExport <-
function () format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss")
U.dateToday <-
function () 
{
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    calcDayBasedOnNYClose <- function() {
        time_now <- Sys.time()
        date_today <- as.Date(format(time_now, tz = TZ_AMERICA))
        hour_in_new_york <- as.integer(format(time_now, "%H", tz = TZ_AMERICA))
        if (hour_in_new_york >= SWITCH_NEXT_DAY_NY_TIME) {
            date_today <- date_today + 1
        }
        date_today
    }
    
    adjustDayIfWeekEnd <- function(date_today) {
        this_weekday <- weekdays(date_today)
        add_days <- switch(this_weekday, "Saturday" = 2, "Sunday" = 1, 0)
        date_today + add_days
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    calcDayBasedOnNYClose() %>%
        adjustDayIfWeekEnd
}
U.debug <-
function(this_data, this_title = NULL)
  {
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    printItem_Try <- function(this_item) {
      this_class <- class(this_item)[1]
      if (this_class == "list")
        lapply(this_item, printItem)
      else if (this_class %in% c("tbl_df", "tbl", "data.frame", "xts"))
        U.printHeadTail(this_item)
      else
        print(this_item)
    }
    printItem <- function(this_item) {
      if (print_diagnostics)
        U.tryNull(printItem_Try, this_item)
    }
    
    printDiagnostics_Try <- function() {
      if (is.null(this_title)) 
        U.printDiagnostics(this_data)
      else {
        U.printDiagnostics(this_title)
        printItem(this_data)
      }
    }
    printDiagnostics <- function()
      U.try(printDiagnostics_Try)()
    ####################################################################################################
    ### Script
    ####################################################################################################
    force(this_data)
    printDiagnostics()
    this_data
  }
U.dfContainsData <-
function (this_df) 
  {
    testDF <- function(this_df) {
      if (!U.variableExists(this_df)) 
        FALSE
      else {
        if (is.null(this_df))
          FALSE
        else 
          (nrow(this_df) > 0)
      }
    }
    U.try(testDF, FALSE)(this_df)
    
  }
U.dfNoRows2Null <-
function(this_df) {
    res <- NULL;
    if (!is.null(this_df))
      if (nrow(this_df) > 0)
        res <- this_df;
      res;
  }
U.dfReplaceNAColumnsWithZero <-
function(this_df, col_names) {
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    replaceNAs <- function(dat) {
      for (this_col in col_names) {
        this_vector <- U.vectorize(dat[this_col])
        this_vector[which(is.na(this_vector))] <- 0
        dat[this_col] <- this_vector
      }
      dat
    }
    ####################################################################################################
    ### Script
    ####################################################################################################
    U.try(replaceNAs, this_df)(this_df)
  }
U.diagEnd <-
function (this_data, text_diagnostic) U.debug(this_data, paste0(text_diagnostic, " - Done"))
U.diagStart <-
function (this_data, text_diagnostic) U.debug(this_data, paste0(text_diagnostic, " - Starting"))
U.distributionFit <-
function (x) 
  {
    x <- x[which(!is.nan(x))];
    x <- x[which(!is.infinite(x))];
    x <- x[which(!is.na(x))];
    H <- x %>% hist(breaks = 50, freq = FALSE, plot = FALSE);
    Y <- data.frame(x = H$mids, y = H$density, z = 0);
    
    dist_distance <- function(X) {     
      Y$z <- dstable(Y$x, X[1], X[2], X[3], X[4]);
      sum((Y$z - Y$y)^2)
    }
    
    alpha_0 <- 1.3;
    beta_0 <- 0.25;
    gamma_0 <- sd(x, na.rm = TRUE) / sqrt(2);
    mu_0 <- median(x, na.rm = TRUE);
    
    lower_bounds <- c(0.6, -0.75, 0.001, -0.1)
    upper_bounds <- c(1.75, 0.75, 20, 0.1)
    
    X <- c(alpha_0, beta_0, gamma_0, mu_0);
    
    data.frame(param = c("alpha", "beta", "gamma", "mu"), value = optim(X, dist_distance, control = list(maxit = 500), upper = upper_bounds, lower = lower_bounds)$par);
    
  }
U.doGreeks <-
function () datGreeks <<- c(
    lambda = "\u03BB", 
    mu = "\u03BC", 
    rho = "\u03C1", 
    sigma = "\u03C3", 
    tau = "\u03C4", 
    phi = "\u03C6", 
    psi = "\u03C8", 
    xi = "\u03BE", 
    omega = "\u03C9", 
    Delta = "\u0394"
  )
U.encodeString <-
function (string_to_encode, password) 
  {
    ####################################################################################################
    ### Script parameters and variables
    ####################################################################################################
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    keep8Decimals <- function(x) {
      x <- sqrt(x);
      floor(1e8 * (x - floor(x)))
    }
    
    generateRandomSeed <- function() {
      password %>% 
        strsplit("") %>% 
        U.vectorize %>% 
        U.sapply(utf8ToInt) %>% 
        as.character %>% 
        paste(collapse = "") %>% 
        as.double %>% 
        keep8Decimals %>% 
        as.integer;
    }
    
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    generateRandomSeed()
  }
U.findOrReplaceTextInAllCode <-
function (text_from, text_to = "", find_or_replace = "find", do_backup = FALSE) 
  {
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    file_path_list <- c("Code", "Platform", "Scripts") %>% 
        paste0(DIRECTORY_CODE_HD, "", ., "/")
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    listFilesInDirectory <- function(this_dir) {
        files <- list.files(this_dir, recursive = TRUE)
        files[which(U.right(files, 2) == ".R")]
    }
    
    findOrReplaceInFile <- function(file_path_name) {
      file_content <- readLines(file_path_name)
      new_content <- gsub(text_from, text_to, file_content, fixed = TRUE)
      if (any(file_content != new_content)){
        switch(
          find_or_replace,
          "find" = file_path_name,
          "replace" = {
            if (do_backup) {
              timestamp_str <- paste0("backup_", Sys.time(), ".R")
              backup_file_name <- gsub(".R", timestamp_str, file_path_name)
              file.copy(file_path_name, backup_file_name)
            }
            writeLines(new_content, con = file_path_name)
            NULL;
          }
        )
      } else NULL
    }
    
    findORreplaceFilesInDirectory <- function(this_dir) {
      file_names <- listFilesInDirectory(this_dir) %>% 
        U.noData2Null;
      res <- NULL
      if (!is.null(file_names)) {
        file_names <- paste0(this_dir, file_names)
        res <- lapply(file_names, findOrReplaceInFile)
        if (find_or_replace == "find")
          res <- do.call(c, res)
      }
      res;
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    res <- lapply(file_path_list, findORreplaceFilesInDirectory)
    res <- do.call(c, res)
    source("Utils.R")
    res
  }
U.formatPrice <-
function (px) 
  {
    (10000 / px) %>%
      log10 %>%
      pmax(0) %>%
      round(0) %>%
      '+'(1) %>% 
      strrep("0", .) %>%
      paste0("#,###.", .)
  }
U.functionNull <-
function (FUN) 
  { 
    function_Try <- function(X) FUN(X)
    function(X) U.tryNull(function_Try, X)
  }
U.genPassword <-
function(nchar = 50, specialChar = TRUE) {
    
    ####################################################################################################
    ### Script parameters and variables
    ####################################################################################################
    nchar <- max(c(6, nchar));
    lower_case_list <- 97:122;
    upper_case_list <- 65:90;
    digit_list <- 48:57;
    special_list <- c(33, 35, 36, 38, 42);
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    genAvailableCharacterIdList <- function() {
      character_id_list <- c(lower_case_list, upper_case_list, digit_list);
      if (specialChar) {
        character_id_list <- c(character_id_list, special_list);
      }
      character_id_list;
    }
    
    genRandomCharacterIdList <- function() {
      character_id_list <- genAvailableCharacterIdList();
      
      if (specialChar) { 
        c(
          sample(lower_case_list, 1), 
          sample(upper_case_list, 1), 
          sample(digit_list, 1), 
          sample(special_list, 1),
          sample(character_id_list, nchar - 4)
        );
      }
      else {
        c(
          sample(lower_case_list, 1), 
          sample(upper_case_list, 1), 
          sample(digit_list, 1), 
          sample(character_id_list, nchar - 3)
        );
      }
    }
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    genRandomCharacterIdList() %>%
      U.scrambleVector %>%
      intToUtf8;
  }
U.genSeedFromPassword <-
function (string_to_encode, password) 
  {
    ####################################################################################################
    ### Script parameters and variables
    ####################################################################################################
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    
    keep8Decimals <- function(x) {
      x <- sqrt(x);
      floor(1e8 * (x - floor(x)))
    }
    
    generateRandomSeed <- function() {
      password %>% 
        strsplit("") %>% 
        U.vectorize %>% 
        U.sapply(utf8ToInt) %>% 
        as.character %>% 
        paste(collapse = "") %>% 
        as.double %>% 
        keep8Decimals %>% 
        as.integer;
    }
    
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    generateRandomSeed()
  }
U.getIPAddress <-
function() {
    ip_info <- system("ip address", intern=TRUE);
    ip_info <- ip_info[grepl("scope global dynamic eno1", ip_info)];
    ip_info <- substr(ip_info, gregexpr("inet", ip_info)[[1]] + 5, nchar(ip_info));
    ip_info <- substr(ip_info, 1, gregexpr("/", ip_info)[[1]]-1);
    ip_info;
  }
U.harmonicMean <-
function(x) {
    1/mean(1/x, na.rm = TRUE)
  }
U.headTail <-
function (this_df) 
  {
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    ncol_switch_1 <- 10;
    nrow_switch_2 <- 20;
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    if (U.dfContainsData(this_df)) {
      if ((nrow(this_df) > nrow_switch_2) | (ncol(this_df) > ncol_switch_1)) {
        this_df <- data.frame(this_df)
        rbind(head(this_df), tail(this_df))
      } else {
        data.frame(this_df);    
      }
    }
  }
U.hourTime <-
function (time_now = NULL) {
    time_used <- U.ifelse(is.null(time_now), Sys.time(), time_now);
    as.integer(format(time_used, "%H"))
  }
U.ifColumnDoesntExistAddNAColumn <-
function (this_df, col_names, force_type = NULL) 
  {
    res <- this_df;
    for (this_col_name in col_names) {
      if (!U.isColumnInDataFrame(res, this_col_name)) {
        res[this_col_name] <- 
          if (is.null(force_type)) 
            NA 
        else {
          switch(force_type,
                 "numeric" = NUM_NA,
                 "character" = CHAR_NA,
                 "date" = DATE_NA,
                 "integer" = INT_NA,
                 NA
          )
        }
      }
    }
    res;
  }
U.ifelse <-
function (if_condition, do_if_true, do_else) 
  {
    if (if_condition) do_if_true else do_else
  }
U.isColumnInDataFrame <-
function (this_df, col_names) 
  {
    df_col_names <- colnames(this_df)
    
    testEach <- function(this_col_name) 
      this_col_name %in% df_col_names;
    
    col_names %>%
      U.sapply(testEach)
  }
U.left <-
function(this_string, n_char) {
    this_string <- this_string %>% U.vectorize %>% as.character;
    len_substr <- n_char + U.ifelse(n_char >= 0, 0, nchar(this_string));
    substr(this_string, 1, len_substr);
  }
U.left_join <-
function (tbl_left, tbl_right, by = NULL) 
  {
    if (is.null(tbl_left) | is.null(tbl_right))
      tbl_left
    else 
      left_join(tbl_left, tbl_right, by)
    
  }
U.mode <-
function (this_vector) 
  {
    # Gets the mode of a vector
    this_vector <- U.vectorize(this_vector);
    
    unique_values <- unique(this_vector);
    unique_values[which.max(tabulate(match(this_vector, unique_values)))];
  }
U.mround <-
function (x, base) base * round(x / base, 0)
U.NaN2NA <-
function(dat) {
    for (j in 1:ncol(dat)) dat[,j][which(is.nan(dat[,j]))] <- NA;
    dat
  }
U.newton1DSolver <-
function (target_function, starting_point, solver_tolerance = 1e-12, dx = 1e-5, max_iter = 100) 
  {
    ####################################################################################################
    ### Script variables
    ####################################################################################################
    
    ####################################################################################################
    ### Sub routines
    ####################################################################################################
    continueLoop <- function(f_x, i) {
      (abs(f_x) > solver_tolerance) & (i <= max_iter)
    }
    
    solverAlgo_Try <- function() {
      x <- starting_point;
      current_value <- target_function(x);
      current_iter <- 1;
      continue_loop <- continueLoop(current_value, current_iter);
      while (continue_loop) {
        current_derivative <- (target_function(x + dx) - current_value) / dx;
        x <- x - current_value / current_derivative;
        current_value <- target_function(x);
        current_iter <- current_iter + 1;
        continue_loop <- continueLoop(current_value, current_iter);
      }
      
      if (continue_loop) {
        x <- NUM_NA
      }
      x;
    }
    solverAlgo <- function()
      U.try(solverAlgo_Try, NUM_NA)()
    
    ####################################################################################################
    ### Script 
    ####################################################################################################
    solverAlgo();
  }
U.noData <-
structure(list(No_Data = "No Data"), row.names = c(NA, -1L), class = "data.frame")
U.noData2Null <-
function(dat) {
    if (is.null(dat))
      NULL
    else if (is.null(nrow(dat))) {
      if (length(dat) == 0)
        NULL
      else
        dat
    }
    else if (nrow(dat) == 0) 
      NULL 
    else dat
  }
U.printBanner <-
function (doing_what, big_banner = TRUE) 
  {
    banner_size <- 100;
    
    banner <- paste(rep("#", banner_size), collapse = "");
    time_now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S");
    size_timestamp <- nchar(time_now);
    
    char_used <- nchar(doing_what) + 9 + size_timestamp;
    
    middle_line <- sprintf("### %s - %s", doing_what, time_now);
    
    if (char_used <= banner_size) {
      fill_blanks <- paste(rep(" ", banner_size - char_used), collapse = "");
      middle_line <- sprintf("### %s%s %s ###", doing_what, fill_blanks, time_now);
    }
    
    if (big_banner) message(banner);
    message(middle_line);
    if (big_banner) message(banner)
  }
U.printDiagnostics <-
function (diagnostic_list)
  {
    if (print_diagnostics) {
      U.printBanner(diagnostic_list[[1]]);
      if (length(diagnostic_list) > 1)
        lapply(diagnostic_list[-1], print)
    }
    
  }
U.printHeadTail <-
function (this_df) 
  {
    ####################################################################################################
    ### Script Variables
    ####################################################################################################
    ncol_switch_1 <- 20;
    nrow_switch_2 <- 30;
    
    ####################################################################################################
    ### Script
    ####################################################################################################
    
    if (U.dfContainsData(this_df)) {
      if ((nrow(this_df) > nrow_switch_2) | (ncol(this_df) > ncol_switch_1)) {
        this_df <- data.frame(this_df)
        print(head(this_df))
        print(tail(this_df))
      } else {
        print(data.frame(this_df));    
      }
    }
  }
U.printMilestone <-
function(this_data, milestone = "")
  {
    force(this_data)
    U.printBanner(milestone, FALSE)
    this_data
  }
U.printTickerProgressVerbose <-
function (this_item, item_list) {
    item_position <- which(item_list == this_item);
    progress_status <- round(100 * item_position / length(item_list), 2);
    
    "doing %s - %s%%" %>% 
      sprintf(this_item, progress_status) %>%
      U.printBanner(FALSE);
  }
U.read.csv <-
function(file_name, data2Tibble = TRUE, noRowsNull = TRUE) {
    res <- read.csv(file_name, stringsAsFactors = FALSE);
    if (data2Tibble) res <- U.data2Tibble(res);
    if (noRowsNull) res <- U.dfNoRows2Null(res);
    res;
  }
U.readGoogleSheet <-
function (url_googlesheet, data2Tibble = TRUE, noRowsNull = TRUE) 
  {
    
    # On 2020-11-18 the read.csv stopped working so I have to do this
    ####################################################################################################
    ### Script 
    #################################################################################################### 
    
    dat <- url_googlesheet %>% 
      read_html %>% 
      html_table %>% 
      .[[1]] 
    
    colnames(dat) <- dat[1, ]
    dat <- dat[-1, -1]
    rownames(dat) <- NULL
    dat <- dat %>% 
      data.frame
    
    if (all(is.na(dat[1,]) | dat[1,] == "")) {
      dat <- dat[-1, ]
    }
    
    if (data2Tibble) dat <- U.data2Tibble(dat)
    if (noRowsNull) dat <- U.dfNoRows2Null(dat)
    
    dat
  }
U.readIPAddress <-
function() "http://ipv4.icanhazip.com/" %>% read_html %>% html_text %>% gsub("\n", "", ., fixed=TRUE)
U.removeCommas2Num <-
function (this_vector) 
  {
    this_vector <- U.noData2Null(U.vectorize(this_vector));
    
    if (is.null(this_vector))
      NULL
    else 
      as.numeric(gsub(",", "", this_vector))
  }
U.removeWeekEnds <-
function (this_df) 
  {
    this_df %>%
      mutate(tmp_weekday = weekdays(date)) %>% 
      filter(tmp_weekday != "Saturday", tmp_weekday != "Sunday") %>%
      select(-tmp_weekday)
    
  }
U.replaceNaNWithNA <-
function(this_vector) {
    this_vector <- U.vectorize(this_vector);
    if (is.null(this_vector))
      NULL
    else if (length(this_vector) == 0)
      NULL
    else {
      this_vector[which(is.infinite(this_vector))] <- NUM_NA;
      this_vector[which(is.nan(this_vector))] <- NUM_NA;
    }
    this_vector;
  }
U.replaceNAWithZero <-
function(this_vector) {
    dat <- U.vectorize(this_vector);
    dat[which(is.na(dat))] <- 0;
    dat;
  }
U.right <-
function(this_string, N) {
    this_string <- as.character(U.vectorize(this_string));
    len_string <- nchar(this_string);
    substr(this_string, len_string - N + 1, len_string);
  }
U.rot <-
function (x) 
  {
    res <- try(c(tail(x, 1), head(x, -1)), TRUE);
    if (class(res) == "try-error") res <- NA;
    res;
  }
U.sampleDataFrame <-
function () 
  {
    data.frame(
      c1 = c(1, 2, 3, NUM_NA, 5),
      c2 = c("A", CHAR_NA, "C", "D", "E"),
      c3 = 6:10,
      c4 = c("A", "B", "C", "D", "E")
    ) %>% U.dataFrame;
  }
U.sapply <-
function(X, FUN) {
    U.vectorize(sapply(X, FUN, simplify = TRUE, USE.NAMES = FALSE))
  }
U.save <-
function() {
    All_Objects <- ls(".GlobalEnv", pattern = "U.");
    All_Objects <- All_Objects[substr(All_Objects,1,2) == "U."];
    dump(All_Objects, paste0(DIRECTORY_CODE_HD, "Code/Utils.R"));
  }
U.saveAll <-
function () 
  {
    A.save()
    B.save()
    C.save()
    D.save()
    E.save()
    G.save()
    I.save()
    T.save()
    U.save()
    V.save()
  }
U.scrambleVector <-
function(this_vector) {
    if (length(this_vector) == 0)
      NULL
    else 
      this_vector[sample(length(this_vector))]
  }
U.semi_join <-
function (tbl_left, tbl_right, by = NULL) 
  {
    if (is.null(tbl_right))
      tbl_left[0,]
    else if (is.null(tbl_left))
      tbl_left
    else
      anti_join(tbl_left, tbl_right, by)
  }
U.shuffleVector <-
function (x) 
{
    x[sample.int(length(x))]
}
U.sortVector <-
function(v) v[order(v)]
U.splitInBatch <-
function (main_list, batch_i, nb_batches) 
  {
    ####################################################################################################
    ### Script
    ####################################################################################################
    nb_items <- length(main_list)
    batch_size <- ceiling(nb_items / nb_batches)
    
    i0 <- (batch_i - 1)
    index_to_do <- (batch_size*i0) + 1:batch_size
    
    if (batch_i == nb_batches) {
      index_to_do <- (1 + batch_size*i0):nb_items
    }
    
    main_list[index_to_do] %>% na.omit %>% U.vectorize
  }
U.try <-
function (f, f_rtn = NULL) possibly(f, f_rtn, TRUE)
U.tryNull <-
function (FUN, fun_arg) 
  {
    if (!U.variableExists(fun_arg))
      NULL
    else if (is.null(fun_arg))
      NULL
    else
      U.try(FUN)(fun_arg)
  }
U.tryValueOrNA <-
function(this_statement) {
    res <- try(this_statement, TRUE);
    if (class(res) == "try-error") NA else res;
  }
U.unlistDataFrame <-
function(D) {
    res <- if (class(D)[1] != "data.frame") data.frame(D, stringsAsFactors = FALSE) else D;
    if (!is.null(res)) {
      if (prod(dim(res)) > 0) {
        for (j in 1:ncol(res)) {
          try_date <- try(as.Date(as.vector(unlist(as.character(res[,j])))), TRUE);
          is_date <- (class(try_date) != "try-error") & (colnames(res)[j] != "date_time");
          if (is_date) is_date <- any(!is.na(try_date)); # if they are all NAs then do not convert into date.
          if (
            (gsub("date", "", tolower(colnames(res)[j]), fixed = TRUE) != tolower(colnames(res)[j])) &
            (gsub("time", "", tolower(colnames(res)[j]), fixed = TRUE) == tolower(colnames(res)[j]))
          ) is_date <- TRUE;
          if (tolower(colnames(res)[j]) %in% c("month to date", "year to date")) is_date <- FALSE;
          res [,j] <- if (is_date) try_date else as.vector(unlist(res[,j]));
        }
      }
    }
    res;
  }
U.upperFirstLetter <-
function(this_string) {
    first_letter <- toupper(U.left(this_string, 1))
    rest_string <- substr(this_string, 2, nchar(this_string));
    paste0(first_letter, rest_string)
  }
U.variableExists <-
function (this_variable) 
  {
    this_test <- try(this_variable, TRUE)
    this_test <- !(class(this_test) == "try-error")
    this_test[1]
  }
U.vectorize <-
function (dat) as.vector(unlist(dat))
U.vectorizeUnique <-
function (dat, do_null = FALSE) {
    res <- unique(U.vectorize(dat));
    if (do_null) 
      if (length(res) == 0) 
        res <- NULL;
      res;
  }
U.write.csv <-
function(data_table, file_name) write.csv(data_table, file_name, row.names = FALSE)
U.yearFrac <-
function(start_date, end_date) {
    d1 <- as.Date(start_date);
    d2 <- as.Date(end_date);
    
    ytd <- function(D, origin=as.Date("1970-01-01")) {
      if (!inherits(D, "Date"))
        D <- as.Date(D, origin=origin)
      as.numeric(D - as.Date(format(D, "%Y-01-01"), origin=origin) + 1) / 365
    }
    
    if ((length(d1) == 1) & (length(d2) == 1)) {
      n_years <- as.numeric(substr(d2, 1, 4)) - as.numeric(substr(d1,1,4));
      ytd(d2) - ytd(d1) + n_years;
    }
    else if ((length(d1) == 1) & (length(d2) > 1)) {
      date_function <- function(d) U.yearFrac(d1, d);
      sapply(d2, date_function, USE.NAMES = FALSE);
    }
    else if ((length(d1) > 1) & (length(d2) == 1)) {
      date_function <- function(d) U.yearFrac(d, d2);
      sapply(d1, date_function, USE.NAMES = FALSE);
    }
    else if (length(d1) == length(d2)) {
      n_years <- as.numeric(substr(d2, 1, 4)) - as.numeric(substr(d1,1,4));
      U.sapply(d2, ytd) - U.sapply(d1, ytd) + n_years;
    }
    else NULL
  }
