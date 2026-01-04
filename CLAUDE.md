# Ventura Trading Model - Project Context

## Overview

Ventura is a trading model system written primarily in R with Python scripts for Interactive Brokers API integration. It gathers historical prices, processes technical data, runs random forest prediction models, generates trading signals, and includes a Shiny dashboard for visualization.

The system runs distributed across multiple networked machines via crontab scheduling.

## Directory Structure

```
/home/fls/Models/Ventura/
├── HD/                          # NAS HDD mount
│   ├── Code/                    # Core R modules (10 files)
│   │   ├── Init.R               # Entry point, initialization, script execution framework
│   │   ├── Utils.R              # Utility functions (U.*)
│   │   ├── DB.R                 # Database connection and queries (D.*)
│   │   ├── Assets.R             # Asset/instrument handling (A.*)
│   │   ├── Book.R               # Trade book management (B.*)
│   │   ├── Communications.R     # Notifications/messaging (C.*)
│   │   ├── Engine.R             # Core engine logic (E.*)
│   │   ├── GUI.R                # GUI helpers (G.*)
│   │   ├── Technicals.R         # Technical indicators (T.*)
│   │   └── Ventura.R            # Model-specific logic (V.*)
│   ├── Platform/                # Shiny dashboard
│   │   ├── server.R
│   │   └── ui.R
│   ├── Scripts/                 # Scheduled jobs and utilities
│   │   ├── Model/               # Train.R, Predict.R, Backtesting.R, etc.
│   │   ├── Technicals/          # Price_Histo.R, Price_Live.R, Full.R, etc.
│   │   ├── Book/                # PnL_Github.R
│   │   ├── Communications/      # Send_Predict.R, Send_Signal.R, Send_Status.R
│   │   ├── Maintenance/         # Active_Future.R, Static_Tables.R, Load_Tech.R
│   │   ├── Scenario/            # Calc_Bumped.R, Predict_Bumped.R
│   │   ├── Crontab/             # Generate_Crontab.R
│   │   ├── Python/              # IB API scripts (see below)
│   │   └── zzz_Old/             # Archived/deprecated scripts
│   └── Data/                    # Data files (on NAS)
└── SD/                          # NAS SSD mount (faster storage)
```

## Module Naming Convention

Functions use a prefix indicating their module:
- `U.*` - Utils (general utilities, error handling, data manipulation)
- `D.*` - DB (database operations, SQL queries)
- `T.*` - Technicals (technical indicators, price processing)
- `B.*` - Book (trade management, positions, P&L)
- `I.*` - Init (initialization, script execution framework)
- `V.*` - Ventura (model-specific: training, prediction)
- `A.*` - Assets (instrument/asset management)
- `C.*` - Communications (notifications, alerts)
- `E.*` - Engine (core trading engine)
- `G.*` - GUI (Shiny/visualization helpers)

Each module has a `X.save()` function that dumps all module functions to its .R file.

## Script Execution Pattern

All scripts follow this pattern:
```r
start_time <<- Sys.time()
script_name <<- "ScriptName"
max_time_hours <<- 0.5

Script <- function() {
    # Main logic here
    I.completeStage(1)
    # More logic
    I.completeStage(2)
}

source("Init.R"); I.executeScript()
```

`I.executeScript()` handles:
- Database connectivity check
- Hard drive access verification
- Script status logging to `status_script` table
- Timeout management
- Error handling

## Coding Style

- **Heavy use of pipes** (`%>%`) - tidyverse style, this is standard modern R
- **Functional error handling** via `U.try()` which wraps `purrr::possibly()` - this is a valid pattern
- **Global variables** for configuration (e.g., `DIRECTORY_CODE_HD`, `TO_DAY`, `YESTERDAY`)
- **Tibbles** preferred over data.frames
- **Debug output** via `U.debug()` and `U.printBanner()`

## Infrastructure

### Database
- MySQL on 192.168.0.37
- Database name: `Ventura`
- Connection via `D.connect()` in DB.R
- Note: Credentials currently in plaintext in DB.R (needs securing)

### Network Machines
```
192.168.0.33  - Main (Big)
192.168.0.34  - Home (New)
192.168.0.37  - DB / VENTURA3
192.168.0.38  - VENTURA5
192.168.0.39  - VENTURA2
192.168.0.40  - VENTURA1
192.168.0.41  - VENTURA4
192.168.0.42  - IB (Interactive Brokers gateway)
192.168.0.43  - DB
192.168.0.44  - GLENORCHY
```

### Crontab
- Jobs distributed across machines
- Crontabs generated from central source via `Generate_Crontab.R`
- Helper scripts: `RScriptVentura.sh`, `PScriptVentura.sh`, `KillScriptVentura.sh`
- Time zone: London (Europe/London)

## Key Python Scripts (IB API Integration)

Located in `/HD/Scripts/Python/`:
- `Price_IB.py` - Fetch live prices from IB
- `Price_IB_Future.py` - Fetch futures prices
- `Price_IB_Exec.py` - Prices for execution
- `Read_Executions.py` - **BROKEN** - Read executed trades from IB
- `Execute_Orders.py` - Place orders
- `Signal_List.py` - Generate signal list for execution
- `IB_Account_Data.py` - Fetch account data/NAV

## Current Pain Points (as of Jan 2025)

### 1. Read_Executions.py - BROKEN
- Script to pull executed trades from IB API is not working
- This has stopped trade inventory maintenance
- Trading has been paused due to this
- **Status**: Investigating error (user to provide error message)

### 2. Historical Data Corruption
- Machine downtime caused gaps in price history
- Some currencies have incorrect prices
- Current workaround: Manual download from investing.com, then `T.importInvestingComHistoFile()`
- Diagnostic: Run `T.plotPriceSeries()` across assets to identify issues

### 3. Trade Execution Not Automated
- Need to write execution script using Python IB API
- Currently trades require manual intervention

### 4. Trade Inventory Workflow (when working)
- `Read_Executions.py` pulls executions from IB
- `B.readTradesFromIB()` processes the data
- `B.createNewTradeIDFromLegs()` creates trade records manually

## Priorities (Agreed Plan)

1. **Backup to GitHub** - Before any changes
2. **Fix Read_Executions.py** - Critical for resuming trading
3. **Fix historical data issues** - Improve data pipeline resilience
4. **Restructure codebase** - Add documentation, tests, logging improvements
5. **Future: Convert to R package** - Maybe, lower priority

## Key Functions Reference

### Trade Management
- `B.readTradesFromIB(account_id, query_type)` - Fetch trades from IB Flex Query
- `B.processTradesFromIB(just_today, save_to_db)` - Process and save trades
- `B.createNewTradeIDFromLegs(trade_date, strategy_id, tp_pct, leg_id_list)` - Create trade record
- `B.closeTradeFromLegs(trade_id, exit_type, exit_date, leg_id_list)` - Close trade
- `B.readTradesFromDB()` - Load all trades from database

### Technical Analysis
- `T.plotPriceSeries()` - Diagnostic plot for price data
- `T.importInvestingComHistoFile()` - Import historical data from investing.com CSV
- `T.addAllSplines()` - Add spline-based technical indicators

### Utilities
- `U.try(f, default)` - Wrap function with error handling (returns default on error)
- `U.debug(data, title)` - Debug output (prints when `print_diagnostics` is TRUE)
- `U.printBanner(message, big)` - Formatted console output

## Notes for Future Sessions

- Project root: `/home/fls/Models/Ventura/`
- Working directory for R: `/home/fls/Models/Ventura/HD/Code/`
- Only look at .R files in Code/ (ignore other file types)
- The Shiny dashboard works well - low priority for changes
- Owner is self-taught in R, professional Python developer
