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

### GitHub Setup (Configured 2026-02-19)

**Repository**: `git@github.com:Ventura-Strats/Ventura.git`

**Branches**:
- `main` - Source code (R modules, Python scripts, Shiny dashboard)
- `signals` - Trading signals (timestamped CSV files for accountability)

**Local Repos**:
| Purpose | Local Path | Branch |
|---------|------------|--------|
| Code | `/home/fls/Models/Ventura/HD/` | main |
| Signals | `/home/fls/Data/Ventura/HD/Git/Ventura/` | signals |

**Authentication**: SSH key (no password prompts)
- Private key: `~/.ssh/id_ed25519`
- Public key: Added to https://github.com/settings/keys

**Signal Push Workflow**:
1. `Signal_List.py` generates CSV in `trades_new/YYYY-MM/YYYY-MM-DD/`
2. Calls `GitPushVentura.sh` (in `/usr/local/bin/`)
3. Script commits and pushes to `signals` branch

**Quick Reference**:
```bash
# Code repo - manual push
cd /home/fls/Models/Ventura/HD
git add -A && git commit -m "message" && git push

# Test SSH connection
ssh -T git@github.com

# If SSH key lost or new machine:
ssh-keygen -t ed25519 -C "ventura-trading"
# Then add ~/.ssh/id_ed25519.pub to https://github.com/settings/keys
```

**Note**: SSH keys don't expire. They work until deleted from GitHub or the local machine.

## Key Python Scripts (IB API Integration)

Located in `/HD/Scripts/Python/`:
- `Price_IB.py` - Fetch live prices from IB
- `Price_IB_Future.py` - Fetch futures prices
- `Price_IB_Exec.py` - Prices for execution
- `Read_Executions.py` - Read executed trades from IB
- `Execute_Orders.py` - Place orders
- `Signal_List.py` - Generate signal list for execution
- `IB_Account_Data.py` - Fetch account data/NAV

## Current Status (Updated 2026-02-19)

### Completed
- **GitHub setup**: Full SSH authentication configured, code on `main` branch, signals on `signals` branch, `GitPushVentura.sh` fixed
- **IB API error() fix**: Fixed `advancedOrderRejectJson` parameter issue in 10 Python files
- **init.py fix**: `scriptFinish()` now handles missing `script_id` gracefully
- **Read_Executions.py fix**: Fixed `formatExecutionTime()` to parse timezone dynamically from IB time string (was hardcoded to `Asia/Hong_Kong`, now handles any timezone like `Europe/London`)
- **DB.R pool fix**: Added `validationInterval = 30` to connection pool to prevent "Lost connection to server during query" errors by validating connections before use
- **Cross-asset correlation matrix**: New function `T.calcHistoricalCorrelationsMatrix()` for min-variance portfolio sizing (see Key Functions Reference)

### Issues Fixed (2026-01-04)
The IB API scripts were failing with: `error() missing 1 required positional argument: 'advancedOrderRejectJson'`

**Root cause**: The `error()` callback signature changed in newer IB API versions. The parameter was made optional with `advancedOrderRejectJson=""`.

**Files fixed**:
- IB_Account_Data.py, Read_Executions.py, Execute_Orders.py, Signal_List.py
- Price_IB.py, Get_Future_Expiries.py, do_the_trades.py
- ib_retrieve_live_price.py, ib_retrieve_live_price_from_both_books.py, ib_retrieve_contract_details.py
- init.py (scriptFinish crash fix)

### Remaining Issues

#### 1. "DB not writable" warning
- Scripts show `Good to start: False` and `DB not writable`
- Account 1 data still retrieves successfully despite this warning
- Account 2 fails (possibly separate IB connection issue)
- **Status**: User investigating

#### 2. Historical Data Corruption
- Machine downtime caused gaps in price history
- Some currencies have incorrect prices
- Current workaround: Manual download from investing.com, then `T.importInvestingComHistoFile()`
- Diagnostic: Run `T.plotPriceSeries()` across assets to identify issues

#### 3. Trade Execution Not Automated
- Need to write execution script using Python IB API
- Currently trades require manual intervention

#### 4. Trade Inventory Workflow (when working)
- `Read_Executions.py` pulls executions from IB
- `B.readTradesFromIB()` processes the data
- `B.createNewTradeIDFromLegs()` creates trade records manually

### Security Note
- DB.R (line 16) has database password in plaintext - should move to environment variable

## Priorities (Agreed Plan)

1. ~~**Backup to GitHub**~~ - DONE
2. ~~**Fix IB API error() signature**~~ - DONE
3. ~~**Test Read_Executions.py**~~ - DONE (fixed timezone parsing, tested with EUR.USD execution on account 2)
4. ~~**GitHub signal push**~~ - DONE (SSH auth, signals branch, GitPushVentura.sh fixed)
5. **Investigate "DB not writable"** - User investigating
5. **Fix historical data issues** - Improve data pipeline resilience
6. **Restructure codebase** - Add documentation, tests, logging improvements
7. **Future: Convert to R package** - Maybe, lower priority

### Completed: Eigenvalue-Based Portfolio Sizing (v2)
**Goal**: When multiple correlated signals fire together (e.g., "buy all indices" during a crash), recognize this is effectively 1-2 independent bets, not 50, and size accordingly.

**Implementation**:
- `T.calcHistoricalCorrelationsMatrix(instrument_ids, lookback_weeks, shrinkage, as_of_date)` - weekly return correlation matrix (uses Tuesday close to avoid Friday data release noise)
- `V.portfolioSizing(dat_signals, cor_matrix, risk_per_bet_pct, max_daily_risk_pct, aum_total, correlation_adjustment, min_weight)` - eigenvalue-based sizing with:
  - **Antagonist signal netting**: If same asset has buy+sell signals from different strategies, sums directions. Net > 0 → buy, net < 0 → sell, net = 0 → no trade. Prevents optimizer exploiting opposite signals as "free hedge".
  - Calculates N_effective (effective number of independent bets) from eigenvalues: N_eff = (Σλ)² / Σ(λ²)
  - Total risk = min(N_effective × risk_per_bet_pct, max_daily_risk_pct)
  - Trade correlation = direction_i × direction_j × asset_correlation
  - `correlation_adjustment` parameter: 0 = use historical, 0.2 = inflate correlations by 20%, -0.2 = reduce by 20%
  - Uses `quadprog::solve.QP` for weight distribution
- `G.Trades.Table.predict(dat_predict, aum_total, risk_per_bet_pct, max_daily_risk_pct, correlation_adjustment)` - shows sized notionals and N_Eff
- `G.Trades.Table.correlations(dat_predict)` - shows trade-adjusted correlation matrix in dashboard

**Example behavior**:
| Scenario | Signals | N_eff | risk/bet | Total Risk |
|----------|---------|-------|----------|------------|
| Crash (all indices) | 50 | ~2 | 0.5% | 1% |
| Mixed day | 4 | ~3.5 | 0.5% | 1.75% |
| Single signal | 1 | 1 | 0.5% | 0.5% |
| 20 uncorrelated | 20 | ~15 | 0.5% | 5% (capped) |

**Dashboard**: Trades tab shows Weight_Pct, Sized_Notional, and N_Eff columns, plus correlation matrix below

## Next Priorities

### 2. Automatic Trade Execution (Python)
- Execute trades automatically via Interactive Brokers API
- Python script to read signals and place orders
- Logic to be specified in detail before implementation
- Previous attempt exists but was written before user knew Python well

### 3. Trade Database Entry Automation
- After trades execute on IB, automatically log them to trade database
- Currently done manually - time consuming
- Should capture: entry time, exit time, targets, etc.
- Links to `Read_Executions.py` and `B.readTradesFromIB()` workflow

### 4. Python Codebase Cleanup
- All Python scripts (IB API integration) are poorly written legacy code
- Need complete refactoring into proper Python style
- To be done incrementally, with proper specs for each script

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
- `T.calcHistoricalCorrelationsMatrix(instrument_ids, lookback_weeks, shrinkage, as_of_date)` - Calculate weekly return correlation matrix for portfolio optimization. Defaults: 104 weeks lookback, 0.1 shrinkage toward identity, instruments with `use_for_trading=1`

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
