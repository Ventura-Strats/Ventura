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
- Credentials in environment variables (`VENTURA_DB_USER`, `VENTURA_DB_PASSWORD`, `VENTURA_DB_HOST`)

### Network Machines
```
192.168.0.34  - Home (dev machine, largest RAM)
192.168.0.37  - Database server (MySQL only, no code execution)
192.168.0.38  - VENTURA5 (processing)
192.168.0.39  - VENTURA2 (processing)
192.168.0.40  - VENTURA1 (processing)
192.168.0.41  - VENTURA4 (processing)
192.168.0.42  - IB (Interactive Brokers IB Gateway, ports 7497/7498 for accounts 1/2)
```

**Note**: Code runs from shared NAS drive. Git pull only needed on one machine.

### Machine Status Monitoring
Each machine runs a crontab shell script that appends timestamped system metrics to text files:
- **Data location**: `/home/fls/Data/Glenorchy/SD/Machine_Status/<computer>/`
- **Computer name**: Read from `/home/fls/Data/System/this_computer.txt`
- **Metrics collected**: `cpu_load.txt`, `memory_load.txt`, `swap_load.txt`, `heat_load.txt`, `ib_load.txt` (java process), `R_load.txt`, `disk_space.txt`, `system_load.txt`
- **Collection method**: `top -b -n 1`, `df`, `sensors`, `ps aux` piped through `ts` for timestamps
- **Dashboard**: Tab 6.1 "Machine Status" via `G.Diagnostic.MachineStatus.Plot.systemLoad(metric, sub_metric, nb_days)`
- **Status**: Work in progress, still has bugs

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
- `trade_orders.py` - Interactive order placement: exit orders (target+stop OCA), entry orders (chase algo)
- `order_execution.py` - Chase algorithm for entry order execution (used by trade_orders.py)

## Current Status (Updated 2026-03-18)

### Completed
- **GitHub setup**: Full SSH authentication configured, code on `main` branch, signals on `signals` branch, `GitPushVentura.sh` fixed
- **IB API error() fix**: Fixed `advancedOrderRejectJson` parameter issue in 10 Python files
- **init.py fix**: `scriptFinish()` now handles missing `script_id` gracefully
- **Read_Executions.py fix**: Fixed `formatExecutionTime()` to parse timezone dynamically from IB time string (was hardcoded to `Asia/Hong_Kong`, now handles any timezone like `Europe/London`)
- **DB.R pool fix**: Added `validationInterval = 30` to connection pool to prevent "Lost connection to server during query" errors by validating connections before use
- **Cross-asset correlation matrix**: New function `T.calcHistoricalCorrelationsMatrix()` for min-variance portfolio sizing (see Key Functions Reference)
- **Trade workflow automation** (2026-02-21): New functions `B.generateOrders()`, `B.matchLegsToTrades()`, `B.confirmLegMatch()`, `B.confirmSingleLeg()` for end-to-end trade lifecycle (see Session_Notes/2026-02-21_trade_workflow_automation.md)
- **Dashboard orders table** (2026-02-21): New `G.Trades.Table.orders()` displays execution orders in the Trades tab before they're sent to IB (see Session_Notes/2026-02-21_dashboard_orders_table.md)
- **Correlation floor at zero** (2026-03-03): `T.calcHistoricalCorrelationsMatrix()` has `floor_at_zero` parameter for display use. **Fixed (2026-03-10)**: floor moved from asset correlations to trade correlations inside `V.portfolioSizing()` - was incorrectly treating buy/sell on correlated assets as uncorrelated instead of hedged
- **G.Trades.Table.orders fix** (2026-03-03): Fixed `unused argument (default = NULL)` error - `U.try()` parameter is `f_rtn`, not `default`
- **IB Gateway move** (2026-03-03): IB API calls moved from machine H (34) to machine I (42) using IB Gateway. Ports: 7496 (account 1), 7497 (account 2). No code change needed
- **First live trades** (2026-03-02): 3 trades executed manually from signals. Manual workflow used pending full automation testing
- **Trade orders module** (2026-03-04): New `trade_orders.py` for placing exit orders (target LMT + stop STP as OCA pair) and entry orders (chase algo) interactively from Python console. See Session_Notes/2026-03-04_trade_orders_module.md
- **Trades tab restructure** (2026-03-06): Split into 4 sequential tables: Signals (with Execute checkbox) → Portfolio Sizing (new `G.Trades.Table.sizing()`, N_Eff as text header) → Correlation Matrix → Execution Orders. All downstream tables filter on tradable instruments and react to Execute checkbox. See Session_Notes/2026-03-06_trades_tab_restructure.md
- **Correlation floor fix & dedup** (2026-03-10): Fixed floor_at_zero bug (was applied to asset correlations before direction adjustment, now applied to trade correlations after). Deduplicated correlation matrix computation in dashboard via `Trades.Data.cor_matrix` reactive in server.R. See Session_Notes/2026-03-10_correlation_floor_fix.md
- **Machine status monitoring** (2026-03-15): Added dashboard tab 6.1 "Machine Status" with `G.Diagnostic.MachineStatus.Plot.systemLoad()`. Displays CPU, memory, swap, heat, IB process, R process, DB stats from text files collected by crontab shell script on each machine. Data stored in `/home/fls/Data/Glenorchy/SD/Machine_Status/<computer>/`. Still has bugs, work in progress. See Session_Notes/2026-03-15_machine_status_and_fixes.md
- **Tradable instruments cleanup** (2026-03-16): Replaced 4 hardcoded `tradable_instruments` lists with new `A.tradableInstruments()` function. Reads from INSTRUMENTS table filtering on `asset_class != "bond"`, `use_for_training == 1`, `use_for_trading == 1`, `use_for_trading_gs == 1`. Changed: `G.Trades.Table.correlations`, `G.Trades.Table.predict`, `G.Trades.Table.sizing` (GUI.R), `B.generateOrders` (Book.R). See Session_Notes/2026-03-16_tradable_instruments_cleanup.md
- **Backtest portfolio sizing** (2026-03-17): Replaced crude equal-risk sizing in `V.readBacktest` with eigenvalue-based `V.portfolioSizing`. Daily signals now go through antagonist netting, N_eff calculation, and min-variance optimization — same pipeline as live trading. Correlation matrix recomputed weekly (rolling). Removed per-strategy P&L tracking (total portfolio only). Added `n_effective` and `avg_n_eff` to diagnostics. See Session_Notes/2026-03-17_backtest_portfolio_sizing.md
- **V.portfolioSizing antagonist netting fix** (2026-03-18): Fixed NA propagation bug when strategies disagree on the same instrument (e.g., strategy 7 BUY + strategy 11 SELL). Netted-out instruments produced NA weights via `left_join`, crashing multi-strategy backtests. Fix: `replace_na()` after join gives cancelled instruments weight=0. Affects `V.portfolioSizing` (shared by backtest, dashboard, and order generation). See Session_Notes/2026-03-18_portfolio_sizing_netting_fix.md

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
- ~~DB.R (line 16) has database password in plaintext~~ - FIXED (2026-02-21)
- Credentials in environment variables: `VENTURA_DB_USER`, `VENTURA_DB_PASSWORD`, `VENTURA_DB_HOST`
- **Source files** (on shared NAS, gitignored):
  - `/home/fls/Models/Ventura/HD/.ventura_env` - sourced by wrapper scripts and `~/.bashrc`
  - `/home/fls/Models/Ventura/HD/Code/.Renviron` - for RStudio (dev machine only)
- Both R (`DB.R`) and Python (`db.py`) read from env vars

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
  - Trade correlation = direction_i × direction_j × asset_correlation, **floored at zero** (negative trade correlations would reduce perceived risk; floor is conservative)
  - `correlation_adjustment` parameter: 0 = use historical, 0.2 = inflate correlations by 20%, -0.2 = reduce by 20%
  - Uses `quadprog::solve.QP` for weight distribution
- `G.Trades.Table.predict(dat_predict)` - signal list with Execute checkbox (no sizing columns)
- `G.Trades.Table.sizing(dat_predict, aum_total, risk_per_bet_pct, max_daily_risk_pct, correlation_adjustment)` - eigenvalue-based sizing on selected trades, grouped by instrument after antagonist netting
- `G.Trades.Table.correlations(dat_predict)` - shows trade-adjusted correlation matrix in dashboard
- `G.Trades.Table.orders(dat_predict, risk_per_bet_pct, max_daily_risk_pct, correlation_adjustment, account_ids)` - shows execution orders from B.generateOrders() in dashboard Trades tab (all 21 columns, all accounts)

**Example behavior**:
| Scenario | Signals | N_eff | risk/bet | Total Risk |
|----------|---------|-------|----------|------------|
| Crash (all indices) | 50 | ~2 | 0.5% | 1% |
| Mixed day | 4 | ~3.5 | 0.5% | 1.75% |
| Single signal | 1 | 1 | 0.5% | 0.5% |
| 20 uncorrelated | 20 | ~15 | 0.5% | 5% (capped) |

**Dashboard**: Trades tab has 4 sections: Signals (with Execute checkbox) → Portfolio Sizing (N_Eff as text header, then table with Instrument, Direction, Weight_Pct, Sized_Notional) → Correlation Matrix → Execution Orders. Unticking a signal recalculates sizing, correlations, and orders for selected trades only. All sections filter on tradable instruments (currently hardcoded, to be moved to DB). Asset correlation matrix is computed once in `Trades.Data.cor_matrix` reactive and shared across all three downstream functions.

**Backtest integration** (2026-03-17): `V.readBacktest` now calls `V.portfolioSizing` for each day's new signals. Correlation matrix is recomputed weekly (rolling, cached by ISO week). Replaced crude `capital_allocation_ratio` with eigenvalue-based sizing. Per-strategy P&L tracking removed; only total portfolio P&L. Summary statistics include `avg_n_eff`. See Session_Notes/2026-03-17_backtest_portfolio_sizing.md

## Next Priorities

### 2. Automatic Trade Execution (Python) - PARTIALLY DONE
- **DONE**: `B.generateOrders()` converts signals to sized orders, exports CSV for Execute_Orders.py
- **DONE**: Uses V.portfolioSizing() for eigenvalue-based N_effective sizing
- **DONE**: Scales orders by account NAV from book_nav table
- **DONE**: `trade_orders.py` places exit orders (target+stop as OCA) and entry orders interactively
- **TODO**: Test `trade_orders.py` live with real orders (dry_run=False)
- **TODO**: Enable actual order placement in Execute_Orders.py for full automation (currently commented out)
- **TODO**: Test full flow: B.generateOrders() -> CSV -> Execute_Orders.py -> IB

### 3. Trade Database Entry Automation - DONE
- **DONE**: `B.matchLegsToTrades()` matches execution legs to trades (ENTRY/TARGET/STOP/MATURITY)
- **DONE**: `B.confirmLegMatch()` creates/closes trades after user review
- **DONE**: `B.confirmSingleLeg()` for manual single-leg confirmation
- Flow: Read_Executions.py -> book_trade_leg -> B.matchLegsToTrades() -> review -> B.confirmLegMatch()

### 4. Tradable Instruments List Cleanup - DONE
- **DONE**: New `A.tradableInstruments()` function in Assets.R reads from INSTRUMENTS table
- **DONE**: Replaced hardcoded lists in `G.Trades.Table.predict()`, `G.Trades.Table.sizing()`, `G.Trades.Table.correlations()` (GUI.R) and `B.generateOrders()` (Book.R)
- **DONE**: Uses existing DB flags: `use_for_training`, `use_for_trading`, `use_for_trading_gs` from `static_instrument`
- To change tradable instruments: update `use_for_trading_gs` in DB, re-run `Static_Tables.R`

### 5. Python Codebase Cleanup
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

### Order Generation & Execution Matching (NEW - 2026-02-20)
- `B.generateOrders(dat_predict, risk_per_bet_pct, max_daily_risk_pct, correlation_adjustment, account_ids, export_csv)` - Convert signals to sized orders for execution. Uses V.portfolioSizing() for eigenvalue-based N_effective sizing. Returns orders for multiple accounts scaled by NAV, exports CSV for Execute_Orders.py
- `B.matchLegsToTrades(lookback_days, price_tolerance_pct, timestamp_tolerance_minutes)` - Match execution legs to trades. Determines if each leg is ENTRY, TARGET, STOP, or MATURITY. Returns summary for manual review
- `B.confirmLegMatch(match_summary, confirm_types, dry_run)` - Execute matches after review. Creates trades via B.createNewTradeIDFromLegs() or closes via B.closeTradeFromLegs()
- `B.confirmSingleLeg(leg_id, match_type, trade_id, strategy_id, tp_pct, dry_run)` - Manually confirm a single leg when automatic matching fails

### Backtesting
- `V.readBacktest(strats_list)` - Run backtest simulation with eigenvalue-based portfolio sizing. Calls `V.portfolioSizing()` daily for new signals. Correlation matrix recomputed weekly (rolling). Key parameters (inside function): `risk_per_bet_pct` (0.5), `max_daily_risk_pct` (5), `correlation_adjustment` (0), `lookback_weeks` (104), `pnl_reference_usd` (1000). Returns list with `pnl`, `pnl_per_day`, `data`, `summary` (including `avg_n_eff`)
- `V.backtestModel()` - Generate backtest signals (run separately, produces CSV files consumed by `V.readBacktest`)

### Technical Analysis
- `T.plotPriceSeries()` - Diagnostic plot for price data
- `T.importInvestingComHistoFile()` - Import historical data from investing.com CSV
- `T.addAllSplines()` - Add spline-based technical indicators
- `T.calcHistoricalCorrelationsMatrix(instrument_ids, lookback_weeks, shrinkage, as_of_date, floor_at_zero)` - Calculate weekly return correlation matrix for portfolio optimization. Defaults: 104 weeks lookback, 0.1 shrinkage toward identity. `floor_at_zero=TRUE` clamps negative correlations to 0 (used for sizing, not display)

### Interactive Order Placement (Python - trade_orders.py, NEW 2026-03-04)
- `show_live_trades(account_id)` - Display all live trades with entry/target/stop prices
- `exit_orders(trade_id, account_id, dry_run=True)` - Place target LMT + stop STP as OCA pair (GTC). dry_run=True by default
- `exit_orders_all(account_id, dry_run=True)` - Place exit orders for all live trades
- `TradeOrderManager(account_id, client_id)` - Class for full control: connect(), get_trade_info(), place_exit_orders(), place_entry_order(), cancel_exit_orders()

### Asset Management
- `A.tradableInstruments()` - Returns pair names of tradable instruments from INSTRUMENTS table (filters: not bond, use_for_training=1, use_for_trading=1, use_for_trading_gs=1)
- `A.getInstrumentId(input_list)` - Convert pairs/tickers to instrument_ids

### Utilities
- `U.try(f, default)` - Wrap function with error handling (returns default on error)
- `U.debug(data, title)` - Debug output (prints when `print_diagnostics` is TRUE)
- `U.printBanner(message, big)` - Formatted console output

## Notes for Future Sessions

- Project root: `/home/fls/Models/Ventura/`
- Working directory for R: `/home/fls/Models/Ventura/HD/Code/`
- Log files: `/home/fls/Data/Ventura/SD/Log/` (subdirs: Python/, Technicals/, Model/, Communications/, Maintenance/, Book/)
- Only look at .R files in Code/ (ignore other file types)
- The Shiny dashboard works well - low priority for changes
- Owner is self-taught in R, professional Python developer

### Session Notes Practice
When user asks to "update CLAUDE.md" (indicates end of session):
1. Create session notes file in `Session_Notes/YYYY-MM-DD_brief_description.md`
2. Include: summary of changes, usage examples, testing checklist, next steps
3. Update CLAUDE.md with session status
4. Commit all changes with descriptive message
5. Create a git tag: `vX.Y-brief-description`
6. Push commit and tag to GitHub

**Tag format**: Increment minor version (v1.1, v1.2, v1.3...) for each session. Major version for breaking changes.
