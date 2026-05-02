# Ventura Changelog

Historical record of completed work. For current status, see CLAUDE.md. For full details, see Session_Notes/.

## 2026-05-02
- **Codebase reorganization**: R modules to Code/R/, Python scripts to Code/Python/, junk files to Code/Old/
- **Path references updated**: All dump() calls, Init.R setwd/load, Book.R python path, Platform setwd, Python init.py chdir, .gitignore
- **Shell launchers updated**: RScriptVentura.sh and PScriptVentura.sh point to new Code/R/ and Code/Python/ paths
- **Safety symlink**: Scripts/Python -> Code/Python for backward compatibility
- **B.closeTradeFromLegs()**: Made exit_date optional, auto-derived from leg timestamp

## 2026-03-24
- **Local model loading**: V.modelPredict() loads from Models_Local/ first, NAS fallback
- **Per-machine model copy crontab**: Each machine copies only its needed model files, staggered 5min apart
- **Crontab kill-then-predict**: Merged kill+start into single cron line, final kill at :05 next hour
- **Price_Live.R filter change**: A.filterInstruments("predict") -> A.filterInstruments("all") in getPairList()
- **Predict.R machine mapping docs**: Added strategy-to-machine assignment comments

## 2026-03-22
- **psi_60 bug fix**: Fixed lag(close, 20) -> lag(close, 60) in T.addSimpleStuff. Corrected 974K+ DB rows
- **Technical features documentation**: Documentation/Technical_Features.md (~237 features catalogued)
- **V.removeUselessFeatures save_to_db**: Added save_to_db parameter for crontab automation
- **Backtest enhancements**: _stitched.csv format, gross_pnl metric added to summary stats
- **Crontab Predict kill timing**: :59 -> :05 next hour for extra runtime
- **Strategies documentation**: Documentation/Strategies.md (14 strategies, scoring system, criteria)
- **Research areas documentation**: Documentation/Research_Areas.md (future feature/strategy ideas)
- **New features implementation prep**: Documentation/New_Features_Implementation.md (pipeline trace, architecture decisions)

## 2026-03-21
- **A.filterInstruments refactor**: Renamed A.tradableInstruments() to A.filterInstruments(use_case), 5 use cases, 26 call sites
- **Trim_Machine_Status.sh**: Maintenance script keeps last 3000 lines per file (1.3GB -> 18MB)
- **Backtest data gap fix**: V.readBacktest(2) NA crash on 2021-08-20, repopulated missing dates
- **Documentation folder**: Created Documentation/ with Feature_Selection.md

## 2026-03-19
- **MySQL server tuning**: 20G buffer pool, O_DIRECT, 1G redo log, 8 I/O threads, binlog off, 8G swap
- **"DB not writable" fix**: SQLAlchemy 2.x requires text() wrapping and explicit commit() in db.py

## 2026-03-18
- **V.portfolioSizing antagonist netting fix**: NA propagation from strategy disagreements, replace_na() after join

## 2026-03-17
- **Backtest portfolio sizing**: Eigenvalue-based V.portfolioSizing in V.readBacktest, weekly correlation recompute

## 2026-03-16
- **Tradable instruments cleanup**: A.tradableInstruments() reads from INSTRUMENTS table DB flags

## 2026-03-15
- **Machine status monitoring**: Dashboard tab 6.1, G.Diagnostic.MachineStatus.Plot.systemLoad() (still has bugs)

## 2026-03-10
- **Correlation floor fix & dedup**: Floor applied to trade correlations after direction adjustment. Deduplicated via Trades.Data.cor_matrix reactive

## 2026-03-06
- **Trades tab restructure**: 4 sequential tables: Signals -> Portfolio Sizing -> Correlation Matrix -> Execution Orders

## 2026-03-04
- **Trade orders module**: trade_orders.py for exit orders (OCA pairs) and entry orders (chase algo)

## 2026-03-03
- **Correlation floor at zero**: floor_at_zero parameter for T.calcHistoricalCorrelationsMatrix()
- **G.Trades.Table.orders fix**: U.try() parameter is f_rtn, not default
- **IB Gateway move**: From machine H (34) to machine I (42), ports 7496/7497

## 2026-03-02
- **First live trades**: 3 trades executed manually from signals

## 2026-02-21
- **Trade workflow automation**: B.generateOrders(), B.matchLegsToTrades(), B.confirmLegMatch(), B.confirmSingleLeg()
- **Dashboard orders table**: G.Trades.Table.orders() for Trades tab
- **Security**: Credentials moved to environment variables

## 2026-02-19
- **GitHub setup**: SSH auth, main + signals branches, GitPushVentura.sh

## 2026-01-04
- **IB API error() fix**: advancedOrderRejectJson parameter in 10 Python files
- **init.py fix**: scriptFinish() handles missing script_id
- **Read_Executions.py fix**: Dynamic timezone parsing (was hardcoded Asia/Hong_Kong)
- **DB.R pool fix**: validationInterval = 30 for connection pool

## Major Feature: Eigenvalue-Based Portfolio Sizing (v2)
- T.calcHistoricalCorrelationsMatrix() - weekly return correlation (Tuesday close)
- V.portfolioSizing() - eigenvalue N_eff sizing with antagonist netting, correlation floor, quadprog optimization
- Dashboard: Signals -> Sizing -> Correlation -> Orders pipeline
- Backtest integration (2026-03-17): daily sizing, weekly correlation recompute
