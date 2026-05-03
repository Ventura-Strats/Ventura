# Ventura Trading Model

R trading model with Python IB API integration. Random forest predictions, technical indicators, Shiny dashboard. Distributed across networked machines via crontab.

## Directory Structure

```
/home/fls/Models/Ventura/
├── HD/Code/R/              # Core R modules (Init, Utils, DB, Assets, Book, Communications, Engine, GUI, Technicals, Ventura)
├── HD/Code/Python/         # Python package + legacy scripts
│   ├── ventura/            # Main Python package
│   │   ├── config.py       # VenturaConfig frozen dataclass (replaces init.py globals)
│   │   ├── db.py           # Database class with connection pooling
│   │   ├── utils.py        # Date/TZ helpers, logging setup
│   │   ├── script_runner.py # run_script() — Python equiv of R's I.executeScript()
│   │   ├── ib/             # IB interface (all ib_insync, no ibapi)
│   │   │   ├── connection.py  # IBConnection manager, port/client_id lookup
│   │   │   ├── account.py     # AccountDataRetriever (NAV, positions, FX)
│   │   │   ├── executions.py  # ExecutionReader (read fills from IB)
│   │   │   ├── prices.py      # Historical/Live/Future price retrieval
│   │   │   ├── contracts.py   # ContractResolver (qualify, resolve conids)
│   │   │   ├── orders.py      # Order, OrderExecutor (chase algo)
│   │   │   └── trade_orders.py # TradeOrderManager (exit/entry via OCA)
│   │   └── signals/        # Signal processing
│   │       ├── predictions.py # Load/filter model predictions
│   │       ├── sizing.py      # PortfolioSizer (eigenvalue-based)
│   │       ├── signal_converter.py # SignalConverter (signals→orders)
│   │       ├── order_list.py  # NewOrdersBuilder (spot/future/ETF)
│   │       ├── github.py      # GithubSignalExporter
│   │       ├── exits.py       # ExitOrdersBuilder
│   │       └── combined.py    # CombinedOrdersBuilder
│   └── old/                # Archived legacy Python files
├── HD/Scripts/Python/      # Thin Python scripts (called by crontab via PScriptVentura.sh)
│   ├── ib_account_data.py
│   ├── read_executions.py
│   ├── price_ib.py
│   ├── price_ib_future.py
│   ├── signal_list.py
│   ├── execute_orders.py
│   ├── place_bracket_order.py  # CLI for R's B.sendOrder()
│   └── get_future_expiries.py
├── HD/Code/Old/            # Archived files (VenturaStrat.RData, old CSVs, Rmd reports)
├── HD/Platform/            # Shiny dashboard (server.R, ui.R)
├── HD/Scripts/             # Scheduled jobs (Model/, Technicals/, Book/, Communications/, Maintenance/, Scenario/, Crontab/, Python/)
├── HD/Documentation/       # Feature selection, strategies, technical features, research areas
├── HD/Session_Notes/       # Per-session change logs
└── SD/                     # SSD mount (models, fast storage)
```

## Module Naming

Functions use prefix: `U.*` Utils | `D.*` DB | `T.*` Technicals | `B.*` Book | `I.*` Init | `V.*` Ventura | `A.*` Assets | `C.*` Communications | `E.*` Engine | `G.*` GUI. Each module has `X.save()` to dump functions to .R file.

## Script Pattern

**R scripts**: set `start_time`, `script_name`, `max_time_hours`, define `Script()` with `I.completeStage()` calls, then `source("Init.R"); I.executeScript()`. This handles DB check, drive access, status logging, timeout, errors.

**Python scripts** (in `Scripts/Python/`): thin wrappers using `ventura.script_runner.run_script()`:
```python
from ventura.script_runner import run_script, ScriptContext
def main(ctx: ScriptContext) -> None:
    # ctx.db, ctx.config, ctx.start_time available
    ...
if __name__ == "__main__":
    run_script("Script_Name", main_fn=main, max_time_hours=1.0)
```

## Coding Style

- Tidyverse pipes (`%>%`), tibbles over data.frames
- Error handling via `U.try(f, f_rtn)` wrapping `purrr::possibly()`
- Global config variables (`DIRECTORY_CODE_HD`, `TO_DAY`, `YESTERDAY`)
- Debug: `U.debug()`, `U.printBanner()`

## Infrastructure

- **Database**: MySQL 8.0.45 on 192.168.0.37 (X), database `Ventura`, credentials via env vars (`VENTURA_DB_*`)
- **Machines**: H=.34 (dev) | X=.37 (DB) | Z=.38 | W=.39 | V=.40 | Y=.41 | I=.42 (IB Gateway, ports 7496/7497)
- **Code on shared NAS** — git pull only needed on one machine
- **GitHub**: `git@github.com:Ventura-Strats/Ventura.git` — `main` (code), `signals` (trading signals)
- **Credentials**: env vars from `.ventura_env` (gitignored), `.Renviron` for RStudio

## Key Paths

- Project root: `/home/fls/Models/Ventura/`
- R working directory: `/home/fls/Models/Ventura/HD/Code/R/`
- Python working directory: `/home/fls/Models/Ventura/HD/Code/Python/`
- Log files: `/home/fls/Data/Ventura/SD/Log/`
- Local models: `/home/fls/Data/Ventura/Models_Local/`
- NAS models: `/home/fls/Data/Ventura/SD/Models/`
- Crontab generator: `/home/fls/Models/Glenorchy/HD/Scripts/Crontab/Crontab_Generator_user.txt` (Glenorchy project)

## Current Priorities

1. **Crontab migration**: Update PScriptVentura.sh (needs sudo) and crontab entries to use new Scripts/Python/ scripts
2. **Automate trade execution**: Test ventura.ib.trade_orders live (dry_run=False), enable execute_orders.py
3. **Execution reconciliation**: Daily Flex Query to catch missed overnight/weekend fills
4. **Fix historical data gaps**: Improve data pipeline resilience
5. **Move old Python files**: Once crontab migrated, move remaining Code/Python/*.py to Code/Python/old/
6. **Future**: Convert to R package (lower priority)

## Remaining Issues

1. **Historical data corruption**: Gaps from downtime, some incorrect FX prices. Workaround: `T.importInvestingComHistoFile()`. Diagnostic: `T.plotPriceSeries()`
2. **Trade execution not fully automated**: ventura.ib.trade_orders works in dry_run only
3. **Machine status monitoring**: Dashboard tab 6.1 still has bugs
4. **PScriptVentura.sh**: Needs sudo to update PATH_SCRIPTS from Code/Python/ to Scripts/Python/

## Python Package

The `ventura` package in `Code/Python/ventura/` provides:
- **config.py**: `VenturaConfig` frozen dataclass (replaces 13+ init.py globals)
- **db.py**: `Database` class with SQLAlchemy connection pooling
- **script_runner.py**: `run_script()` + `ScriptContext` (equivalent of R's `I.executeScript()`)
- **ib/**: All IB interaction via `ib_insync` (replaced old `ibapi` callback classes)
- **signals/**: Signal processing pipeline (predictions → sizing → orders → execution)

Old `ibapi` scripts remain in `Code/Python/` during transition. New scripts in `Scripts/Python/` are thin wrappers around the ventura package.

## Preferences

- R modules in Code/R/, Python package in Code/Python/ventura/
- Shiny dashboard works well — low priority for changes
- Owner is self-taught in R, professional Python developer

## Reference

- `CHANGELOG.md` — History of completed work
- `Documentation/` — Feature selection, strategies, technical features, research areas, implementation guide
- `Session_Notes/` — Detailed per-session change logs
- `.claude/skills/` — session-end, backtest, crontab, ib-orders
