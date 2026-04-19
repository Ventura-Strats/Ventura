# Ventura Trading Model

R trading model with Python IB API integration. Random forest predictions, technical indicators, Shiny dashboard. Distributed across networked machines via crontab.

## Directory Structure

```
/home/fls/Models/Ventura/
├── HD/Code/           # Core R modules (Init, Utils, DB, Assets, Book, Communications, Engine, GUI, Technicals, Ventura)
├── HD/Platform/       # Shiny dashboard (server.R, ui.R)
├── HD/Scripts/        # Scheduled jobs (Model/, Technicals/, Book/, Communications/, Maintenance/, Scenario/, Crontab/, Python/)
├── HD/Documentation/  # Feature selection, strategies, technical features, research areas
├── HD/Session_Notes/  # Per-session change logs
└── SD/                # SSD mount (models, fast storage)
```

## Module Naming

Functions use prefix: `U.*` Utils | `D.*` DB | `T.*` Technicals | `B.*` Book | `I.*` Init | `V.*` Ventura | `A.*` Assets | `C.*` Communications | `E.*` Engine | `G.*` GUI. Each module has `X.save()` to dump functions to .R file.

## Script Pattern

All scripts: set `start_time`, `script_name`, `max_time_hours`, define `Script()` with `I.completeStage()` calls, then `source("Init.R"); I.executeScript()`. This handles DB check, drive access, status logging, timeout, errors.

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
- R working directory: `/home/fls/Models/Ventura/HD/Code/`
- Log files: `/home/fls/Data/Ventura/SD/Log/`
- Local models: `/home/fls/Data/Ventura/Models_Local/`
- NAS models: `/home/fls/Data/Ventura/SD/Models/`
- Crontab generator: `/home/fls/Models/Glenorchy/HD/Scripts/Crontab/Crontab_Generator_user.txt` (Glenorchy project)

## Current Priorities

1. **Automate trade execution**: Test trade_orders.py live (dry_run=False), enable Execute_Orders.py
2. **Execution reconciliation**: Daily Flex Query to catch missed overnight/weekend fills
3. **Fix historical data gaps**: Improve data pipeline resilience
4. **Python codebase cleanup**: Refactor legacy IB API scripts incrementally
5. **Future**: Convert to R package (lower priority)

## Remaining Issues

1. **Historical data corruption**: Gaps from downtime, some incorrect FX prices. Workaround: `T.importInvestingComHistoFile()`. Diagnostic: `T.plotPriceSeries()`
2. **Trade execution not fully automated**: trade_orders.py works in dry_run only; Execute_Orders.py order placement commented out
3. **Machine status monitoring**: Dashboard tab 6.1 still has bugs

## Preferences

- Only look at .R files in Code/ (ignore other file types)
- Shiny dashboard works well — low priority for changes
- Owner is self-taught in R, professional Python developer

## Reference

- `CHANGELOG.md` — History of completed work
- `Documentation/` — Feature selection, strategies, technical features, research areas, implementation guide
- `Session_Notes/` — Detailed per-session change logs
- `.claude/skills/` — session-end, backtest, crontab, ib-orders
