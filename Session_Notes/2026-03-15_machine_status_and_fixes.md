# Session Notes: 2026-03-15 - Machine Status Monitoring & Correlation Fixes

## Summary

### 1. Correlation Floor Bug Fix (Claude)

**Problem**: `floor_at_zero` was applied on raw *asset* correlations before direction adjustment. Buy-one/sell-other on +0.72 correlated assets produced -0.72 trade correlation, causing optimizer to treat them as a free hedge (45% weight each).

**Fix**: Floor moved from `T.calcHistoricalCorrelationsMatrix()` to `V.portfolioSizing()` → `buildTradeCorrelationMatrix()`, applied *after* `dir_i * dir_j * asset_cor`.

### 2. Correlation Matrix Deduplication (Claude)

Trades tab was computing `T.calcHistoricalCorrelationsMatrix()` three times (sizing, correlations, orders). Added `Trades.Data.cor_matrix` reactive in server.R, passed to all three functions via new `cor_matrix` parameter.

### 3. Machine Status Monitoring (User)

New dashboard tab 6.1 "Machine Status" added by user from another repo:
- `G.Diagnostic.MachineStatus.Plot.systemLoad(metric, sub_metric, nb_days)` in GUI.R
- Reads timestamped text files from `/home/fls/Data/Glenorchy/SD/Machine_Status/<computer>/`
- Metrics: CPU, memory, swap, heat, IB process, R processes, DB stats (connections, size, CPU, memory)
- Data collected by crontab shell script on each machine using `top`, `df`, `sensors`, `ps aux`
- **Status**: Still has bugs, work in progress

### 4. TO_DAY Fix (User)

Added `TO_DAY <<- Sys.Date()` to key reactives in server.R to ensure date stays current across dashboard refreshes (Predict, Trades, Book, Diagnostics tabs).

## Files Changed

| File | Change | By |
|------|--------|----|
| `Code/Ventura.R` | Floor trade correlations at zero after direction adjustment | Claude |
| `Code/GUI.R` | Added `cor_matrix` param to sizing/correlations/orders functions; added `G.Diagnostic.MachineStatus.Plot.systemLoad()` | Claude + User |
| `Code/Book.R` | Added `cor_matrix` param to `B.generateOrders()`; removed `floor_at_zero = TRUE` | Claude |
| `Platform/server.R` | `Trades.Data.cor_matrix` reactive; `TO_DAY` refresh; Machine Status tab 6.1 | Claude + User |
| `Platform/ui.R` | Machine Status tab UI; NAV date default updated to 2026 | User |

## Next Steps

- Fix remaining bugs in Machine Status dashboard tab
- Move `tradable_instruments` list from hardcoded to DB (`instrument` table)
- Test `trade_orders.py` live with real orders
