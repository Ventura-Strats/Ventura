# Session Notes: 2026-04-23 (2) - Position Inventory Tab & Bracket Order RTH

## Summary of Changes

### 1. Target order fills outside RTH (place_bracket_order.py)
- Added `take_profit.outsideRth = True` to the bracket order's take-profit leg
- Stop loss remains RTH-only (default `outsideRth = False`)
- This allows profit targets to be filled during extended/overnight sessions

### 2. New Position inventory tab in dashboard (GUI.R, ui.R, server.R)
- Replaced the broken "FX position" sub-tab with a unified "Position" tab under Book
- Tab contains two sections: **ETF** and **FX** position reconciliation

#### ETF Position table (new: `G.Book.Position.Table.etf_position()`)
- **Expected**: from `trd_live` filtered for `instrument_type == "ETF"`, net shares = `sum(buy_sell * size)` grouped by pair and account
- **Actual**: from `px_position_{account}_last.csv` files (IB Account Data), filtered for `secType == "STK"`, joined to ETF table by conid to resolve pair names
- **Reconciliation**: full join on pair, computes Diff per account and total
- Output columns: `Pair | Expected_1 | Actual_1 | Diff_1 | Expected_2 | Actual_2 | Diff_2 | Total_Expected | Total_Actual | Diff_Total`
- Uses gvisTable with `#,###` format (same style as existing FX table)

#### FX Position table (bug fix)
- Fixed crash "replacement has 0 rows, data has 16" in `G.Book.FX.Table.fx_position()`
- **Root cause**: when there are no live FX trades, `spread()` produces no `Expected_*` columns; the subsequent loop tried `Actual_i - Expected_i` where `Expected_i` was NULL, producing a length-0 replacement
- **Fix**: added guard after join to create missing `Expected_*`/`Actual_*`/`Total_*` columns as 0 before the reconciliation loop

### 3. Minor: B.save() and B.sendOrder script_path fix (Book.R)
- `B.save()` moved alphabetically (dump artifact)
- `B.sendOrder()` script path fixed from `DIRECTORY_HD` to `DIRECTORY_CODE_HD`

### Remaining
- FX actual positions from IB report look incorrect — to investigate next session (likely an issue with how `book_live_position_fx` data or CSV FX positions are read/interpreted)
