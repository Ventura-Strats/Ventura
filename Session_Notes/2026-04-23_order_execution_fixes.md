# Session Notes: 2026-04-23 - Order Execution Fixes & Bracket Order Wrapper

## Summary of Changes

### 1. Fix broken SQL in B.matchLegsToTrades (Book.R)
- **Bug fix**: `getUnmatchedLegs()` and `getLiveTrades()` had SQL subqueries referencing `FROM instrument` (non-existent table) with columns `pair`, `asset_class`, `conid_spot` that don't exist on `static_instrument`
- **Fix**: Replaced SQL subqueries with R-side `left_join` on the in-memory `INSTRUMENTS` tibble, matching the pattern already used in `getRecentSignals()`
- Both functions now build `instruments_with_identifier` from `INSTRUMENTS` and join by `identifier` after the SQL query

### 2. Aggregate orders by instrument in B.generateOrders (Book.R)
- **Bug**: When multiple strategies signal the same instrument (e.g., 2 strategies both buy SPXUSD), the order list contained duplicate lines for the same asset
- **Root cause**: `V.portfolioSizing()` correctly nets and splits weights across strategies, but `B.generateOrders()` passed per-strategy rows straight through to order formatting
- **Fix**: Added `group_by(instrument_id, ...) %>% summarise(weight = sum(weight), sized_notional = sum(sized_notional))` after sizing, before `addInstrumentDetails`
- `signals_used` output now reads from `dat_sized` (pre-aggregation) to preserve per-strategy detail

### 3. New: B.sendOrder() bracket order wrapper (Book.R + place_bracket_order.py)
- **New R function** `B.sendOrder(pair, buy_sell, size, entry_price, target_price, stop_price, account_id, dry_run=TRUE)`:
  - Resolves instrument metadata (asset_class, conid, tick_size) from `INSTRUMENTS` and local CSV tables
  - Validates inputs (direction, size, price ordering)
  - Rounds prices to tick_size
  - `dry_run=TRUE` (default): prints order summary, returns without calling Python
  - `dry_run=FALSE`: calls Python script via `system()`, parses JSON result
- **New Python script** `Scripts/Python/place_bracket_order.py`:
  - Standalone (no db.py/init.py imports), only needs `ib_insync`
  - Places IB bracket order via `ib.bracketOrder()`: parent LMT + take-profit LMT GTC + stop-loss STP GTC
  - All metadata received via CLI args, outputs JSON to stdout
  - client_id=25, port = 7496 + account_id - 1

### Notes
- Existing bug identified: `B.generateOrders()` line 438 uses `attribute_id == 4` for tick_size lookup, but the only attribute_id in `static_instrument_attribute_dbl` is 5. Out of scope for this session.
- Other modified files (GUI.R, Ventura.R, Load_Tech.R, Predict.R) were not committed -- pre-existing uncommitted changes
