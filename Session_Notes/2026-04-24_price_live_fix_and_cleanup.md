# Session Notes: 2026-04-24 - Price_Live.R Fix & Cleanup

## Summary of Changes

### 1. Fix archive_predict query hang in T.estimateTodayOHLCIndexPriceWhenNotYetOpenFromLiveFuture (Technicals.R)
- **Bug**: `prepareIndexPrice()` filtered on `timestamp_px` (not indexed), causing a full table scan on `archive_predict` taking 3+ minutes
- **Impact**: Price_Live.R hung at stage 6; `R.utils::withTimeout` could not interrupt the C-level `dbGetQuery` blocking call, so the script never completed
- **Root cause**: PK on `archive_predict` is `(strategy_id, instrument_id, use_weights, timestamp)` — filtering on `timestamp_px` bypasses the index entirely
- **Fix**: Changed SQL WHERE clause to filter on `timestamp >= YESTERDAY` (uses PK index), then R-side `filter(timestamp_px >= YESTERDAY)` on the small result set

### 2. Fix U.dbeug typo (Technicals.R)
- Line 4410 had `U.dbeug(4)` instead of `U.debug(4)`
- This silently prevented the final `saveToDB` step from executing (caught by outer `U.try`)

### 3. Remove debug statements from T.estimateTodayOHLCIndexPriceWhenNotYetOpenFromLiveFuture (Technicals.R)
- Removed all `U.debug()`, `U.printBanner()`, and `print()` debug calls from the function
- Cleaned up commented-out code (`instruments_to_do <- 27`, `estimateLiveOHLC`)

### 4. B.sendOrder() signature change (Book.R, user change)
- Replaced `target_price` and `stop_price` params with `tgt_pct`
- Target and stop prices now computed symmetrically from entry_price and tgt_pct

### 5. Other user changes
- **Ventura.R**: Added debug/logging around model loading (local vs NAS), fixed backtest weights file naming
- **Load_Tech.R**: Added `removeOlderFiles()` to clean up old technicals backups (keep last per month)
- **Predict.R**: Updated strategy distribution comments, cleaned up machine assignment docs
