# Session Notes: 2026-03-10 - Correlation Floor Fix & Deduplication

## Summary

Two related fixes to the portfolio sizing pipeline:

### 1. Correlation Floor Bug Fix

**Problem**: `floor_at_zero` was applied on raw *asset* correlations in `T.calcHistoricalCorrelationsMatrix()`, before the direction adjustment (`dir_i * dir_j * asset_cor`) in `V.portfolioSizing()`. This caused incorrect behavior in two scenarios:

- **Buy-one / sell-other on correlated assets**: Asset correlation +0.72 passed through the floor unchanged, then `(+1)(-1)(0.72) = -0.72` trade correlation. The optimizer treated this as a free hedge, giving 45% weight to each trade and only 10% to the uncorrelated third trade.
- **Buy-both on negatively correlated assets**: A negative asset correlation would be floored to 0 before the direction flip could turn it into the correct positive trade correlation.

**Fix**: Removed `floor_at_zero = TRUE` from callers in `Book.R` and `GUI.R`. Added `pmax(trade_cor, 0)` inside `buildTradeCorrelationMatrix()` in `V.portfolioSizing()`, applied *after* the direction adjustment.

**Files changed**: `Ventura.R`, `Book.R`, `GUI.R`

### 2. Correlation Matrix Deduplication in Dashboard

**Problem**: The Trades tab computed `T.calcHistoricalCorrelationsMatrix()` independently in three places: `G.Trades.Table.sizing()`, `G.Trades.Table.correlations()`, and `G.Trades.Table.orders()` (via `B.generateOrders()`). This is the same expensive DB query and computation repeated 3x on every refresh.

**Fix**: Added `Trades.Data.cor_matrix` reactive in `server.R` that computes the matrix once. All three functions now accept an optional `cor_matrix` parameter (default `NULL` for standalone use, provided when called from the dashboard).

**Files changed**: `server.R`, `GUI.R`, `Book.R`

## Changes by File

| File | Change |
|------|--------|
| `Code/Ventura.R` | Added `pmax(trade_cor, 0)` + `diag(trade_cor) <- 1` after direction adjustment in `buildTradeCorrelationMatrix()` |
| `Code/Book.R` | Added `cor_matrix = NULL` param to `B.generateOrders()`, removed `floor_at_zero = TRUE` |
| `Code/GUI.R` | Added `cor_matrix = NULL` param to `G.Trades.Table.correlations()`, `G.Trades.Table.sizing()`, `G.Trades.Table.orders()`. Each computes its own if not provided |
| `Platform/server.R` | New `Trades.Data.cor_matrix` reactive, passed to all three Trades tab functions |

## Testing Checklist

- [ ] Reload Shiny dashboard, verify Trades tab loads without errors
- [ ] Check N_Eff value is reasonable (should be higher than before for mixed buy/sell signals)
- [ ] Verify correlation matrix display shows unmodified trade correlations (including negatives)
- [ ] Verify sizing weights are more balanced when buy/sell signals on correlated assets
- [ ] Test with single signal (edge case)
- [ ] Test with all signals unchecked (edge case)
- [ ] Test `B.generateOrders()` standalone (without `cor_matrix`) still works

## Next Steps

- Move `tradable_instruments` list from hardcoded to DB (`instrument` table)
- Test `trade_orders.py` live with real orders
