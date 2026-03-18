# Session Notes: Backtest Portfolio Sizing Integration (2026-03-17)

## Summary

Replaced the crude equal-risk-per-trade sizing in `V.readBacktest` with the eigenvalue-based `V.portfolioSizing` that already powers live trading. The backtest now uses the same antagonist-netting, N_eff calculation, and min-variance optimization pipeline as the live Trades tab.

## Key Changes

### Parameters Replaced
- **Removed**: `pnl_tp_usd` (0.01), `capital_maximum_allocation_per_day` (0.1), `max_capital_per_trade_vs_nav` (5)
- **Added**: `risk_per_bet_pct` (0.5%), `max_daily_risk_pct` (5%), `correlation_adjustment` (0), `lookback_weeks` (104), `shrinkage` (0.1), `pnl_reference_usd` (1000)

### Core Sizing Change (`calcPnLTradesNew`)
- Old: crude `capital_allocation_ratio = 10 / max(10, nb_trades)` scaling
- New: calls `V.portfolioSizing()` with `aum_total = nav_morning` (current NAV)
- Correlation matrix recomputed weekly (rolling) via `getCorrelationMatrix()` with caching
- Falls back to identity matrix if insufficient historical data

### Data Pipeline
- `instrument_id` preserved through `readFiles` -> `prepareBacktestData` -> `prepareTradesList`
- `prepareTradesList` now outputs unsigned `notional_ref` (reference notional for $1K P&L) matching `V.portfolioSizing` contract
- `findNotionalsForExistingTrade` uses `instrument_id` in join keys for more precise matching

### Per-Strategy P&L Removed
- Removed ~196 per-strategy columns from PnL table (14 metrics x 14 strategies)
- Simplified `preparePnLTable`, `initializeDailyMorningValues`, `calcPnLTradesLive`, `calcPnLTradesExit`, `wrapUpDailyValues`
- Added `n_effective` column for daily effective bets tracking
- Added `avg_n_eff` to yearly and total summary statistics

### New Subroutine: `getCorrelationMatrix(date_i)`
- Caches correlation matrix by ISO week string
- Recomputes via `T.calcHistoricalCorrelationsMatrix()` only when week changes
- ~100 DB calls over a 2-year backtest (vs ~500 daily)
- Wrapped in `U.try()` for resilience

## Files Modified
- `Code/Ventura.R` — `V.readBacktest` function (lines ~2844-3445)

## Functions Reused (no changes)
- `V.portfolioSizing()` — eigenvalue-based sizing (antagonist netting, N_eff, min-variance)
- `T.calcHistoricalCorrelationsMatrix()` — weekly return correlation matrix

## Testing Checklist
- [ ] Run `V.readBacktest()` with short date range (e.g., 1 month)
- [ ] Verify `n_effective` values are reasonable (1-20 range)
- [ ] Verify NAV progression is stable (no explosion or immediate collapse)
- [ ] Verify correlation matrix recomputation messages appear weekly
- [ ] Compare summary stats (Sharpe, return, drawdown) against old crude sizing
- [ ] Test with `strats_list = c(1)` (single strategy) — should work with no netting
- [ ] Test edge case: day with all signals cancelling (same asset, opposite directions)

## Usage
```r
# Run full backtest with default parameters
result <- V.readBacktest()

# Run with specific strategies
result <- V.readBacktest(strats_list = c(1, 2, 5))

# Key parameters to tune (inside the function):
# risk_per_bet_pct <- 0.5        # risk per independent bet as % of NAV
# max_daily_risk_pct <- 5        # cap on total daily risk as % of NAV
# correlation_adjustment <- 0    # inflate/deflate historical correlations
# lookback_weeks <- 104          # correlation matrix lookback
```

## Notional Magnitude Reference
With `initial_nav = 100`, `risk_per_bet_pct = 0.5`, single EURUSD signal at 1% target:
- `notional_ref = 1000 / 0.01 = 100,000 EUR` (for $1K P&L)
- `total_risk_usd = 100 * 0.5% = 0.50`
- `sized_notional = 1 * 100000 * 0.50 / 1000 = 50 EUR`
- P&L at target = 50 * 1% = $0.50 (= 0.5% of NAV)

## Next Steps
- Run full backtest and compare results with old sizing
- Consider making sizing parameters function arguments instead of hardcoded
- Consider adding `avg_n_eff` to the PnL plot or a separate diagnostic plot
