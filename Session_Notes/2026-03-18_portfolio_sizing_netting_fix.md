# Session Notes: V.portfolioSizing Antagonist Netting Fix (2026-03-18)

## Bug

`V.readBacktest(c(7, 11))` crashed with:
```
Error in if (U.dfContainsData(trd_new) && nav_morning > 0) { :
  missing value where TRUE/FALSE needed
```

Individual strategies (`V.readBacktest(7)`, `V.readBacktest(11)`) worked fine.

## Root Cause

In `V.portfolioSizing` (Ventura.R ~line 2303), when mapping netted weights back to original signals via `left_join`:

1. Two strategies disagree on same instrument (e.g., strat 7 BUY EURUSD + strat 11 SELL EURUSD)
2. `netAntagonistSignals` correctly removes instrument (net direction = 0)
3. `left_join` back to original signals produces `NA` for `buy_sell_netted` and `weight_netted` (no match in `dat_netted`)
4. `matches_netted = (sign(buy_sell) == NA)` returns `NA`, not `FALSE`
5. `ifelse(NA, ..., 0)` returns `NA`, not `0`
6. `sized_notional = NA` cascades: fees -> pnl -> nav_evening -> nav_morning -> crash

Single strategies never trigger this because one strategy can't produce opposite signals on the same instrument on the same day.

## Fix

Added `replace_na()` after the `left_join` in `V.portfolioSizing` (line ~2308):
```r
mutate(
    buy_sell_netted = replace_na(buy_sell_netted, 0),
    weight_netted = replace_na(weight_netted, 0)
)
```

Also added `pmax(n_matching, 1)` to prevent division by zero.

Cancelled instruments get `buy_sell_netted = 0`, so `sign(buy_sell) == 0` is always `FALSE`, giving `weight = 0` and `sized_notional = 0`.

## Scope

Fix is in `V.portfolioSizing` which is shared by:
- `V.readBacktest` (backtest)
- `G.Trades.Table.sizing` (dashboard)
- `B.generateOrders` (order generation)

The live paths were unlikely to trigger this (user manually selects signals), but the fix makes them robust too.

## Files Modified
- `Code/Ventura.R` — `V.portfolioSizing`, lines ~2308-2311
