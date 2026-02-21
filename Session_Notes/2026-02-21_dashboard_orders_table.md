# Session Notes: Dashboard Orders Table
**Date**: 2026-02-21
**Status**: Complete

## Summary
Added an "Execution Orders" table to the Shiny dashboard Trades tab, showing the output of `B.generateOrders()` before orders are sent to IB. Displays all 21 order columns across all accounts.

---

## Changes Made

### 1. GUI.R - New Function

#### G.Trades.Table.orders() (~40 lines)
**Purpose**: Display execution orders in the dashboard before they're sent to IB.

**Usage** (called automatically by dashboard reactive):
```r
G.Trades.Table.orders(
    dat_predict,                      # From G.Predict.Data.predict()
    risk_per_bet_pct = 0.5,           # 0.5% risk per effective bet
    max_daily_risk_pct = 5,           # 5% max daily risk
    correlation_adjustment = 0,       # 0=historical
    account_ids = c(1, 2)             # Both accounts
)
```

**Key features**:
- Calls `B.generateOrders(dat_predict, export_csv = FALSE)` - no CSV side-effects
- Wrapped in `U.try()` so dashboard doesn't crash if DB/NAV unavailable
- Shows all 21 columns: order_id, ib_order_id, account_id, instrument_id, ticker, future_id, conid, contract, tick_size, buy_sell, buy_sell_action, size_to_do, px_order, px_live, px_avg, initial_position, position, filled, remaining, status, ib_status
- Returns empty table with column headers when no signals

### 2. server.R - Reactive Wiring
```r
Trades.Table.orders <- reactive(G.Trades.Table.orders(Predict.Data.predict()))
output$Trades.Table.orders <- renderRHandsontable(Trades.Table.orders())
```
Shares the same `Predict.Data.predict()` data source - refreshes on same button.

### 3. ui.R - Layout Update
Added between signal table and correlation matrix:
```r
h4("Execution Orders"),
rHandsontableOutput("Trades.Table.orders"),
hr(),
```

---

## Dashboard Layout (Trades Tab)

```
[Refresh]
[Signal Sizing Table]              <- existing
--------------------------------------
Execution Orders                    <- NEW
[Orders Table - all 21 columns]     <- NEW
--------------------------------------
Correlation Matrix (Trade-Adjusted) <- existing
[Correlation Table]                 <- existing
```

---

## Files Changed

| File | Change | Description |
|------|--------|-------------|
| `Code/GUI.R` | +40 lines | New `G.Trades.Table.orders()` function |
| `Platform/server.R` | +3 lines | Reactive + render for orders table |
| `Platform/ui.R` | +3 lines | Output element in Trades tab |

---

## Testing Checklist

- [ ] Load dashboard: `source("Init.R")` then run Shiny app
- [ ] Click Refresh on Trades tab
- [ ] Verify "Execution Orders" table appears between signal table and correlation matrix
- [ ] Verify all 21 columns are present
- [ ] Verify per-account rows show (account_id 1 and 2)
- [ ] Verify px_order formatted with 6 decimals
- [ ] Verify size_to_do shows thousands separators
- [ ] Test with no active signals: should show empty table with column headers
- [ ] Test when DB is down: should show empty table (U.try catches error)

---

## Next Steps

1. **Test with live signals**: Verify the orders match what `B.generateOrders()` returns in console
2. **Enable Execute_Orders.py**: Still TODO - order placement is commented out
3. **Python codebase cleanup**: Incremental refactoring of IB API scripts
