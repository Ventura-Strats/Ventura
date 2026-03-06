# Session Notes: 2026-03-06 - Trades Tab Restructure

## Summary of Changes

### Trades Tab Restructured into 4 Sequential Tables

The dashboard Trades tab was restructured from a single table with mixed concerns into 4 clean, sequential sections:

1. **Signals** (`G.Trades.Table.predict`) - Signal list with Execute checkbox
2. **Portfolio Sizing** (`G.Trades.Table.sizing`) - NEW - Eigenvalue-based sizing on selected trades
3. **Correlation Matrix** (`G.Trades.Table.correlations`) - Trade-adjusted correlations
4. **Execution Orders** (`G.Trades.Table.orders`) - Per-account orders from `B.generateOrders()`

### 1. Simplified G.Trades.Table.predict()
- **Removed**: All sizing logic (`V.portfolioSizing`, `T.calcHistoricalCorrelationsMatrix` calls)
- **Removed**: Parameters `aum_total`, `risk_per_bet_pct`, `max_daily_risk_pct`, `correlation_adjustment`
- **Removed**: Columns `Weight_Pct`, `Sized_Notional`, `N_Eff`
- **Kept**: Execute checkbox (editable), all signal columns (Region, Asset_Class, Strategy, Ticker, Name, Price, Predict, Target, Stop, Target_Pct, Notional_for_1k_PnL)
- **Rationale**: Sizing columns were misleading when trades were unticked - they didn't recalculate

### 2. New G.Trades.Table.sizing()
- Takes filtered `dat_predict` (after Execute checkbox filtering)
- Filters on `tradable_instruments` list (same as predict)
- Runs `V.portfolioSizing()` which includes antagonist signal netting
- Groups results by instrument (merging multiple strategies on same asset)
- Returns a list: `n_effective` (numeric) and `table` (gvisTable with Instrument, Direction, Weight_Pct, Sized_Notional)
- N_Eff displayed as text above the table, not as a column
- Only shows trades with weight > 0 (netted-out trades excluded)

### 3. G.Trades.Table.correlations() - tradable filter added
- Now filters on `tradable_instruments` list (was missing before)
- Ensures correlation matrix only shows instruments we can actually trade

### 4. N_Eff displayed as text header
- "Effective number of bets: X.XX" shown above the sizing table
- Removed from table columns (same value for every row was redundant)

### 5. Reactive Chain Updated (server.R)
- `Trades.Data.predict_filtered` feeds into sizing, correlations, AND orders
- Sizing reactive returns a list; N_Eff rendered as text, table rendered as gvisTable
- All downstream tables react to Execute checkbox changes

### 6. UI Layout Updated (ui.R)
- Section headers: "Signals", "Portfolio Sizing", "Correlation Matrix", "Execution Orders"
- N_Eff text output above sizing table
- Horizontal rules between each section

## Files Changed
- `Code/GUI.R` - Simplified `G.Trades.Table.predict()`, added `G.Trades.Table.sizing()`, added tradable filter to `G.Trades.Table.correlations()`
- `Platform/server.R` - New sizing reactive (list output), N_Eff text output, correlations uses filtered data
- `Platform/ui.R` - Trades tab layout: signals → sizing (with N_Eff text) → correlation → orders

## Reactive Data Flow
```
Predict.Data.predict()
  → G.Trades.Table.predict()  [signals with Execute checkbox, tradable only]
  → Trades.Data.predict_filtered  [filters by Execute == TRUE]
    → G.Trades.Table.sizing()        [eigenvalue sizing, tradable only, returns list]
      → Trades.Text.n_eff             [N_Eff as text header]
      → Trades.Table.sizing           [instrument/direction/weight/notional table]
    → G.Trades.Table.correlations()  [correlation matrix, tradable only]
    → G.Trades.Table.orders()        [execution orders]
```

## Known Technical Debt
- `tradable_instruments` list is hardcoded in 3 functions: `G.Trades.Table.predict()`, `G.Trades.Table.sizing()`, `G.Trades.Table.correlations()`
- Plan: move to database table (e.g. `instrument.tradable = 1`) and read once

## Next Steps
- Move tradable instruments list to database table
- Test `trade_orders.py` live with real orders (dry_run=False)
- Verify OCA linkage in IB Gateway
- Test for account 2
- Enable `Execute_Orders.py` for full automation
