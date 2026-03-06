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
- Runs `V.portfolioSizing()` which includes antagonist signal netting
- Groups results by instrument (merging multiple strategies on same asset)
- Returns `gvisTable` with columns: Instrument, Direction, N_Eff, Weight_Pct, Sized_Notional
- Only shows trades with weight > 0 (netted-out trades excluded)

### 3. Reactive Chain Updated (server.R)
- `Trades.Data.predict_filtered` feeds into sizing, correlations, AND orders
- Previously correlations used unfiltered data - now uses filtered
- Order: sizing → correlations → orders (all react to Execute checkbox changes)

### 4. UI Layout Updated (ui.R)
- Added `h4()` section headers: "Signals", "Portfolio Sizing", "Correlation Matrix", "Execution Orders"
- Sizing uses `htmlOutput` (for `gvisTable` rendering)
- Horizontal rules between each section

## Files Changed
- `Code/GUI.R` - Simplified `G.Trades.Table.predict()`, added `G.Trades.Table.sizing()`
- `Platform/server.R` - New sizing reactive, correlations now uses filtered data, reordered
- `Platform/ui.R` - Trades tab layout: signals → sizing → correlation → orders

## Reactive Data Flow
```
Predict.Data.predict()
  → G.Trades.Table.predict()  [signals with Execute checkbox]
  → Trades.Data.predict_filtered  [filters by Execute == TRUE]
    → G.Trades.Table.sizing()        [eigenvalue sizing on selected trades]
    → G.Trades.Table.correlations()  [correlation matrix on selected trades]
    → G.Trades.Table.orders()        [execution orders on selected trades]
```

## Next Steps
- Test `trade_orders.py` live with real orders (dry_run=False)
- Verify OCA linkage in IB Gateway
- Test for account 2
- Enable `Execute_Orders.py` for full automation
