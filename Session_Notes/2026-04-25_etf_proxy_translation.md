# Session Notes: 2026-04-25 - ETF Proxy Translation

## Summary of Changes

### 1. New function: B.resolveETFProxy() (Book.R)
- **Purpose**: Translates index instrument signals (e.g., SPXUSD, NDXUSD, RUTUSD) into their tradable ETF proxy equivalents (SPY, QQQ, IWM) for instruments where `trade_instrument_type == "ETF"` in INSTRUMENTS
- **Data source**: `static_etf_proxy` table maps instrument_id to ETF conid, tick_size, ib_symbol, lot_size
- **What it does**:
  - Overrides `conid` with ETF proxy conid (e.g., 756733 for SPY instead of ES future)
  - Overrides `tick_size` with ETF tick_size (e.g., 0.01 for SPY)
  - Sets `future_id = NA` (not trading the future)
  - Converts `sized_notional` from index units to ETF share count by dividing by current ETF price
- **ETF price source**: `px_position_{1,2}_last.csv` files from IB_Account_Data.py, filtered to `secType == "STK"`, averaged across accounts
- **Safety**: Warns if no ETF price is found for a proxy instrument, leaves `sized_notional` unconverted in that case
- **Early exits**: Returns input unchanged if no `trade_instrument_type` column or no ETF rows present

### 2. Integrate into B.generateOrders() — addInstrumentDetails()
- `B.resolveETFProxy()` piped at the end of `addInstrumentDetails()`, after futures/spot conid resolution
- Affects both direct `B.generateOrders()` calls and dashboard display via `G.Trades.Table.orders`

### 3. Integrate into B.sendOrder() — resolveInstrument()
- After existing futures resolution, checks `trade_instrument_type == "ETF"` and overrides conid/tick_size from `static_etf_proxy`
- No price conversion needed here since `B.sendOrder()` receives `size` directly in shares from caller
