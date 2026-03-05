# Session Notes: 2026-03-04 - Trade Orders Module

## Summary of Changes

### 1. Correlation Floor at Zero (from late 2026-03-03)
- Added `floor_at_zero` parameter to `T.calcHistoricalCorrelationsMatrix()` (default `FALSE`)
- Enabled in sizing callers (`B.generateOrders`, `G.Trades.Table.predict`), not in display (`G.Trades.Table.correlations`)

### 2. Fixed G.Trades.Table.orders Dashboard Error
- `U.try()` was called with `default = NULL` instead of positional `f_rtn` parameter
- Removed the named argument

### 3. IB Gateway Move Documented
- IB API moved from machine H (34) to machine I (42) using IB Gateway
- Ports: 7496 (account 1), 7497 (account 2)
- `getIBPort()` default base of 7496 works correctly, no code change needed

### 4. New `trade_orders.py` Module (main deliverable)
Interactive Python module for placing IB orders directly from console.

**File**: `/HD/Scripts/Python/trade_orders.py`

**Key components**:
- `TradeInfo` dataclass - holds all trade details loaded from DB
- `TradeOrderManager` class - main interface for order placement
- Module-level convenience functions for quick use

**Exit orders** (target + stop as OCA pair):
- Looks up trade from DB: entry price, size, direction, target_pct, instrument details
- Calculates target and stop prices (matching Book.R formula)
- Places LMT at target + STP at stop, linked by OCA group (GTC)
- One fills → the other auto-cancels

**Entry orders**: Delegates to existing chase algorithm in `order_execution.py`

**Safety**: All functions default to `dry_run=True`

### Usage Examples

```python
import sys
sys.path.insert(0, '/home/fls/Models/Ventura/HD/Scripts/Python')
from trade_orders import show_live_trades, exit_orders, exit_orders_all

# See all live trades
show_live_trades(account_id=1)

# Dry run for one trade
exit_orders(1058, account_id=1)

# Actually place orders
exit_orders(1058, account_id=1, dry_run=False)

# All live trades at once
exit_orders_all(account_id=1, dry_run=False)

# Cancel exit orders for a trade
from trade_orders import TradeOrderManager
mgr = TradeOrderManager(account_id=1)
mgr.connect()
mgr.cancel_exit_orders(1058)
mgr.disconnect()

# Entry order with chase algo
mgr = TradeOrderManager(account_id=1)
mgr.place_entry_order(
    instrument_id=42, direction=1, size=25000,
    price=1.0850, price_limit=1.0870, dry_run=False
)
```

### Testing Checklist
- [x] `show_live_trades(1)` returns 4 live trades with correct details
- [x] `get_trade_info(1058)` returns correct CHFJPY SELL trade
- [x] `exit_orders(1058, dry_run=True)` shows correct target/stop/OCA
- [x] `exit_orders_all(1, dry_run=True)` processes all 4 trades
- [ ] `exit_orders(trade_id, dry_run=False)` - test live with one trade
- [ ] Verify OCA linkage in IB Gateway (cancel one → other cancels)
- [ ] Test for account 2

## Files Changed
- `Code/Technicals.R` - `floor_at_zero` parameter on `T.calcHistoricalCorrelationsMatrix()`
- `Code/Book.R` - `floor_at_zero = TRUE` in `B.generateOrders()`
- `Code/GUI.R` - `floor_at_zero = TRUE` in `G.Trades.Table.predict()`, fixed `U.try` in `G.Trades.Table.orders()`
- `Scripts/Python/trade_orders.py` - **NEW** - IB order placement module

## Next Steps
- Test `place_exit_orders(dry_run=False)` with a real trade
- Test for account 2
- Eventually integrate entry orders into the signal → execution workflow
- Enable `Execute_Orders.py` for full automation
