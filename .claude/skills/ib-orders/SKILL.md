---
name: ib-orders
description: Interactive Brokers trade execution workflow. Use when the user asks about placing orders, IB API, trade execution, B.generateOrders, trade_orders.py, Read_Executions.py, or trade matching.
---

# IB Trade Execution Workflow

## Infrastructure

- **IB Gateway**: Machine I (192.168.0.42), ports 7496 (account 1) / 7497 (account 2)
- **Python scripts**: `Scripts/Python/`

## End-to-End Flow

```
Predict.R signals
  -> B.generateOrders()        [R, sizes via portfolio optimization]
  -> CSV export
  -> Execute_Orders.py         [Python, sends to IB - NOT YET AUTOMATED]
  -> IB executes trades
  -> Read_Executions.py        [Python, reads fills every 5 min]
  -> B.matchLegsToTrades()     [R, matches fills to trades]
  -> B.confirmLegMatch()       [R, creates/closes trade records]
```

## Step 1: Generate Orders (R)

```r
source("Init.R"); I.loadModules()
result <- B.generateOrders()  # uses latest predictions
```

`B.generateOrders(dat_predict, risk_per_bet_pct, max_daily_risk_pct, correlation_adjustment, account_ids, export_csv, export_path)` — Book.R ~line 404. Uses V.portfolioSizing() for eigenvalue sizing, scales by account NAV from book_nav. Default accounts: c(1,2). Returns: orders_all, orders_by_account, nav_data, sizing_metadata.

## Step 2: Place Orders (Python)

### Interactive (trade_orders.py) — RECOMMENDED

```python
import sys; sys.path.insert(0, '/home/fls/Models/Ventura/HD/Scripts/Python')
from trade_orders import show_live_trades, exit_orders, exit_orders_all

show_live_trades(account_id=1)                          # view live trades
exit_orders(trade_id=42, account_id=1)                  # dry_run=True default
exit_orders(trade_id=42, account_id=1, dry_run=False)   # LIVE
exit_orders_all(account_id=1, dry_run=False)            # all live trades

# Full control via TradeOrderManager
from trade_orders import TradeOrderManager
mgr = TradeOrderManager(account_id=1, client_id=10)
mgr.connect()
info = mgr.get_trade_info(trade_id=42)
mgr.place_exit_orders(info)
mgr.place_entry_order(info)
mgr.cancel_exit_orders(info)
```

### Automated (Execute_Orders.py) — NOT YET ENABLED
- Reads CSV from B.generateOrders()
- Order placement currently commented out

## Step 3: Read Executions (Python)

`Read_Executions.py` — runs every 5 min via crontab on H. Uses IB API reqExecutions (past ~24h only). Writes to book_trade_leg table.

**Gap**: Misses GTC fills from overnight/weekend. Flex Query reconciliation planned but not implemented.

## Step 4: Match & Confirm (R)

```r
matches <- B.matchLegsToTrades(lookback_days = 7)
print(matches)
B.confirmLegMatch(matches, confirm_types = c("ENTRY", "TARGET"))

# Manual single-leg confirmation
B.confirmSingleLeg(leg_id = 123, match_type = "ENTRY",
                   strategy_id = 1, tp_pct = 0.02, dry_run = TRUE)
```

### Match Types
- **ENTRY**: New trade -> B.createNewTradeIDFromLegs()
- **TARGET**: Take-profit hit -> closes trade
- **STOP**: Stop-loss hit -> closes trade
- **MATURITY**: Max duration -> closes trade

## Key Tables

- `book_trade_leg` — Individual execution fills from IB
- `book_trade` — Trade records (entry + exit paired)
- `book_nav` — Account NAV snapshots (from IB_Account_Data.py)

## Flex Query Tokens (for future reconciliation)

- Account 1: 106713781439309215279050
- Account 2: 5179386948882217289232
- Functions: `B.readTradesFromIB(account_id, query_type)`, `B.processTradesFromIB(save_to_db=TRUE)`

## Current Status

| Component | Status |
|-----------|--------|
| B.generateOrders() | Working |
| trade_orders.py | Working, needs live testing (dry_run only) |
| Execute_Orders.py | Not yet enabled |
| Read_Executions.py | Working (every 5 min) |
| B.matchLegsToTrades() | Working |
| Flex Query reconciliation | Not implemented |
