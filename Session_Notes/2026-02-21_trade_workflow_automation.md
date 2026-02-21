# Session Notes: Trade Workflow Automation
**Date**: 2026-02-21
**Status**: Ready for review

## Summary
Implemented automated workflow from trade signals to order generation and execution matching. Added 4 new functions to `Book.R` totaling ~820 lines.

---

## Changes Made

### 1. Book.R - New Functions

#### B.generateOrders() (~290 lines)
**Purpose**: Convert signals to sized orders for IB execution

**Usage**:
```r
result <- B.generateOrders(
    dat_predict = NULL,           # Load from G.Predict.Data.predict() if NULL
    risk_per_bet_pct = 0.5,       # 0.5% risk per effective bet
    max_daily_risk_pct = 5,       # 5% max daily risk
    correlation_adjustment = 0,    # 0=historical, 0.2=inflate 20%
    account_ids = c(1, 2),        # Generate for both accounts
    export_csv = TRUE             # Export CSV for Execute_Orders.py
)
```

**Key features**:
- Uses `V.portfolioSizing()` for eigenvalue-based N_effective sizing
- Gets latest NAV from `book_nav` table (WHERE date < TODAY)
- Scales orders proportionally by each account's NAV
- Exports CSV to: `Orders/Combined/{YYYY-MM}/{YYYY-MM-DD}/combined_orders_{account}_{date}-{hour}.csv`

**Returns**:
```r
list(
    orders_all,           # All orders combined
    orders_by_account,    # List keyed by account_id
    nav_data,             # NAV used per account
    sizing_metadata,      # n_effective, total_risk_pct, correlation_matrix
    exported_files,       # Paths to exported CSVs
    signals_used          # Signals with sizing info
)
```

---

#### B.matchLegsToTrades() (~320 lines)
**Purpose**: Match execution legs to trades (entry/exit detection)

**Usage**:
```r
summary <- B.matchLegsToTrades(
    lookback_days = 7,
    price_tolerance_pct = 0.01,        # 1% tolerance for target/stop
    timestamp_tolerance_minutes = 60    # 60 min for entry matching
)
```

**Matching logic**:
| Type | Detection |
|------|-----------|
| ENTRY | Matches signal instrument_id + buy_sell, timestamp within tolerance |
| TARGET | Closes live trade, price within 1% of target |
| STOP | Closes live trade, price within 1% of stop |
| MATURITY | Closes live trade, date >= entry + 7 days |

**Confidence levels**: HIGH, MEDIUM, LOW, VERY_LOW, NONE

**Returns**: Tibble with columns:
- `leg_id, account_id, pair, timestamp, buy_sell, size, price`
- `match_type, confidence, suggested_trade_id, suggested_strategy_id`
- `price_diff_pct, time_diff_minutes, expected_target, expected_stop`
- `instruction` (R code to execute the match)

---

#### B.confirmLegMatch() (~130 lines)
**Purpose**: Execute matches after user review

**Usage**:
```r
# Preview only (default)
B.confirmLegMatch(match_summary, confirm_types = c("HIGH"), dry_run = TRUE)

# Actually execute
B.confirmLegMatch(match_summary, confirm_types = c("HIGH", "MEDIUM"), dry_run = FALSE)
```

**Actions**:
- For ENTRY: calls `B.createNewTradeIDFromLegs()`
- For TARGET/STOP/MATURITY: calls `B.closeTradeFromLegs()`

---

#### B.confirmSingleLeg() (~70 lines)
**Purpose**: Manual single-leg confirmation when automatic matching fails

**Usage**:
```r
# Create trade from entry leg
B.confirmSingleLeg(leg_id=123, match_type="ENTRY", strategy_id=1, tp_pct=0.02)

# Close trade from exit leg
B.confirmSingleLeg(leg_id=456, match_type="TARGET", trade_id=789)
```

---

### 2. CLAUDE.md Updates
- Added new functions to "Key Functions Reference" section
- Updated "Next Priorities" section to reflect completion status
- Documented the new workflow

---

## Workflow Diagram

```
┌──────────────────────────────────────────────────────────────────┐
│ 1. SIGNAL GENERATION                                             │
│    G.Predict.Data.predict() → B.generateOrders()                 │
│                                    ↓                             │
│                              CSV files (per account)             │
└──────────────────────────────────────────────────────────────────┘
                                     ↓
┌──────────────────────────────────────────────────────────────────┐
│ 2. EXECUTION                                                     │
│    Execute_Orders.py reads CSV → places orders via IB TWS        │
└──────────────────────────────────────────────────────────────────┘
                                     ↓
┌──────────────────────────────────────────────────────────────────┐
│ 3. RECORDING                                                     │
│    Read_Executions.py → book_trade_leg (legs recorded)           │
└──────────────────────────────────────────────────────────────────┘
                                     ↓
┌──────────────────────────────────────────────────────────────────┐
│ 4. MATCHING                                                      │
│    B.matchLegsToTrades() → summary tibble for review             │
└──────────────────────────────────────────────────────────────────┘
                                     ↓
┌──────────────────────────────────────────────────────────────────┐
│ 5. CONFIRMATION                                                  │
│    B.confirmLegMatch(dry_run=FALSE) → book_trade + book_trade_map│
└──────────────────────────────────────────────────────────────────┘
```

---

## Files Changed

| File | Lines Added | Description |
|------|-------------|-------------|
| `Code/Book.R` | ~820 | 4 new functions |
| `CLAUDE.md` | ~20 | Documentation updates |

---

## Testing Checklist

- [ ] Load Book.R without errors: `source("Book.R")`
- [ ] Test B.generateOrders() with live signals
- [ ] Verify CSV format matches Execute_Orders.py expectations
- [ ] Test B.matchLegsToTrades() with historical legs
- [ ] Test B.confirmLegMatch() with dry_run=TRUE
- [ ] End-to-end test with a real small trade

---

## Next Steps

1. **Enable Execute_Orders.py**: Order placement is currently commented out (safety). Need to uncomment lines ~430-432
2. **Test with real execution**: Small test trade to verify full flow
3. **Account 2 FX filter**: Add parameter to filter out FX for account 2 (noted for future)

---

## Git Commands to Commit

```bash
cd /home/fls/Models/Ventura/HD
git add Code/Book.R CLAUDE.md
git commit -m "Add trade workflow automation: B.generateOrders, B.matchLegsToTrades, B.confirmLegMatch

- B.generateOrders: Convert signals to sized orders using V.portfolioSizing()
- B.matchLegsToTrades: Match execution legs to trades (entry/target/stop/maturity)
- B.confirmLegMatch: Execute matches after user review
- B.confirmSingleLeg: Manual single-leg confirmation helper

Uses eigenvalue-based N_effective for correlation-aware sizing.
Exports CSV compatible with Execute_Orders.py.

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"

git push
```
