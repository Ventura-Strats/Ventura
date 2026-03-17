# Session Notes: 2026-03-16 - Tradable Instruments List Cleanup

## Summary

Replaced 4 hardcoded `tradable_instruments` lists with a single `A.tradableInstruments()` function that reads from the INSTRUMENTS table. The tradable list is now driven by DB flags (`use_for_training`, `use_for_trading`, `use_for_trading_gs`) instead of being maintained in code.

## Changes Made

### New Function: `A.tradableInstruments()` (Assets.R)
```r
A.tradableInstruments <- function() {
    INSTRUMENTS %>%
        filter(
            asset_class != "bond",
            use_for_training == 1,
            use_for_trading == 1,
            use_for_trading_gs == 1
        ) %>%
        .$pair
}
```

### Replaced Hardcoded Lists
| File | Function | Line |
|------|----------|------|
| GUI.R | `G.Trades.Table.correlations` | 2452 |
| GUI.R | `G.Trades.Table.predict` | 2557 |
| GUI.R | `G.Trades.Table.sizing` | 2621 |
| Book.R | `B.generateOrders` | 441 |

### Left Untouched
- `Ventura.R:2882` - uses `use_for_trading_ib == 1` and returns `instrument_id` (not `pair`). Different filter for backtesting purposes.

## Usage

To add/remove instruments from trading:
1. Update `use_for_trading_gs` in the `static_instrument` DB table
2. Re-run `Static_Tables.R` to refresh the INSTRUMENTS table
3. All 4 functions automatically pick up the change

## Testing Checklist
- [ ] Run `Static_Tables.R` to ensure INSTRUMENTS loads with `use_for_trading_gs` column
- [ ] Verify `A.tradableInstruments()` returns expected instrument list
- [ ] Check Trades tab in dashboard: Signals, Sizing, Correlation tables still work
- [ ] Check `B.generateOrders()` still filters correctly

## Next Steps
- Priority 4 from CLAUDE.md (Tradable Instruments List Cleanup) is now complete
