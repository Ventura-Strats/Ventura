# 2026-03-21: A.filterInstruments() refactor + Trim_Machine_Status.sh

## Summary

Two changes this session:

### 1. Renamed `A.tradableInstruments()` to `A.filterInstruments(use_case)` with 5 use cases

The old function applied a single hardcoded filter (`!bond + training + trading + gs`). The new function centralises all instrument filtering into one place with a `use_case` parameter:

| use_case | Filter | Purpose |
|----------|--------|---------|
| `"all"` | No filter (all INSTRUMENTS rows) | Technicals computation, data quality checks |
| `"training"` | `use_for_training == 1` | Model training data loading |
| `"predict"` | `use_for_trading == 1` | Live prices, live technicals, predictions, dashboard |
| `"exec"` | `use_for_trading_gs == 1` | Portfolio sizing, order generation, Trades tab |
| `"IB"` | `use_for_trading_ib == 1` | IB-specific: signal list, backtest filtering, calibration |

Returns a character vector of pair names (same as before).

### 2. Trim_Machine_Status.sh

Shell script that keeps only the last 3000 lines of each `.txt` file in Machine_Status directory. First run reduced 1.3GB / 13.7M lines to 18MB / 160K lines. Added to crontab on machine H at 23:04 daily.

## Files Changed

### A.filterInstruments definition
- `Code/Assets.R` — Replaced `A.tradableInstruments` with `A.filterInstruments(use_case)`. Also updated `A.instrumentsWithBadData` to use `A.filterInstruments("all")`.

### use_case = "exec" (was A.tradableInstruments)
- `Code/GUI.R` — `G.Trades.Table.correlations`, `G.Trades.Table.predict`, `G.Trades.Table.sizing`
- `Code/Book.R` — `B.generateOrders`

### use_case = "predict" (was `use_for_trading == 1`)
- `Code/GUI.R` — 5 dashboard display functions (lines ~1695, 2092, 2149, 2212, 2251)
- `Code/Technicals.R` — `T.getLivePrices` default, `T.prepareTechnicalsExec`
- `Scripts/Model/Predict.R` — prediction pair list
- `Scripts/Technicals/Price_Live.R` — live price fetch
- `Scripts/Technicals/Price_Live_Exec.R` — execution-time price fetch
- `Scripts/Technicals/Live.R` — live technicals
- `Scripts/Scenario/Calc_Bumped.R` — scenario analysis

### use_case = "all" (was `use_for_training + use_for_trading >= 1`)
- `Code/Technicals.R` — `T.addTradingDataAllPossibilities`
- `Code/Ventura.R` — `V.modelPredict`, `V.modelPredictBacktest` defaults
- `Scripts/Technicals/Recent.R` — batch technicals
- `Scripts/Technicals/Full_Old.R` — archived script

### use_case = "training" (was `use_for_training == 1`)
- `Code/Engine.R` — `prepareBaseData`
- `Code/Technicals.R` — training data filter in `T.getTechnicals`

### use_case = "IB" (was `use_for_trading_ib == 1`)
- `Code/Ventura.R` — two `filterProbabilities` functions, backtest filtering (2 sites)

### Machine status trimming
- `Scripts/Maintenance/Trim_Machine_Status.sh` — new script
- `Scripts/Crontab/Crontab_Generator_user.txt` — replaced old Clean_Track_System_Files.R, enabled KillScript Predict.R jobs

### Not changed (intentional)
- SQL strings in Technicals.R, Ventura.R, Static_Tables.R, PowerLaw.R — raw SQL can't call R functions
- Python scripts — different language
- Commented-out diagnostic code in Technicals.R

## Usage

```r
# Get pairs for prediction
A.filterInstruments("predict")

# Get pairs for execution/sizing
A.filterInstruments("exec")

# Default is "exec"
A.filterInstruments()

# Get instrument_ids (combine with INSTRUMENTS)
filter(INSTRUMENTS, pair %in% A.filterInstruments("IB"))$instrument_id
```

## Testing Checklist

- [ ] Dashboard loads without error (all 5 GUI.R call sites)
- [ ] Predict.R runs successfully for all strategy batches
- [ ] Price_Live.R fetches prices without hanging
- [ ] Live.R computes technicals for correct instrument set
- [ ] B.generateOrders() produces correct orders (exec use_case)
- [ ] Backtest V.readBacktest() completes without error
- [ ] Verify instrument counts: `length(A.filterInstruments("all"))` vs `"predict"` vs `"exec"` vs `"IB"`

## Next Steps

- Review the use_case mapping in detail — especially whether "IB" sites should actually be "exec"
- Consider whether "all" should mean "all rows" or "training OR trading" (currently all rows)
- Python scripts still filter directly — could add equivalent function to init.py later
- SQL queries still use raw column filters — acceptable, just needs to stay in sync with DB flags
