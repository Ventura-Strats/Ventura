# Session Notes - 2026-03-22: psi_60 Bug Fix & Technical Features Documentation

## Summary of Changes

### 1. Technical Features Documentation (new file)
- **File**: `Documentation/Technical_Features.md`
- Catalogues all ~237 features produced by the `T.calcTechnicals()` pipeline
- 26 functions documented with column names, descriptions, TECH_PARAM dependencies
- 14 columns are forward-looking targets (NextMonthTrend: 2, MaxMoveInNextMonths: 12), leaving ~223 candidate predictors
- Identified bugs, redundancy candidates, and design questions for upcoming feature review

### 2. psi_60 Bug Fix
- **Bug**: `T.addSimpleStuff` line 1021 -- `psi_60` used `lag(close, 20)` instead of `lag(close, 60)`, making it an exact duplicate of `psi_20`
- **R code fix**: Changed to `lag(close, 60)` in `Technicals.R`
- **DB fix**: Direct SQL UPDATE of all 974,336 rows in `histo_technicals_dbl` (feature_id = 67) across 146 instruments, using MySQL 8.0 `LAG()` window function:
  ```sql
  UPDATE histo_technicals_dbl T67
  JOIN (
      SELECT instrument_id, date,
             close AS close_today,
             LAG(close, 60) OVER (PARTITION BY instrument_id ORDER BY date) AS close_60
      FROM histo_px_daily
  ) PX ON PX.instrument_id = T67.instrument_id AND PX.date = T67.date
  JOIN histo_technicals_dbl V
    ON V.instrument_id = T67.instrument_id
    AND V.date = T67.date
    AND V.feature_id = 2
  SET T67.value = LN(PX.close_today / PX.close_60) / V.value
  WHERE T67.feature_id = 67
    AND PX.close_60 IS NOT NULL AND PX.close_60 > 0
    AND PX.close_today > 0
    AND V.value IS NOT NULL AND V.value > 0;
  ```
- **Verified**: Spot-checked instrument 5, date 2026-03-20 -- stored value matches `ln(14833.34 / 14986.3) / 0.13648 = -0.07517` to 17 decimal places
- **No manual follow-up needed**: Tonight's `Recent.R` (00:02) uses fixed R code; `Load_Tech.R` (01:40) rebuilds `Technicals.RData` from corrected DB

### 3. User Changes (committed together)

#### V.removeUselessFeatures -- save_to_db parameter
- **File**: `Code/Ventura.R`
- New `save_to_db = FALSE` parameter added to `V.removeUselessFeatures()`
- When `TRUE`: calls `D.SQL(sprintf(res$sql_delete, strat_id))` and `D.replaceDataIntoTable("strategy_feature", res$dat_feature)` automatically
- When `FALSE` (default, backward-compatible): prints instructions for manual execution

#### Remove_Features.R -- crontab-ready
- **File**: `Scripts/Model/Remove_Features.R`
- Now calls `V.removeUselessFeatures(model_to_do, "", TRUE)` with `save_to_db=TRUE`
- Ready to be added to crontab scheduler for automated feature selection

#### V.readBacktest enhancements
- **File**: `Code/Ventura.R`
- Reads `_stitched.csv` backtest files instead of plain `.csv`
- Added `gross_pnl` metric to yearly and total summary: `mean(buy_sell * (px_exit - px_entry) / (t_up - px_entry))`
- Whitespace cleanup and comment alignment throughout `V.readBacktest`

#### Crontab kill timing adjustment
- **File**: `Scripts/Crontab/Crontab_Generator_user.txt`
- `Predict.R` kill jobs moved from `:59` to `:05` next hour (e.g., 04:59 -> 05:05)
- Gives prediction scripts 6 extra minutes before forced kill

#### Feature_Selection.md doc fix
- **File**: `Documentation/Feature_Selection.md`
- Corrected `D.execute()` to `D.SQL()` in the usage example

## Feature Review Priorities (from Technical_Features.md)

### Bugs
1. ~~**psi_60 lag bug**~~ -- FIXED
2. **Skewness/kurtosis on prices not returns** (`T.addSkewKurtosis`) -- measures trend shape, not distribution shape

### Redundancy Candidates
3. `autocor_close` vs `mr_st`/`mr_lt` -- all measure lag-1 return autocorrelation
4. `volstdiff`/`volltdiff` vs `vol_st_20` -- overlapping vol momentum
5. 30 spline inflection levels + 10 histogram levels = 40 S/R features
6. `sup_exp_close` vs `hurst_st` -- both detect trend persistence

### Design Questions
7. `bb_up`/`bb_dn` -- absolute prices, model feature or dashboard-only?
8. `engulfing` -- 0/1 but no direction encoding
9. `weekday` -- integer encoding implies ordering
10. `perf_ovn_*` -- likely near-zero variance for FX (24h market)
11. Filled high/low = close creates zero-range days affecting vol estimators
12. `histo_pareto` staleness -- how often refreshed?
13. Spline levels l_4/l_5, r_4/r_5 -- are far-away levels used?
14. `close_spline` normalises by vol_st vs `close_spline_slow` by vol_lt

## Testing Checklist

- [x] `psi_60` R code fix verified (line 1021: `lag(close, 60)`)
- [x] SQL UPDATE completed -- 974,336 rows updated
- [x] Manual arithmetic verification passed (instrument 5, 2026-03-20)
- [x] Row count unchanged after UPDATE (974,336)
- [x] `psi_60` and `psi_20` now produce different values
- [ ] `Recent.R` runs tonight with fixed code (automatic, 00:02)
- [ ] `Load_Tech.R` rebuilds Technicals.RData (automatic, 01:40)
- [ ] Model retraining picks up corrected psi_60 (next `Train.R` cycle)

## Next Steps

- **Feature review**: Use `Technical_Features.md` as checklist for systematic review
- **Feature importance analysis**: Check which of the ~223 features actually contribute to model performance (especially the redundancy candidates)
- **Remove_Features.R crontab**: Add to scheduler once validated manually
- **SkewKurtosis fix**: Decide whether to compute on returns instead of prices
