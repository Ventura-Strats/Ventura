# Technical Features Computed by T.calcTechnicals

This document catalogues every feature produced by the `T.calcTechnicals()` pipeline in `Technicals.R`, grouped by the function that creates them. The pipeline produces **~237 columns** on top of the base OHLC data. Two groups are **forward-looking targets** used for model training, not predictors.

Each section includes a brief description, the columns added, and review observations.

---

## Pipeline Overview

```
T.getHistoPx  -->  fillMissingOpen/HighLow
  -->  T.addSTVol  -->  T.addLTVol  -->  T.addAllSplines
  -->  T.addBollingerBandsFLS  -->  T.addSimpleStuff
  -->  T.addDateRelated  -->  T.addAssetClass  -->  T.addFXPriority
  -->  T.addTechnicalZones  -->  T.addVolDifferences
  -->  T.addLTVolAverage  -->  T.addSkewKurtosis
  -->  T.addMeanReversionMomentum  -->  T.addRecentHiLo
  -->  T.addBiggestRecentCandle  -->  T.addEngulfing
  -->  T.addOvernightStats  -->  T.addCandleShadowsMeasure
  -->  T.addNewHighsOrLows  -->  T.addRecentTrend
  -->  T.addNextMonthTrend  -->  T.addAutoCorrelation
  -->  T.addSplineDivergence  -->  T.addMaxMoveInNextMonths
  -->  T.addPareto  -->  T.addSuperExponentiality
  -->  finalCleanData
```

### Global Config (from TECH_PARAM table)

| Parameter | Used by |
|-----------|---------|
| `vol_days_st` | STVol, SkewKurtosis, MeanRevMom, VolDifferences |
| `vol_days_lt` | LTVol, AllSplines, SkewKurtosis, MeanRevMom, VolDifferences |
| `vol_n_periods_year_D` | STVol, LTVol, BollingerBands, LTVolAverage, VolDifferences, Spline |
| `bb_width` | AllSplines, BollingerBands |
| `spline_nb_points` | AllSplines, TechnicalZones |
| `spline_df_slow/medium/fast` | Spline (via AllSplines) |
| `nb_years_vol_lt_decile` | AllSplines, LTVolAverage, TechnicalZones |
| `n_periods_year` | calcTechnicals (row count calculation) |
| `autocorrelation_n_days` | AutoCorrelation |

---

## 0. Data Preparation

### T.fillMissingOpenWithPreviousClose (line 4678)
Fills NA opens with previous day's close. Saves original as `open_na` (dropped in finalCleanData).

### T.fillMissingHighAndLowWithClose (line 4657)
Fills NA highs/lows with close. Saves originals as `high_na`, `low_na` (dropped in finalCleanData).

> **Observation**: Filling high/low with close implicitly says "the range was zero" on that day. This can distort range-based features (Yang-Zhang vol, Parkinson vol, candle shadow measures, hi_lo). Consider whether these filled days should instead be excluded from rolling windows, or flagged so downstream functions can skip them.

---

## 1. T.addSTVol (line 1221) -- Short-Term Volatility

Yang-Zhang volatility estimator (`TTR::volatility()`) over a short rolling window, annualised.

| Column | Description |
|--------|-------------|
| `vol_st` | Short-term annualised Yang-Zhang volatility |

---

## 2. T.addLTVol (line 449) -- Long-Term Volatility

Same estimator as STVol but with a longer window.

| Column | Description |
|--------|-------------|
| `vol_lt` | Long-term annualised Yang-Zhang volatility |

> **Observation**: `vol_lt` is the workhorse normaliser for almost every other feature. Anything that makes `vol_lt` unreliable (data gaps, filled high/low = close) propagates everywhere. Worth adding a data-quality check on `vol_lt` stability.

---

## 3. T.addAllSplines (line 1) -- Cubic Smoothing Splines

Fits cubic smoothing splines at three speeds (slow, medium, fast) to `tp = (close + high + low) / 3`. Each speed has a configurable degrees-of-freedom (`spline_df_slow/medium/fast` in TECH_PARAM). Also computes a custom MACD from the spline ratios.

| Column | Description | Count |
|--------|-------------|-------|
| `spline_slow/medium/fast` | Smoothed spline value at each speed | 3 |
| `slope_slow/medium/fast` | `log(spline / lag(spline)) / (vol_lt * sqrt(1/n_per_year))` | 3 |
| `accel_slow/medium/fast` | `slope - lag(slope)` | 3 |
| `slope_change_slow/medium/fast` | Detects slope sign flip: `sign(sign(slope) - sign(lag(slope)))` | 3 |
| `n_slope_slow/medium/fast` | Days since last slope sign change | 3 |
| `lvl_spline_l_1..5_slow/medium/fast` | 5 nearest spline inflection points below spot (vol-normalised log distance) | 15 |
| `lvl_spline_r_1..5_slow/medium/fast` | 5 nearest spline inflection points above spot (vol-normalised log distance) | 15 |
| `macdfls` | `spline_fast / spline_medium - 1` | 1 |
| `macdcross` | Sign change in macdfls | 1 |

**Subtotal: 47 columns**

> **Observation**: This is the single largest feature block. The inflection-level features (30 columns) encode support/resistance from the spline shape. Worth verifying whether the random forest actually uses the further-away levels (l_4, l_5, r_4, r_5) -- if not, trimming these would cut 12-18 columns of noise. Also, `slope` normalises by `vol_lt * sqrt(1/n_per_year)` -- the square root suggests this converts annual vol to per-period vol, which is correct, but worth confirming the intent.

---

## 4. T.addBollingerBandsFLS (line 208) -- Custom Bollinger Bands

Bollinger bands centred on `spline_medium` (not a simple moving average), using `vol_st` for the band width.

| Column | Description |
|--------|-------------|
| `bb_up` | `spline_medium * (1 + bb_width * vol_st / sqrt(n_per_year))` |
| `bb_dn` | `spline_medium * (1 - bb_width * vol_st / sqrt(n_per_year))` |

> **Observation**: These are absolute price levels, not normalised. They don't appear to be directly used as model features -- they might only be for charting. If they are fed to the model, the absolute price level would vary by instrument and time, which the tree can't generalise from. Confirm whether the model consumes these or only the dashboard does.

---

## 5. T.addSimpleStuff (line 1005) -- Returns, Ratios, Gaps

Basic vol-normalised log returns at multiple horizons, plus OHLC ratios and vol dynamics.

| Column | Description |
|--------|-------------|
| `psi_1` | 1-day log return / vol_lt |
| `psi_5` | 5-day log return / vol_lt |
| `psi_20` | 20-day log return / vol_lt |
| `psi_60` | **BUG**: computes `log(close / lag(close, 20))` (lag 20, not 60) / vol_lt |
| `psi_130` | 130-day (6-month) log return / vol_lt |
| `psi_260` | 260-day (1-year) log return / vol_lt |
| `open_close` | `log(open / close) / vol_lt` |
| `high_close` | `log(high / close) / vol_lt` |
| `low_close` | `log(low / close) / vol_lt` |
| `vol_ratio` | `log(vol_st / vol_lt)` |
| `vol_ratio_diff_vs_yesterday` | Daily change in vol_ratio |
| `gap_open` | `log(open / lag(close,1)) / vol_lt` -- overnight gap |
| `psi_5_yesterday` | `lag(psi_5, 1)` |
| `volstdiff` | `vol_st - lag(vol_st, 20)` |
| `volltdiff` | `vol_lt - lag(vol_lt, 20)` |

**Subtotal: 15 columns**

> **BUG**: `psi_60` uses `lag(close, 20)` instead of `lag(close, 60)`. This makes it a duplicate of `psi_20`. Should be `lag(close, 60)` for a 3-month return. Needs fix.
>
> **Observation**: `volstdiff` and `volltdiff` are raw differences in annualised vol (e.g. 0.12 - 0.10 = 0.02). Since vol magnitudes differ across instruments (FX ~8% vs indices ~20%), these are not comparable cross-asset. Consider normalising: `log(vol_st / lag(vol_st, 20))` instead (which is already partially captured by `vol_st_20` in VolDifferences). One of these is likely redundant.

---

## 6. T.addDateRelated (line 288) -- Calendar Features

| Column | Description |
|--------|-------------|
| `weekday` | Integer 1 (Mon) to 7 (Sun) |
| `days_to_eom` | Business days until end of month |

> **Observation**: `weekday` is an integer, so the model treats Monday(1) as "less than" Friday(5). One-hot encoding or cyclical encoding (sin/cos) would be more appropriate for a tree-based model. Though random forests can split on integers, they can't naturally capture "Monday and Friday are similar" without multiple splits. Also consider: is day-of-week actually predictive after controlling for everything else? Feature importance will tell.

---

## 7. T.addAssetClass (line 97) -- Asset Class Label

| Column | Description |
|--------|-------------|
| `asset_class` | Factor: `fx_dm`, `fx_em`, `index`, `metal`, `yield`, `bond` |

> **Observation**: This is a categorical grouping feature. It lets the model learn different behaviour by asset class. Sensible. Since `bond` instruments are filtered out by `A.filterInstruments`, the `bond` level is effectively unused.

---

## 8. T.addFXPriority (line 417) -- Currency Reference Direction

| Column | Description |
|--------|-------------|
| `ccy_1_reference` | 1 = ccy_1 is the "reference" currency, -1 = ccy_2 is, 0 = neither clearly |

> **Observation**: This encodes the FX quoting convention so the model can interpret direction consistently. Hardcoded pair-by-pair overrides. If new FX pairs are added, this function needs manual updating. Worth checking whether pairs like CADJPY, CADCHF, GBPCHF, GBPCAD have correct assignments.

---

## 9. T.addTechnicalZones (line 1446) -- Support/Resistance Levels

Builds a price histogram from historical OHLC data and finds the most-visited price levels. Returns the 5 nearest levels above and below current price, vol-normalised.

| Column | Description |
|--------|-------------|
| `lvl_l_1` .. `lvl_l_5` | 5 nearest support levels below spot (vol-normalised log distance) |
| `lvl_r_1` .. `lvl_r_5` | 5 nearest resistance levels above spot (vol-normalised log distance) |

**Subtotal: 10 columns**

> **Observation**: This is conceptually similar to the spline inflection levels (`lvl_spline_*`). The difference is these are based on volume-at-price (time spent at each level), while the spline ones are geometric turning points. Both encode "where are the nearest barriers". Check feature importance to see if one set dominates the other -- if so, the weaker set may just be adding noise. 10 + 30 = 40 level-based features is a lot.

---

## 10. T.addVolDifferences (line 1802) -- Volatility Estimator Comparisons

Computes multiple volatility estimators (close-to-close, Parkinson, Garman-Klass, Rogers-Satchell, GK-YZ) and takes log-ratios between them. Also computes MAD and up-day vs down-day vol ratios.

| Column | Description |
|--------|-------------|
| `vol_ratio_up_dn_st` | `log(vol of up-days / vol of down-days)`, ST window |
| `vol_ratio_up_dn_lt` | Same, LT window |
| `vol_c_vs_yz` | `log(close-to-close vol / Yang-Zhang vol_st)` |
| `vol_0_vs_trend` | `log(zero-mean vol / close-to-close vol)` -- large when mean return != 0 |
| `vol_hilo_vs_c` | `log(Parkinson vol / close-to-close vol)` |
| `vol_gk_vs_c` | `log(Garman-Klass vol / close-to-close vol)` |
| `vol_rs_vs_c` | `log(Rogers-Satchell vol / close-to-close vol)` |
| `vol_gkz_vs_c` | `log(GK-YZ vol / close-to-close vol)` |
| `mad_st_vs_vol_c` | `log(MAD_st / close-to-close vol)` |
| `mad_lt_vs_vol_lt` | `log(MAD_lt / vol_lt)` |
| `vol_st_20` | `log(vol_st / lag(vol_st, 20))` -- 20-day vol momentum |

**Subtotal: 11 columns**

> **Observation**: This is a rich and well-designed feature block. The log-ratios between estimators capture microstructure information: e.g. `vol_hilo_vs_c` being high means intraday ranges are wide relative to close-to-close moves (choppy, mean-reverting intraday). `vol_0_vs_trend` detects trending periods. `vol_ratio_up_dn` captures asymmetric vol. These are among the more theoretically grounded features.
>
> Note: 4 columns (`vol_hilo_vs_yz`, `vol_gk_vs_yz`, `vol_rs_vs_yz`, `vol_gkz_vs_yz`) are explicitly set to NULL inside the function -- they were removed at some point. The comparisons vs close-to-close were kept, the comparisons vs Yang-Zhang were dropped.

---

## 11. T.addLTVolAverage (line 467) -- Vol Regime

| Column | Description |
|--------|-------------|
| `vol_lt_avg` | Average of `vol_lt` over a long historical window |
| `vol_st_decile` | Percentile rank (0-1) of current `vol_st` in historical distribution |
| `vol_decile_diff` | `vol_st_decile - lag(vol_st_decile, 20)` |

> **Observation**: `vol_st_decile` is a useful regime indicator -- it tells the model whether current vol is historically high or low for this instrument. The 20-day change in decile captures vol acceleration. Good features. Requires at least 700 data points for the decile computation; newer instruments will get NAs.

---

## 12. T.addSkewKurtosis (line 1035) -- Distribution Shape

Rolling skewness, kurtosis, and Jarque-Bera normality test on close prices at ST and LT windows.

| Column | Description |
|--------|-------------|
| `kurtosis_st` | Rolling kurtosis, ST window |
| `skew_st` | Rolling skewness, ST window |
| `normality_st` | `log(Jarque-Bera statistic)`, ST window |
| `kurtosis_lt` | Rolling kurtosis, LT window |
| `skew_lt` | Rolling skewness, LT window |
| `normality_lt` | `log(Jarque-Bera statistic)`, LT window |

**Subtotal: 6 columns**

> **BUG / QUESTION**: The code has a comment noting these are computed on close **prices**, not **returns**. Skewness and kurtosis of a price series are dominated by the trend (a trending price series has positive skew regardless of return distribution). These should almost certainly be computed on daily log returns instead. The Jarque-Bera test on prices is similarly meaningless -- it's always going to reject normality for a price series. If this is intentional (treating it as a proxy for trend shape), it should be documented. If not, this is a bug that should be fixed.

---

## 13. T.addMeanReversionMomentum (line 619) -- Market Regime

Three measures of whether the market is trending or mean-reverting.

| Column | Description |
|--------|-------------|
| `hurst_st` | Hurst exponent (ST). > 0.5 = trending, < 0.5 = mean-reverting |
| `hurst_lt` | Hurst exponent (LT) |
| `momersion_st` | Fraction of positive daily changes in ST window |
| `momersion_lt` | Fraction of positive daily changes in LT window |
| `mr_st` | Regression coefficient of `r_t ~ r_{t-1}` (ST). Negative = mean-reverting |
| `mr_lt` | Regression coefficient of `r_t ~ r_{t-1}` (LT) |

**Subtotal: 6 columns**

> **Observation**: Three different measures of the same underlying concept (regime). Hurst exponent is the most robust theoretically but computationally expensive. Momersion is fast but crude. The regression coefficient is essentially lag-1 autocorrelation of returns (which overlaps with `autocor_close` from T.addAutoCorrelation). Verify whether `mr_st`/`mr_lt` and `autocor_close` are measuring the same thing with different windows. If so, one set is redundant.

---

## 14. T.addRecentHiLo (line 847) -- Rolling Highs/Lows

For each of {5, 20, 65, 130, 260} day windows: rolling max high, min low, days since each, position in range.

| Column pattern | Description | Count |
|----------------|-------------|-------|
| `lo_{n}` | `log(rolling_min_low / close) / vol_lt` | 5 |
| `n_lo_{n}` | Days since the rolling low | 5 |
| `hi_{n}` | `log(rolling_max_high / close) / vol_lt` | 5 |
| `n_hi_{n}` | Days since the rolling high | 5 |
| `pos_in_range_{n}` | Position within [lo, hi] range (0 to 1) | 5 |
| `hi_vs_hi_20` | Today's high_close vs 20-day high | 1 |
| `lo_vs_lo_20` | Today's low_close vs 20-day low | 1 |

**Subtotal: 27 columns**

> **Observation**: Solid feature set. `pos_in_range` is particularly useful -- it's a non-linear transformation of price relative to its recent range. The `n_hi`/`n_lo` features (time since high/low) are less commonly used but can capture "breakout age". 27 columns is a lot; check if the 5-day window features are adding value vs noise given daily rebalancing.

---

## 15. T.addBiggestRecentCandle (line 159) -- Extreme Candle Detection

Measures the largest daily candle (range and body) in recent windows.

| Column pattern | Description | Count |
|----------------|-------------|-------|
| `hi_lo` | Today's vol-normalised daily range `log(high/low) / vol_lt` | 1 |
| `hi_lo_{n}` | Max daily range in last n days | 5 |
| `n_hi_lo_{n}` | Days since the biggest range | 5 |
| `cl_op_{n}` | Max absolute open-to-close body in last n days | 5 |
| `n_cl_op_{n}` | Days since the biggest body | 5 |
| `hivslo_cl_mean_{n}` | `log(sum(upper_wick)^2 / sum(lower_wick)^2)` -- upper vs lower wick bias | 5 |

**Subtotal: 26 columns**

> **Observation**: The `hivslo_cl_mean` formula is unusual -- it squares the sums rather than summing the squares. `log(sum(h_c)^2 / sum(l_c)^2) = 2 * log(sum(h_c) / sum(l_c))`, so it's just a scaled version of the log-ratio of cumulative upper vs lower wicks. The squaring doesn't add information; it just doubles the value. Not a bug, but the name `_mean` is misleading.

---

## 16. T.addEngulfing (line 346) -- Engulfing Candle Pattern

Detects bullish/bearish engulfing candlestick patterns.

| Column | Description |
|--------|-------------|
| `engulfing` | 1 if engulfing pattern detected, 0 otherwise |
| `n_engulfing` | Days since last engulfing pattern |

> **Observation**: Classic candlestick pattern. The size filters (candle not too small, ratio not too extreme) are sensible to avoid false signals on doji-like candles. However, engulfing doesn't encode direction (bullish vs bearish). A +1/-1 encoding would be more informative than binary 0/1. Currently the model can only learn "an engulfing happened" but not "was it bullish or bearish" from this feature alone (it would need to combine with `psi_1` direction).

---

## 17. T.addOvernightStats (line 756) -- Overnight vs Intraday Decomposition

Separates cumulative overnight gaps from total performance.

| Column | Description |
|--------|-------------|
| `perf_ovn_5` | Overnight contribution to 5-day performance (vol-normalised) |
| `perf_ovn_20` | Same, 20 days |
| `perf_ovn_65` | Same, 65 days |
| `perf_ovn_130` | Same, 130 days |
| `perf_ovn_260` | Same, 260 days |

**Subtotal: 5 columns**

> **Observation**: Clever feature. Overnight gaps carry different information than intraday moves (news, macro events). If overnight is driving the total return, the move is news-driven; if intraday is, it's flow-driven. This distinction matters for momentum persistence. For FX (24h market), "overnight" is less meaningful than for equities/indices -- the gap is typically tiny. Check if these features have near-zero variance for FX pairs.

---

## 18. T.addCandleShadowsMeasure (line 236) -- Wick Analysis

Measures upper and lower candle shadows (wicks) and their ratios over rolling windows.

| Column pattern | Description | Count |
|----------------|-------------|-------|
| `up_shdw_{n}` | Cumulative upper shadow over n days | 5 |
| `dn_shdw_{n}` | Cumulative lower shadow over n days | 5 |
| `up_vs_dn_shdw_{n}` | `log(up_shadow / dn_shadow)` -- shadow asymmetry | 5 |
| `shdw_vs_body_{n}` | Shadow area vs total candle area (ratio) | 5 |
| `shdw_sd_{n}` | Std dev of total shadow measure over n days | 5 |

**Subtotal: 25 columns**

> **Observation**: Shadow asymmetry (`up_vs_dn_shdw`) is a good proxy for intraday rejection -- long upper shadows suggest selling pressure. The 5-window expansion (5, 20, 65, 130, 260) creates 25 columns from essentially 5 concepts. Feature selection should tell whether the longer windows (130, 260) add value here.

---

## 19. T.addNewHighsOrLows (line 665) -- Breakout Counting

| Column | Description |
|--------|-------------|
| `new_hi` | 1 if today's high is a new 65-day high |
| `new_lo` | 1 if today's low is a new 65-day low |
| `nb_new_hi_last_month` | Count of new 65-day highs in the last 20 days |
| `nb_new_lo_last_month` | Count of new 65-day lows in the last 20 days |

> **Observation**: The 65-day lookback is hardcoded. This feature partially overlaps with `pos_in_range_65` from RecentHiLo (if pos_in_range = 1, you're at a new high). The monthly count (`nb_new_hi_last_month`) adds value by capturing breakout persistence -- a stock making a new high every day for 2 weeks is different from one that touched it once.

---

## 20. T.addRecentTrend (line 899) -- Linear Regression on OHLC

For each of {10, 21, 130} day windows, fits a linear regression to intraday-expanded OHLC prices.

| Column pattern | Description | Count |
|----------------|-------------|-------|
| `trend_{n}_r2` | `atanh(R-squared)` of the regression -- trend quality | 3 |
| `trend_{n}_slope` | Regression slope, normalised by `price * vol_lt` | 3 |
| `trend_{n}_dw_s` | Durbin-Watson statistic -- serial correlation in residuals | 3 |

**Subtotal: 9 columns**

> **Observation**: Interesting approach -- melting OHLC into pseudo-intraday gives 4x the data points (open at 07:00, high/low at 14:00, close at 20:00). The atanh transform on R-squared maps [0,1] to [0, inf), which exaggerates differences near R^2=1 (very strong trends). This is sensible if you want to distinguish between "strong trend" and "very strong trend". The Durbin-Watson statistic captures whether the trend is smooth or choppy -- a smooth trend has DW near 0 (positive autocorrelation of residuals), a choppy one has DW near 2.
>
> **Question**: The synthetic intraday timestamps (open=07:00, high=14:00, low=14:00, close=20:00) place high and low at the same time. This means on up-days, the slope is biased by having the low appear at the same time as the high. Is this intentional? It means high and low contribute equal weight regardless of when they actually occurred.

---

## 21. T.addNextMonthTrend (line 700) -- **FORWARD-LOOKING TARGET**

Fits a linear regression to the **next** 21 days of prices. Used as a prediction target, not a predictor.

| Column | Description |
|--------|-------------|
| `trend_next_month_r2` | `atanh(R-squared)` of next month's trend |
| `trend_slope` | Slope of next month's trend, normalised |

> **Important**: This is a **target variable** for model training. It should never be used as a feature. Verify it is excluded from the feature set in `T.getTechnicals()` / `E.trainModel()`.

---

## 22. T.addAutoCorrelation (line 118) -- Return Autocorrelation

| Column | Description |
|--------|-------------|
| `autocor_close` | Rolling autocorrelation of daily log returns (lag-1) |

> **Observation**: Window size from `TECH_PARAM`. This measures the same concept as `mr_st`/`mr_lt` (mean reversion coefficient is essentially lag-1 autocorrelation from a regression). Verify how much correlation there is between `autocor_close` and `mr_st`. If they're >0.9 correlated, one is redundant.

---

## 23. T.addSplineDivergence (line 1204) -- Spline Cross-Speed Features

Derived features comparing the three spline speeds against each other and against price.

| Column | Description |
|--------|-------------|
| `spline_skew` | `slope_fast - slope_slow` -- fast vs slow momentum divergence |
| `spline_range` | `(slope_slow - slope_medium)^2 + (slope_fast - slope_medium)^2` -- slope dispersion |
| `spline_divergence` | `0.5*(spline_slow + spline_fast) - spline_medium` -- convexity |
| `close_spline` | `log(close / spline_medium) / vol_st` -- price vs medium spline (uses vol_st) |
| `close_spline_slow` | `log(close / spline_slow) / vol_lt` -- price vs slow spline |

> **Observation**: `close_spline` normalises by `vol_st` while `close_spline_slow` normalises by `vol_lt`. The inconsistency may be intentional (ST deviations measured in ST vol units, LT in LT vol units) but should be documented. `spline_divergence` is analogous to the curvature of the moving average ribbon. These 5 columns extract a lot of signal from the existing 3 splines without much computational cost -- efficient features.

---

## 24. T.addMaxMoveInNextMonths (line 538) -- **FORWARD-LOOKING TARGET**

Maximum future high and minimum future low at multiple horizons. Used for setting take-profit targets, not for prediction.

| Column pattern | Description | Count |
|----------------|-------------|-------|
| `nxt_max_{n}` | Max high in next n days, vol-normalised | 4 |
| `nxt_min_{n}` | Min low in next n days, vol-normalised | 4 |
| `nxt_max_ampl_{n}` | `max(nxt_max, abs(nxt_min))` -- max amplitude either direction | 4 |

**Subtotal: 12 columns** (for n in {21, 65, 130, 260})

> **Important**: These are **target variables** for take-profit calibration. Must not be used as features. The `nxt_max_ampl` is the maximum adverse/favourable excursion -- useful for setting stop-loss and take-profit levels.

---

## 25. T.addPareto (line 797) -- Power Law Tail Parameters

Joins pre-computed Pareto (power-law) tail distribution parameters from the `histo_pareto` database table. These characterise how fat the tails are.

| Column | Description |
|--------|-------------|
| `pl_alpha_left` | Pareto alpha for left tail (lower = fatter tail) |
| `pl_l_left` | Threshold where power-law behaviour begins, left tail |
| `pl_alpha_right` | Pareto alpha for right tail |
| `pl_l_right` | Threshold where power-law behaviour begins, right tail |
| `pl_l_width` | `(pl_l_right - pl_l_left) / vol_lt` -- width between tail thresholds |
| `pl_alpha_ratio` | `pl_alpha_right / pl_alpha_left` -- tail asymmetry |

**Subtotal: 6 columns**

> **Observation**: Theoretically interesting features. The Pareto alpha determines the probability of extreme moves. `pl_alpha_ratio > 1` means the right tail is thinner (large up-moves are rarer than large down-moves). These are read from a pre-computed table -- the computation happens offline via a separate script. Check how frequently the table is refreshed and whether stale values could be an issue. Also, forward-filling NAs means new instruments get no Pareto data until the offline job runs.

---

## 26. T.addSuperExponentiality (line 1238) -- Bubble Detection

Tests whether recent price action follows a super-exponential trajectory (convex on a log scale), which is a hallmark of speculative bubbles (per Didier Sornette's work).

| Column | Description |
|--------|-------------|
| `sup_exp_close` | Super-exponentiality measure on close prices (atanh-transformed, -1 to +1 mapped to reals) |
| `sup_exp_high` | Additional high-price super-exponentiality, adjusted relative to close |
| `sub_exp_low` | Sub-exponentiality measure on low prices, adjusted relative to close |
| `sup_exp_close_lagged` | `lag(sup_exp_close, 5)` |
| `sup_exp_close_chg` | `sup_exp_close - sup_exp_close_lagged` -- 5-day change |

**Subtotal: 5 columns**

> **Observation**: The algorithm generates all pairs of dates in the last 20 days, drawing lines between each pair and checking whether actual prices are above or below. This is O(n^2) in the window size but n=20 is small. The atanh transform is appropriate for mapping a proportion to an unbounded scale. Conceptually related to the Hurst exponent (both detect trends vs mean-reversion) but measures something different: Hurst detects persistence, super-exponentiality detects acceleration. Check correlation between `sup_exp_close` and `hurst_st` to confirm they're not redundant.

---

## Summary Table

| # | Function | Columns | Category |
|---|----------|---------|----------|
| 1 | T.addSTVol | 1 | Volatility |
| 2 | T.addLTVol | 1 | Volatility |
| 3 | T.addAllSplines | 47 | Trend / Levels |
| 4 | T.addBollingerBandsFLS | 2 | Levels |
| 5 | T.addSimpleStuff | 15 | Returns / Ratios |
| 6 | T.addDateRelated | 2 | Calendar |
| 7 | T.addAssetClass | 1 | Metadata |
| 8 | T.addFXPriority | 1 | Metadata |
| 9 | T.addTechnicalZones | 10 | Levels |
| 10 | T.addVolDifferences | 11 | Volatility |
| 11 | T.addLTVolAverage | 3 | Volatility |
| 12 | T.addSkewKurtosis | 6 | Distribution |
| 13 | T.addMeanReversionMomentum | 6 | Regime |
| 14 | T.addRecentHiLo | 27 | Levels / Momentum |
| 15 | T.addBiggestRecentCandle | 26 | Candles |
| 16 | T.addEngulfing | 2 | Candles |
| 17 | T.addOvernightStats | 5 | Returns |
| 18 | T.addCandleShadowsMeasure | 25 | Candles |
| 19 | T.addNewHighsOrLows | 4 | Breakout |
| 20 | T.addRecentTrend | 9 | Trend |
| 21 | T.addNextMonthTrend | 2 | **Target** |
| 22 | T.addAutoCorrelation | 1 | Regime |
| 23 | T.addSplineDivergence | 5 | Trend |
| 24 | T.addMaxMoveInNextMonths | 12 | **Target** |
| 25 | T.addPareto | 6 | Tail Risk |
| 26 | T.addSuperExponentiality | 5 | Bubble |
| | **Total** | **~237** | |

Of these, **14 columns** are forward-looking targets (NextMonthTrend: 2, MaxMoveInNextMonths: 12), leaving **~223 candidate predictor features**.

---

## Review Priorities

### Bugs to Fix
1. ~~**`psi_60` uses lag 20 instead of lag 60**~~ -- **FIXED (2026-03-22)**. R code corrected in `T.addSimpleStuff` line 1021 (`lag(close, 20)` -> `lag(close, 60)`). All 974K historical rows in `histo_technicals_dbl` corrected via direct SQL UPDATE.
2. **Skewness/kurtosis computed on prices, not returns** (T.addSkewKurtosis, line 1035). Measures trend shape, not return distribution shape.

### Redundancy Candidates
3. **`autocor_close` vs `mr_st`/`mr_lt`** -- both measure lag-1 return autocorrelation.
4. **`volstdiff`/`volltdiff`** (raw vol changes) vs **`vol_st_20`** (log vol ratio) -- overlapping information, different scales.
5. **`lvl_spline_*`** (30 cols) vs **`lvl_l/r_*`** (10 cols) -- both encode nearest support/resistance levels.
6. **`sup_exp_close`** vs **`hurst_st`** -- both detect trend persistence; verify they're not redundant.

### Design Questions
7. **`bb_up`/`bb_dn`** -- are these absolute price levels consumed by the model, or only by the dashboard?
8. **`engulfing`** -- should encode direction (+1 bullish, -1 bearish) not just occurrence (0/1).
9. **`weekday`** -- integer encoding implies ordering; consider alternatives for tree model.
10. **`perf_ovn_*`** -- likely near-zero for FX (24h market); check variance by asset class.
11. **Filled high/low** -- filling with close creates zero-range days that distort range-based features.
12. **Pareto staleness** -- how frequently is `histo_pareto` refreshed? Stale values may mislead.
13. **Spline inflection levels l_4/l_5, r_4/r_5** -- are the further-away levels used by the model, or just noise?
14. **`close_spline` vs `close_spline_slow`** -- one normalises by vol_st, the other by vol_lt. Intentional?
