# Strategies

## Overview

Ventura uses 14 trading strategies (IDs 1-14), plus a non-trading strategy 0 for FX hedges. Each strategy defines a **market regime** through a set of scoring criteria. When an instrument's technicals match all criteria for a strategy, it becomes eligible for a prediction by that strategy's random forest model.

The strategies are **directionally agnostic** at the criteria stage. They identify *conditions* (e.g., "strong trend in a volatile market") rather than specifying buy/sell. The direction comes from the random forest model, which predicts whether the outcome will be `up`, `down`, or `flat`.

## Architecture

### Tables

| Table | Purpose |
|-------|---------|
| `static_strategy` | Strategy ID, description, trade parameters (`bb_width_id`, `max_duration_id`), RF tree count |
| `strategy_criteria` | Scoring criteria per strategy (composite key: `strategy_id` + `score`) |
| `strategy_feature` | Features selected for each strategy's random forest model (~120-130 per strategy) |
| `static_trade_bb_width` | Bollinger Band width multipliers for target/stop levels |
| `static_trade_max_duration` | Maximum trade duration in days |

### Pipeline

1. **Technicals computed** (`T.calcTechnicals`) -- ~237 features per instrument per date
2. **Trade setup computed** (`T.addTradingData`) -- target/stop levels and historical trade outcomes (`up`/`down`/`flat`) for each `bb_width` x `max_duration` combination
3. **Criteria scoring** (`E.genericScoreFunction`) -- evaluates each instrument/date against the strategy's criteria, produces a score out of 10
4. **Feature selection** (`strategy_feature` table) -- filters the ~237 features down to ~120-130 relevant ones per strategy
5. **Model training** (`E.trainModel`) -- conditional inference forest (`cforest`) trained on historical data where `score == 10`
6. **Prediction** (`E.modelPredict`) -- model predicts `up`/`down`/`flat` on today's data for instruments that score 10

## Scoring System

### How Criteria Work

Each strategy has N criteria stored in `strategy_criteria`. The scoring function `E.genericScoreFunction` evaluates all criteria for each instrument/date row and produces a composite score.

Each criterion returns a value between 0 and 1, using smooth cosine ramp functions:

- **`T.testAbove(x, threshold, width)`** -- returns 0 when `x <= threshold - width`, smoothly ramps to 1 when `x >= threshold`
- **`T.testBelow(x, threshold, width)`** -- the mirror: returns 1 when `x <= threshold`, ramps to 0 when `x >= threshold + width`

The ramp uses a cosine interpolation: `(1 - cos(pi * (x - x0) / (x1 - x0))) / 2`, providing a smooth S-curve between 0 and 1.

A criterion is **met** only when the function returns exactly 1 (i.e., the value is fully past the threshold). The smooth ramp region (between 0 and 1) does not qualify the instrument -- it serves as a **proximity indicator**, showing how close an instrument is to meeting the criterion. This is useful in the dashboard and diagnostics to see which instruments are nearly eligible, not just which ones are fully there.

The individual criterion scores (each 0-1) are summed, normalised to a 0-10 scale, and a row qualifies for training/prediction only when `score == 10` -- meaning **every criterion must return exactly 1**.

Some criteria use hard boolean expressions (e.g., `(asset_class == "index") + 0`) which evaluate to exactly 0 or 1 with no ramp.

### Note on R-Squared Features

Features named `*_r2` (e.g., `trend_10_r2`, `trend_21_r2`, `trend_130_r2`) are not raw R-squared values. They are **`atanh(R²)`**, which maps the 0-1 range to 0-infinity. This stretches the upper end of the scale where distinctions matter most:

| Raw R² | atanh(R²) |
|--------|-----------|
| 0.04   | 0.04      |
| 0.50   | 0.55      |
| 0.90   | 1.47      |
| 0.95   | 1.83      |
| 0.99   | 2.65      |

This is analogous to Fisher's z-transformation (1921), which applies `atanh` to correlation coefficients. The original motivation is variance stabilisation: the sampling variance of raw `r` shrinks as `|r|` approaches 1, so `atanh` produces approximately constant variance across the range.

The transform has two concrete benefits in Ventura:

1. **Criteria scoring**: `T.testAbove(trend_10_r2, 1.2, 0.4)` defines a ramp of width 0.4 on the `atanh` scale. In raw R² space, a fixed width would mean something very different at 0.5 vs 0.95. The transform makes the width parameter behave consistently across the range.

2. **`cforest` variable selection**: Standard decision trees select splits based on rank ordering, so monotonic transforms don't change split quality. However, `cforest` (conditional inference forests) uses a permutation-based linear test statistic for variable selection at each node, and this statistic is scale-sensitive. If R² values are bunched near 1, the linear statistic could underweight the association. The `atanh` spread may help `cforest` recognise the feature as worth splitting on, even though the actual split point is rank-based and unaffected.

All criteria thresholds on `*_r2` features (e.g., `trend_10_r2 > 1.2` in Strategy 1) are on the `atanh` scale, not the 0-1 scale.

### Worked Example: Strategy 1 (Strong Trend in Volatile Market)

| # | Criterion | Meaning |
|---|-----------|---------|
| 1 | `T.testAbove(pmax(trend_10_r2, trend_21_r2), 1.2, 0.4)` | At least one of the 10-day or 21-day trend R-squared must be above 1.2 |
| 2 | `T.testAbove(vol_st_decile, 0.85, 0.2)` | Short-term volatility must be in the top ~15% of its historical distribution |
| 3 | `T.testAbove(abs(psi_20), 0.65, 0.3)` | 20-day price momentum (absolute) must be strong (above 0.65) |

All three must score 1.0 for the row to qualify. This selects instruments experiencing a strong directional trend with elevated volatility and significant recent price movement.

## Trade Parameters

### Bollinger Band Width (`bb_width_id`)

The `bb_width_id` controls how far target and stop levels are placed from the current price. The target/stop calculation is:

```
t_up = close + bb_width * (bb_up - spline_medium)
t_dn = close + bb_width * (bb_dn - spline_medium)
```

Where `bb_up` and `bb_dn` are Bollinger Band levels and `spline_medium` is a medium-term spline fit. The `bb_width` multiplier scales the distance.

| `bb_width_id` | Multiplier | Used by |
|---------------|-----------|---------|
| 1 | 1.00x | Strategy 0 |
| 2 | 1.50x | -- |
| 3 | 1.75x | -- |
| 4 | 2.00x | Strategies 1, 3, 4, 5, 6, 7, 9, 12, 13, 14 |
| 5 | 2.25x | Strategies 2, 8, 10, 11 |
| 6 | 2.50x | -- |
| 7 | 3.00x | -- |
| 8 | 4.00x | -- |
| 9 | 5.00x | -- |

Most strategies use 2.00x or 2.25x. Wider multipliers place targets further away, producing higher reward-per-trade but lower hit rates.

### Maximum Duration (`max_duration_id`)

Controls how many trading days a trade is allowed to run before being classified as `flat` (neither target nor stop hit).

| `max_duration_id` | Days | Used by |
|-------------------|------|---------|
| 1 | 1 | Strategy 0 |
| 2 | 2 | -- |
| 3 | 5 | All active strategies (1-14) |
| 4 | 10 | -- |

All active strategies use a 5-day maximum duration.

### Trade Outcome Classification

For each historical date, `T.addTradingData` determines the trade outcome:

- **`up`** -- price hit the upper target (`t_up`) first within the duration window
- **`down`** -- price hit the lower target (`t_dn`) first
- **`flat`** -- neither target was hit within the duration; trade exits at closing price on the last day
- **`unknown`** -- both targets hit on the same day (rare, excluded from training)

### Random Forest Configuration

| Parameter | Value | Notes |
|-----------|-------|-------|
| Algorithm | `cforest` (conditional inference forest) | From `party` package |
| Trees (`rf_n_trees`) | 2000-3000 | Strategy-specific, see table below |
| Target variable | `tgt` (factor: up/flat/down) | 3-class classification |
| Features | ~120-130 per strategy | Selected by `V.removeUselessFeatures()` |
| Time weighting | Exponential decay, 10-year half-life | More recent data weighted higher |
| Training instruments | Limited set (~90 pairs) | Curated list in `E.trainModel` |

## Strategy Reference

### Strategy 0: FX Hedges

**Not a trading strategy.** Used for FX hedge calculations. `bb_width_id=1` (1.00x), `max_duration_id=1` (1 day), 1 tree.

---

### Strategy 1: Strong Trend in Volatile Market

**Description:** Identifies instruments with a strong established trend and elevated volatility.

**Trees:** 3000 | **BB Width:** 2.00x | **Duration:** 5 days | **Features:** 120

| # | Criterion | Plain English |
|---|-----------|---------------|
| 1 | `T.testAbove(pmax(trend_10_r2, trend_21_r2), 1.2, 0.4)` | Strong trend fit (10d or 21d R-squared > 1.2) |
| 2 | `T.testAbove(vol_st_decile, 0.85, 0.2)` | High short-term volatility (top 15% decile) |
| 3 | `T.testAbove(abs(psi_20), 0.65, 0.3)` | Large 20-day price change (absolute) |

---

### Strategy 2: Month High

**Description:** Instruments making new 20-day highs with positive skew and an upward spline.

**Trees:** 3000 | **BB Width:** 2.25x | **Duration:** 5 days | **Features:** 120

| # | Criterion | Plain English |
|---|-----------|---------------|
| 1 | `T.testAbove(hi_vs_hi_20, 0, 0.2)` | Current high above previous 20-day high |
| 2 | `T.testAbove(skew_st, 1.1, 0.4)` | Short-term return distribution positively skewed |
| 3 | `T.testAbove(close_spline, 0.075, 0.075)` | Close above medium-term spline |

---

### Strategy 3: Month Low

**Description:** Mirror of Strategy 2 -- instruments making new 20-day lows with negative skew and a downward spline.

**Trees:** 3000 | **BB Width:** 2.00x | **Duration:** 5 days | **Features:** 120

| # | Criterion | Plain English |
|---|-----------|---------------|
| 1 | `T.testBelow(lo_vs_lo_20, 0, 0.2)` | Current low below previous 20-day low |
| 2 | `T.testBelow(skew_st, -1.3, 0.4)` | Short-term returns negatively skewed |
| 3 | `T.testBelow(close_spline, -0.1, 0.075)` | Close below medium-term spline |

---

### Strategy 4: Long-Term Trendy Market, Quiet

**Description:** Strong long-term trend with low recent volatility. Index-only.

**Trees:** 2000 | **BB Width:** 2.00x | **Duration:** 5 days | **Features:** 128

| # | Criterion | Plain English |
|---|-----------|---------------|
| 1 | `T.testAbove(trend_130_r2, 1.15, 0.4)` | Strong 130-day trend fit |
| 2 | `T.testBelow(abs(psi_5), 0.05, 0.15)` | Minimal 5-day price movement |
| 3 | `T.testBelow(vol_ratio, -0.1, 0.3)` | Short-term vol below long-term vol |
| 4 | `(asset_class == "index") + 0` | **Indices only** |

---

### Strategy 5: Quiet Markets

**Description:** Multi-criteria filter for low-volatility, trending markets. Index-only with the most criteria (10).

**Trees:** 3000 | **BB Width:** 2.00x | **Duration:** 5 days | **Features:** 130

| # | Criterion | Plain English |
|---|-----------|---------------|
| 1 | `T.testAbove(trend_130_r2, 0.5, 0.4)` | Moderate+ long-term trend |
| 2 | `T.testBelow(pmin(vol_decile_diff, vol_st_decile - 0.1), 0, 0.25)` | Volatility declining and/or below average |
| 3 | `T.testBelow(vol_ratio, 0, 0.25)` | Short-term vol below long-term vol |
| 4 | `T.testBelow(vol_ratio_diff_vs_yesterday, 0, 0.10)` | Volatility ratio declining day-over-day |
| 5 | `T.testBelow(normality_lt, 2, 0.4)` | Long-term returns reasonably normal |
| 6 | `T.testAbove(trend_130_slope, 0, 0.25)` | Positive trend slope |
| 7 | `(asset_class == 'index') + 0` | **Indices only** |
| 8 | `T.testAbove(psi_20, 0, 0.4)` | Positive 20-day momentum |
| 9 | `T.testBelow(abs(vol_ratio_up_dn_st), 0.5, 0.4)` | Balanced up/down volatility |
| 10 | `T.testBelow(abs(up_vs_dn_shdw_130), 0.4, 0.4)` | Balanced upper/lower shadows (no bias) |

---

### Strategy 6: Middle of a Wide Recent Range

**Description:** Instruments sitting mid-range within a wide 20-day band, with negative short-term skew.

**Trees:** 2000 | **BB Width:** 2.00x | **Duration:** 5 days | **Features:** 119

| # | Criterion | Plain English |
|---|-----------|---------------|
| 1 | `T.testAbove(hi_lo_20, 0.4, 0.3)` | Wide 20-day high-low range |
| 2 | `T.testBelow(lo_20, -0.15, 0.2)` | 20-day low is meaningfully below current level |
| 3 | `T.testAbove(hi_20, 0.15, 0.2)` | 20-day high is meaningfully above current level |
| 4 | `T.testBelow(skew_st, 0, 0.4)` | Short-term skew is negative |

---

### Strategy 7: Excess Buildup Then Drop Last Week

**Description:** Instruments that built up speculative excess (measured by `sup_exp`) followed by a recent decline.

**Trees:** 3000 | **BB Width:** 2.00x | **Duration:** 5 days | **Features:** 120

| # | Criterion | Plain English |
|---|-----------|---------------|
| 1 | `T.testAbove(sup_exp_close_lagged, 0.6, 0.4)` | Lagged supply/excess indicator was elevated |
| 2 | `T.testBelow(sup_exp_close_chg, -0.3, 0.4)` | Supply/excess indicator has dropped recently |

---

### Strategy 8: Aggressive Medium-Term Top Followed by Close Down

**Description:** Instruments that made intraday highs above recent peaks but closed lower on the day, within an overall uptrend.

**Trees:** 3000 | **BB Width:** 2.25x | **Duration:** 5 days | **Features:** 121

| # | Criterion | Plain English |
|---|-----------|---------------|
| 1 | `T.testAbove(high_close - hi_65, 0, 0.25)` | Intraday high exceeded the 65-day high |
| 2 | `T.testAbove(high_close, 0.05, 0.05)` | Intraday high well above the close (long upper wick) |
| 3 | `T.testBelow(psi_1, -0.0000001, 0.1)` | Negative 1-day return (closed down) |
| 4 | `T.testAbove(slope_slow, 0.02, 0.1)` | Slow spline slope positive (uptrend backdrop) |
| 5 | `T.testAbove(psi_20, 0.1, 0.25)` | Positive 20-day momentum |
| 6 | `T.testAbove(close_spline_slow, 0, 0.05)` | Close above slow spline |

---

### Strategy 9: Optimum

**Description:** All spline trends aligned in the same direction, with the medium-term spline at or near an extremum (turning point), and today's move is counter-trend.

**Trees:** 3000 | **BB Width:** 2.00x | **Duration:** 5 days | **Features:** 122

| # | Criterion | Plain English |
|---|-----------|---------------|
| 1 | `T.testAbove(abs(sign(slope_fast) + sign(slope_medium) + sign(slope_slow)), 3, 1)` | All three spline slopes have the same sign |
| 2 | `T.testBelow(pmin(lvl_spline_r_1_slow, -lvl_spline_l_1_slow), 0.005, 0.05)` | Slow spline near a local extremum |
| 3 | `T.testAbove(abs(slope_medium), 0.15, 0.1)` | Medium spline slope is meaningfully non-zero |
| 4 | `(sign(psi_1) + sign(slope_medium) == 0) + 0` | Today's move is opposite to the medium trend (counter-trend day) |
| 5 | `T.testAbove(hi_lo, 0.05, 0.05)` | Some intraday range (not a dead market) |

---

### Strategy 10: Trend, Then Flat, Then Impulsion

**Description:** Instrument has a strong slow trend, went flat recently (fast spline slope near zero), and today's move is in the direction of the slow trend.

**Trees:** 3000 | **BB Width:** 2.25x | **Duration:** 5 days | **Features:** 128

| # | Criterion | Plain English |
|---|-----------|---------------|
| 1 | `T.testBelow(abs(slope_fast), 0.1, 0.2)` | Fast spline slope near zero (recent consolidation) |
| 2 | `T.testAbove(abs(slope_slow), 0.25, 0.25)` | Strong slow trend |
| 3 | `T.testAbove(sign(psi_1) * sign(slope_slow), 1, 1)` | Today's move aligns with slow trend direction |
| 4 | `T.testAbove(hi_lo, 0.05, 0.05)` | Some intraday range |
| 5 | `T.testBelow(volstdiff, 0, 0.1)` | Short-term vol not increasing |

---

### Strategy 11: Sharp Accelerating Trend

**Description:** Medium-term trend is large and accelerating (slope and acceleration have the same sign).

**Trees:** 2000 | **BB Width:** 2.25x | **Duration:** 5 days | **Features:** 127

| # | Criterion | Plain English |
|---|-----------|---------------|
| 1 | `T.testBelow(nb_x_slope_chg_20_slow, 0, 1)` | Slow spline hasn't changed direction recently |
| 2 | `T.testAbove(abs(slope_medium), 1.25, 0.25)` | Very large medium-term spline slope |
| 3 | `T.testAbove(slope_medium * accel_medium, 0, 0.5)` | Medium slope and acceleration same sign (accelerating) |

---

### Strategy 12: Edge of Range, Flat, Then Acceleration

**Description:** Instrument is near the edge of its long-term range, has been moving slowly, and is now accelerating.

**Trees:** 3000 | **BB Width:** 2.00x | **Duration:** 5 days | **Features:** 127

| # | Criterion | Plain English |
|---|-----------|---------------|
| 1 | `T.testAbove(pmax(abs(pos_in_range_130 - 0.5), abs(pos_in_range_260 - 0.5)), 0.45, 0.15)` | Near edge of 130-day or 260-day range (top or bottom 5%) |
| 2 | `T.testBelow(abs(slope_slow), 0.15, 0.25)` | Slow spline slope was flat |
| 3 | `T.testAbove(abs(accel_fast), 1.5, 0.5)` | Fast spline is accelerating sharply |

---

### Strategy 13: Neverending Short Squeeze After a Big Drop

**Description:** Instrument that had a major drawdown (130-day low well below) but is now in the upper part of its 260-day range, trending up with positive momentum.

**Trees:** 3000 | **BB Width:** 2.00x | **Duration:** 5 days | **Features:** 130

| # | Criterion | Plain English |
|---|-----------|---------------|
| 1 | `T.testAbove(pos_in_range_260, 0.75, 0.25)` | In the top 25% of 260-day range |
| 2 | `T.testBelow(lo_130, -1.5, 0.5)` | 130-day low was very deep |
| 3 | `T.testAbove(slope_slow, 0.35, 0.25)` | Strong positive slow trend |
| 4 | `T.testAbove(psi_60, 0.45, 0.25)` | Strong 60-day momentum |
| 5 | `T.testBelow(abs(spline_range), 1, 0.5)` | Spline range not too extreme |
| 6 | `T.testBelow(abs(spline_skew), 1, 0.5)` | Spline skew not too extreme |
| 7 | `T.testAbove(momersion_st, 0.55, 0.15)` | Short-term momersion above neutral (trending > mean-reverting) |

---

### Strategy 14: Negative Days Much More Volatile Than Positive

**Description:** Instruments where down-day volatility significantly exceeds up-day volatility, with a positive slow trend and contained spline shape.

**Trees:** 2000 | **BB Width:** 2.00x | **Duration:** 5 days | **Features:** 130

| # | Criterion | Plain English |
|---|-----------|---------------|
| 1 | `T.testBelow(vol_ratio_up_dn_lt, -0.25, 0.2)` | Long-term down-vol >> up-vol |
| 2 | `T.testBelow(abs(spline_range), 1, 0.25)` | Spline range contained |
| 3 | `T.testBelow(abs(spline_skew), 1, 0.25)` | Spline skew contained |
| 4 | `T.testAbove(slope_slow, 0, 0.25)` | Positive slow trend |
| 5 | `T.testBelow(volstdiff, 0, 0.1)` | Short-term vol not increasing |

## Summary Table

| ID | Description | Criteria | Features | Trees | BB Width | Asset Filter |
|----|-------------|----------|----------|-------|----------|--------------|
| 1 | Strong trend in volatile market | 3 | 120 | 3000 | 2.00x | All |
| 2 | Month high | 3 | 120 | 3000 | 2.25x | All |
| 3 | Month low | 3 | 120 | 3000 | 2.00x | All |
| 4 | Long-term trendy market, quiet | 4 | 128 | 2000 | 2.00x | Index |
| 5 | Quiet markets | 10 | 130 | 3000 | 2.00x | Index |
| 6 | Middle of a wide recent range | 4 | 119 | 2000 | 2.00x | All |
| 7 | Excess buildup then drop | 2 | 120 | 3000 | 2.00x | All |
| 8 | Aggressive top, close down | 6 | 121 | 3000 | 2.25x | All |
| 9 | Optimum (aligned splines, counter-trend day) | 5 | 122 | 3000 | 2.00x | All |
| 10 | Trend, flat, impulsion | 5 | 128 | 3000 | 2.25x | All |
| 11 | Sharp accelerating trend | 3 | 127 | 2000 | 2.25x | All |
| 12 | Edge of range, flat, acceleration | 3 | 127 | 3000 | 2.00x | All |
| 13 | Short squeeze after big drop | 7 | 130 | 3000 | 2.00x | All |
| 14 | Negative days more volatile | 5 | 130 | 2000 | 2.00x | All |

## Related Documentation

- `Documentation/Feature_Selection.md` -- how `V.removeUselessFeatures()` reduces ~2000 features to ~120-130 per strategy
- `Documentation/Technical_Features.md` -- catalogue of all ~237 features produced by `T.calcTechnicals()`
