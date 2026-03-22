# New Features - Implementation Preparation

## Current Pipeline Summary

Understanding the existing pipeline is essential before adding new features. There are three distinct paths:

### Path 1: Historical (Full Recompute)

```
Scripts/Technicals/Full.R
  → T.calcTechnicalsFull(pair)
    → T.calcTechnicals(pair)          # computes ~237 features for one instrument
      → T.getHistoPx(pair)            # loads OHLC from histo_px_daily
      → chain of T.addXxx() functions # per-instrument, sequential
    → gather() to long format          # wide → EAV (instrument_id, date, feature_id, value)
    → DELETE all rows for this instrument from histo_technicals_dbl / _int
    → D.insertDataIntoTable()          # bulk insert
    → T.addTradingDataAllPossibilities() # compute trade outcomes for all bb_width x duration combos
```

Full.R picks one instrument at a time (randomised, prioritising stale data), recomputes everything, and writes to DB. It runs repeatedly via crontab.

### Path 2: Historical (Recent Update)

```
Scripts/Technicals/Recent.R
  → T.calcTechnicalsRecentOnly(pair_list)
    → T.getTechnicals(pair)           # loads existing technicals from DB (EAV → wide via spread())
    → T.calcTechnicals(pair, TRUE)    # recomputes recent ~130 days only
    → DELETE recent rows, INSERT new ones
```

### Path 3: Live (Daily Prediction)

```
Scripts/Technicals/Live.R
  → E.prepareTechnicalsLive(pair_list)
    → loads live_px (today's prices)
    → loads histo_px_daily (history)
    → for each instrument:
      → T.calcTechnicalsLive(dat_ohlc)  # same T.addXxx chain, but on combined histo + live data
      → keeps only the last row (today)
    → gather() to long format
    → writes to live_technicals_dbl / _int (DELETE then INSERT per instrument)
```

### The T.addXxx Chain (order matters)

```r
T.calcTechnicals(pair):
  T.fillMissingOpenWithPreviousClose →
  T.fillMissingHighAndLowWithClose →
  T.addSTVol → T.addLTVol →
  T.addAllSplines →
  T.addBollingerBandsFLS →
  T.addSimpleStuff →
  T.addDateRelated →
  T.addAssetClass → T.addFXPriority →
  T.addTechnicalZones →
  T.addVolDifferences → T.addLTVolAverage →
  T.addSkewKurtosis →
  T.addMeanReversionMomentum →       # already has hurst_st/lt, momersion_st/lt, mr_st/lt
  T.addRecentHiLo →
  T.addBiggestRecentCandle →
  T.addEngulfing →
  T.addOvernightStats →
  T.addCandleShadowsMeasure →
  T.addNewHighsOrLows →
  T.addRecentTrend →
  T.addNextMonthTrend →
  T.addAutoCorrelation →              # already has autocor_close (lag-1, 65-day window)
  T.addSplineDivergence →
  T.addMaxMoveInNextMonths →
  T.addPareto →
  T.addSuperExponentiality →
  finalCleanData
```

`T.calcTechnicalsLive` uses the same chain minus `T.addNextMonthTrend` and `T.addMaxMoveInNextMonths` (forward-looking, not available live).

### Database Storage (EAV Format)

| Table | Key | Value | Rows |
|-------|-----|-------|------|
| `static_feature` | `feature_id` (max: 250) | `feature` name, `int_or_dbl`, `train` | 251 |
| `histo_technicals_dbl` | `(instrument_id, date, feature_id)` | `double value` | ~215M |
| `histo_technicals_int` | `(instrument_id, date, feature_id)` | `int value` | ~54M |
| `live_technicals_dbl` | `(instrument_id, feature_id)` | `double value` + timestamps | today only |
| `live_technicals_int` | same | `int value` | today only |

### How Features Reach the Model

```
T.getTechnicals(pair, strategy_id)
  → SELECT from histo_technicals_dbl/int WHERE feature_id IN (strategy_feature for this strategy)
  → spread() back to wide format
  → JOIN with histo_trade_outcome (tgt = up/flat/down)
  → returns wide tibble: one row per instrument×date, columns = features + tgt
```

The `strategy_feature` table controls which features each strategy's RF model sees.

### Key Design Constraint

**All current features are per-instrument.** `T.calcTechnicals(pair)` processes one instrument at a time. Cross-asset features (dispersion, VIX, yield curve) don't fit this pattern -- they are date-level, the same for all instruments on a given date.

---

## Features Already Implemented (No Duplication)

Before adding new features, note what already exists:

| Feature | Existing | Location |
|---------|----------|----------|
| Hurst exponent | `hurst_st`, `hurst_lt` | `T.addMeanReversionMomentum` |
| Momersion | `momersion_st`, `momersion_lt` | `T.addMeanReversionMomentum` |
| Mean reversion coefficient | `mr_st`, `mr_lt` | `T.addMeanReversionMomentum` |
| Lag-1 autocorrelation (65d) | `autocor_close` | `T.addAutoCorrelation` |

So from Research_Areas.md, Hurst and basic autocorrelation are already covered. Multi-lag autocorrelation (lags 2-5) and realised variance decomposition would be genuinely new.

---

## Data Availability

### Available in DB

| Data | Instruments | Date Range | Notes |
|------|-------------|------------|-------|
| FX (DM) | 39 pairs | ~2011-present | Daily, updated live |
| FX (EM) | 24 pairs | ~2011-present | Daily, updated live |
| Indices | 67 | ~2011-present | Daily, updated live |
| Metals | 4 | ~2011-present | Daily, updated live |
| Yields | 28 (2Y/5Y/10Y/30Y × 7 countries) | 2011-2021 Jan only | **Stale, stopped updating** |
| Bond ETFs | 17 | varies | Likely also stale |

### Not Available - Need External Source

| Data | Purpose | Possible Sources |
|------|---------|-----------------|
| VIX | Implied vol regime | CBOE (free CSV download), Yahoo Finance, FRED |
| VIX3M / VIX9D | VIX term structure | CBOE |
| MOVE index | Bond implied vol | Bloomberg terminal, ICE (paid) |
| FX implied vols | Currency vol surface | Bloomberg (paid), possibly IB |

### Yield Data Gap

US yields (instrument_ids 97-100) have data only up to 2021-01-27. For yield curve slope features to work historically, this data needs backfilling. Options:

1. **FRED** (Federal Reserve Economic Data) -- free API, has daily Treasury yields going back decades. Best option.
2. **investing.com** -- already have `T.importInvestingComHistoFile()` for manual CSV import.
3. **IB** -- could fetch via `Price_IB.py` if yields are configured.

The yield data would need to be repopulated before yield curve features can be added.

---

## Implementation Plan

### Phase 1: Per-Instrument Features (Simplest)

These fit directly into the existing `T.addXxx` pattern. No architectural changes needed.

#### 1a. Multi-Lag Autocorrelation

Add rolling autocorrelation at lags 2, 3, 5 to complement the existing lag-1 (`autocor_close`).

**Where**: extend `T.addAutoCorrelation` in Technicals.R (line 118).

**New features** (~3 dbl):
- `autocor_close_2` -- lag-2 autocorrelation (65-day window)
- `autocor_close_3` -- lag-3
- `autocor_close_5` -- lag-5

**Implementation**: replicate the existing `rollapply` pattern but with `lag(rtn_t, 2)`, `lag(rtn_t, 3)`, `lag(rtn_t, 5)` instead of `lag(rtn_t, 1)`.

#### 1b. Realised Variance Decomposition

Separate realised variance into continuous and jump components.

**Where**: new function `T.addRealisedVarianceDecomposition`, inserted after `T.addSkewKurtosis` in the chain.

**New features** (~3 dbl):
- `rv_bpv_ratio_st` -- bipower variation / realised variance ratio (short-term window). Values near 1 = continuous moves, values < 1 = jumps present
- `rv_bpv_ratio_lt` -- same, long-term window
- `rv_jump_pct_st` -- estimated jump contribution to total variance (short-term)

**Implementation**:
- Realised variance: `RV = sum(r_i^2)` over the window
- Bipower variation: `BPV = (pi/2) * sum(|r_i| * |r_{i-1}|)` over the window
- Jump ratio: `max(0, 1 - BPV/RV)`

This uses the Barndorff-Nielsen & Shephard (2004) estimator. Simple to compute from daily returns.

#### 1c. Cross-Sectional Rank Features

Rank this instrument's momentum, volatility, etc. vs all other instruments on the same date.

**Where**: this is trickier -- the current pipeline processes one instrument at a time. Two options:

**Option A (recommended)**: compute ranks as a post-processing step after all instruments have their technicals. New function `T.addCrossSectionalRanks` that reads from `histo_technicals_dbl` for all instruments on each date and computes percentile ranks. Runs once after `Full.R` or `Recent.R` completes all instruments. Writes additional rows to `histo_technicals_dbl`.

**Option B**: precompute a cross-sectional rank table for a few key features (psi_20, vol_st, slope_slow) and join it during `T.getTechnicals`. This avoids modifying the computation pipeline but requires a separate scheduled job.

**New features** (~6 dbl):
- `rank_psi_20` -- percentile rank of 20-day momentum vs all instruments
- `rank_psi_60` -- percentile rank of 60-day momentum
- `rank_vol_st` -- percentile rank of short-term volatility
- `rank_slope_slow` -- percentile rank of slow spline slope
- `rank_psi_20_ac` -- percentile rank within same asset class
- `rank_vol_st_ac` -- percentile rank within same asset class

**Implementation**:
```r
# Pseudocode for Option A
T.addCrossSectionalRanks <- function(date_range) {
    # For each date, load psi_20 for all instruments, compute percent_rank
    dat <- D.select("SELECT instrument_id, date, feature_id, value
                     FROM histo_technicals_dbl
                     WHERE feature_id IN (...) AND date >= ...")
    dat %>%
        group_by(date, feature_id) %>%
        mutate(rank_value = percent_rank(value)) %>%
        # write rank_value back with new feature_ids
}
```

### Phase 2: Date-Level Features (Cross-Asset)

These require architectural adaptation because they produce one value per date, not per instrument.

#### Architecture Decision

The EAV tables (`histo_technicals_dbl`) are keyed on `(instrument_id, date, feature_id)`. Date-level features need to exist in this schema. Two approaches:

**Option A: Phantom Instrument (recommended)**
- Create a pseudo-instrument (e.g., `instrument_id = 0` or a reserved ID like 999) that represents "market-wide" features.
- Store date-level features under this instrument_id.
- During `T.getTechnicals`, join these features by date only (ignoring instrument_id).
- Minimal schema change. Slightly hacky but pragmatic.

**Option B: Separate Table**
- Create `histo_technicals_date_dbl` with key `(date, feature_id)`.
- Modify `T.getTechnicals` to also read from this table and cross-join with instruments.
- Cleaner design but requires modifying the data loading path.

**Option C: Replicate Across Instruments**
- Store the same date-level value for every instrument.
- No code changes to reading logic.
- Massively inflates storage (~180 instruments × N features × N dates extra rows).
- Not recommended given the DB is already 22.5GB.

**Recommendation**: Option A (phantom instrument) for speed of implementation, with a comment noting the convention. Can migrate to Option B later if the number of date-level features grows large.

#### 2a. VIX Features

**Prerequisite**: source VIX daily close data. Best approach: download from CBOE or FRED, import via `T.importInvestingComHistoFile()` or a new dedicated import function.

Store VIX as a regular instrument in `histo_px_daily` (it has OHLC data). Then compute features from its price series.

**New features** (~6 dbl):
- `vix_level` -- VIX close
- `vix_pos_in_range_65` -- VIX position in its 65-day range (reuse `pos_in_range` logic)
- `vix_pos_in_range_260` -- same, 260-day
- `vix_psi_5` -- VIX 5-day change
- `vix_psi_20` -- VIX 20-day change
- `vix_term_structure` -- VIX3M / VIX ratio minus 1 (requires VIX3M data too)

**Where**: new function `T.calcVIXFeatures()` that runs independently (reads VIX price series, computes features, writes to `histo_technicals_dbl` under the phantom instrument).

**Live path**: a companion function reads today's VIX from live source and writes to `live_technicals_dbl`.

#### 2b. Dispersion / N_eff as Feature

**Prerequisite**: none -- all data already exists. Reuses `T.calcHistoricalCorrelationsMatrix` logic.

**New features** (~4 dbl):
- `market_n_eff_4w` -- N_effective from 4-week rolling correlation (short-term regime)
- `market_n_eff_13w` -- N_effective from 13-week rolling (~quarter)
- `market_n_eff_52w` -- N_effective from 52-week rolling
- `market_n_eff_chg` -- change in N_eff (13w vs 52w) -- is the market moving toward or away from one-factor mode

**Where**: new function `T.calcDispersionFeatures(as_of_date)` that:
1. Calls `T.calcHistoricalCorrelationsMatrix` at different lookback windows
2. Computes N_eff from eigenvalues for each
3. Writes to `histo_technicals_dbl` under phantom instrument

**Performance note**: computing correlation matrices for every historical date would be slow. Recommend computing weekly (e.g., every Tuesday, matching the weekly return anchor) and forward-filling. This is the same approach used in `V.readBacktest`.

#### 2c. Yield Curve Slope

**Prerequisite**: repopulate yield data (at least US 2Y and 10Y) from FRED or similar.

**New features** (~3 dbl):
- `yield_curve_2s10s` -- 10Y minus 2Y yield
- `yield_curve_2s10s_chg_20` -- 20-day change in curve slope
- `yield_curve_2s10s_pos_in_range` -- slope position in its 260-day range

**Where**: new function `T.calcYieldCurveFeatures()`, same pattern as VIX features.

#### 2d. Cross-Asset Correlations

**New features** (~3 dbl):
- `cor_equity_bond_13w` -- rolling 13-week correlation between SPX returns and US10Y yield changes
- `cor_equity_bond_52w` -- same, 52-week
- `cor_usd_equity_13w` -- rolling correlation between DXY (or a USD index) and SPX

**Prerequisite**: need a USD index. Could approximate by averaging EURUSD, GBPUSD, USDJPY returns (inverted for first two). Or use DXY if available.

### Phase 3: Database Registration

For each new feature, regardless of phase:

1. **Insert into `static_feature`**:
```sql
INSERT INTO static_feature (feature_id, feature, int_or_dbl, train)
VALUES (251, 'autocor_close_2', 1, 1);  -- next available ID after 250
```

2. **Run `V.removeUselessFeatures`** after historical data is populated to determine if the new features should be included in each strategy's `strategy_feature` set. Don't manually add to `strategy_feature` -- let the feature selection process decide.

3. **Update `T.calcTechnicals` chain** (for Phase 1 features) and the corresponding `T.calcTechnicalsLive` chain to include new functions in both paths.

4. **Run `Full.R`** to recompute historical technicals with new features (will take time to cycle through all instruments).

5. **Retrain models** once historical data is populated.

---

## Execution Order

```
1. Add Phase 1 features to code (T.addXxx functions)
2. Register new features in static_feature
3. Add to T.calcTechnicals and T.calcTechnicalsLive chains
4. Run Full.R to populate historical data (let it cycle through all instruments)
5. Run V.removeUselessFeatures for each strategy to update strategy_feature
6. Retrain models, run backtests to validate

7. Source VIX data (CBOE/FRED download + import)
8. Decide on phantom instrument approach for date-level features
9. Add Phase 2 computation functions
10. Register Phase 2 features in static_feature
11. Build historical date-level features
12. Integrate into T.getTechnicals loading path
13. Run V.removeUselessFeatures again, retrain, backtest
```

---

## Storage Impact Estimate

Current: ~215M rows in `histo_technicals_dbl`, ~54M in `_int`.

| Feature Group | New Features (dbl) | Rows Added (est.) | Notes |
|---------------|-------------------|-------------------|-------|
| Multi-lag autocorrelation | 3 | ~3M | ~1M dates × 3 features per instrument, but only ~180 instruments × ~5000 dates |
| Realised variance decomp | 3 | ~3M | Same |
| Cross-sectional ranks | 6 | ~6M | Same |
| VIX features | 6 | ~30K | Date-level only (~5000 dates × 6, under phantom instrument) |
| Dispersion/N_eff | 4 | ~1K | Weekly, date-level (~260 weeks × 4) |
| Yield curve | 3 | ~15K | Date-level |
| Cross-asset correlations | 3 | ~1K | Weekly, date-level |

Total estimated addition: ~12M rows in `histo_technicals_dbl` (~5.5% increase). Negligible impact.
