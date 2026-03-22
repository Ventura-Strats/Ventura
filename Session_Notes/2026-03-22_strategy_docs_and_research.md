# Session Notes: 2026-03-22 - Strategy Documentation & Research Areas

## Summary

Documentation-focused session. Created three new files in `Documentation/`:

### 1. Strategies.md
Full documentation of all 14 trading strategies from `static_strategy` and `strategy_criteria` DB tables, cross-referenced with `E.trainModel` and `E.genericScoreFunction` in Engine.R.

Contents:
- Pipeline overview (technicals → trade setup → criteria scoring → feature selection → RF training → prediction)
- Scoring system: `T.testAbove`/`T.testBelow` cosine ramp functions explained as proximity indicators (smooth 0-1, but criterion met only at exactly 1)
- `atanh(R²)` transform note with Fisher's z-transformation context and honest assessment of tree-split impact (helps cforest variable selection, doesn't affect split points since they're rank-based)
- Trade parameters: BB width multiplier table and max duration table with per-strategy mapping
- Per-strategy reference (1-14) with criteria tables translated to plain English
- Summary comparison table

### 2. Research_Areas.md
Feature and strategy ideas for future investigation, prioritised by expected value-add.

Feature ideas:
- Cross-asset regime (VIX, dispersion/N_eff, yield curve, cross-asset correlations) -- highest priority, qualitatively different from existing per-instrument features
- Per-instrument additions (multi-lag autocorrelation, realised variance decomposition, cross-sectional ranks)
- Strategy historical performance as meta-feature -- high overfitting risk, regime features may be a safer proxy
- Identified what already exists and shouldn't be duplicated (Hurst, momersion, lag-1 autocorrelation)

Strategy ideas: volatility regime change, cross-asset divergence, post-gap continuation, carry+trend for FX, breadth divergence.

### 3. New_Features_Implementation.md
Implementation preparation -- traced the full pipeline without making code changes.

Key findings:
- Three computation paths (Full/Recent/Live) all feed into same T.addXxx chain
- EAV storage: `histo_technicals_dbl` (215M rows), `histo_technicals_int` (54M rows)
- All current features are per-instrument; cross-asset features need architectural adaptation
- Recommended phantom instrument approach for date-level features (store under reserved instrument_id, join by date)
- Yield data (US 2Y/5Y/10Y/30Y) stops at Jan 2021 -- needs FRED backfill before yield curve features work
- No VIX data in DB -- needs external sourcing (CBOE or FRED)
- Phased plan: Phase 1 (per-instrument, fits existing pattern), Phase 2 (date-level, needs phantom instrument), Phase 3 (DB registration, feature selection, retrain)
- Storage impact: ~12M new rows (~5.5% increase), negligible

## Files Changed
- `Documentation/Strategies.md` -- new
- `Documentation/Research_Areas.md` -- new
- `Documentation/New_Features_Implementation.md` -- new
- `CLAUDE.md` -- updated Completed section
- `Session_Notes/2026-03-22_strategy_docs_and_research.md` -- new (this file)

## Next Steps
- User to test trailing win rate as ad-hoc backtest feature
- Source VIX data from CBOE/FRED
- Backfill yield data from FRED
- Implement Phase 1 features (multi-lag autocorrelation, realised variance decomposition, cross-sectional ranks)
- Implement Phase 2 features (VIX, dispersion, yield curve, cross-asset correlations)
- Test new strategies after features are in place
