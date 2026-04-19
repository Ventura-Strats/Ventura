---
name: backtest
description: Running and interpreting Ventura backtests. Use when the user asks about backtesting, V.readBacktest, V.backtestModel, backtest results, or strategy performance analysis.
---

# Backtesting Guide

## Two-Step Process

### Step 1: Generate Signals (V.backtestModel)

Produces backtest signal CSV files. Run via Backtesting.R script.

**Script**: `Scripts/Model/Backtesting.R`

```bash
Rscript ../Scripts/Model/Backtesting.R 0 1 FALSE 1995-01-01 2026-04-01
# Arguments: pid, strat_id, use_weights, start_date, end_date
```

- Uses 120 features, 1000 trees (random forest)
- Output: `/home/fls/Data/Ventura/HD/Backtestings/backtest_strat_X_stitched.csv`

### Step 2: Analyze Results (V.readBacktest)

Reads signal CSVs and simulates portfolio with eigenvalue-based sizing.

```r
source("Init.R"); I.loadModules()
result <- V.readBacktest(c(1, 2, 3))  # strategy IDs
result$summary  # yearly + total stats
```

**Parameters** (hardcoded inside function, ~line 2892 of Ventura.R):
- `risk_per_bet_pct = 0.5`, `max_daily_risk_pct = 5`
- `correlation_adjustment = 0`, `lookback_weeks = 104`, `shrinkage = 0.1`
- `pnl_reference_usd = 1000`
- `flat_fee_per_trade = 2.5 / 100000`, `cost_of_carry_pct_per_annum = 5`

**Returns list with**: `pnl` (cumulative), `pnl_per_day` (daily), `data` (raw signals/trades), `summary` (yearly stats including `win_rate`, `gross_pnl`, `avg_n_eff`)

## Key Concepts

- **Correlation matrix**: Recomputed weekly (rolling, cached by ISO week) via `T.calcHistoricalCorrelationsMatrix()`
- **Portfolio sizing**: `V.portfolioSizing()` called daily for new signals
- **Antagonist netting**: If strategies disagree (buy+sell same instrument), directions summed; net=0 = no trade
- **N_effective**: Effective independent bets from eigenvalues: N_eff = (sum(lambda))^2 / sum(lambda^2)
- **gross_pnl**: Mean P&L as fraction of take-profit distance
- **Tradable instruments**: `A.filterInstruments("IB")`, further restricted by `limited_training_set` for historical consistency

## Related Documentation

- `Documentation/Strategies.md` - Strategy definitions and criteria
- `Documentation/Feature_Selection.md` - Feature selection per strategy
- `Session_Notes/2026-03-17_backtest_portfolio_sizing.md` - Sizing implementation details
