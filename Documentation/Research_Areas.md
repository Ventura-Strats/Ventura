# Research Areas

Ideas for features and strategies to investigate. Prioritised by expected value-add relative to what the system already captures.

## New Features

### 1. Cross-Asset Regime Features (High Priority)

All current ~237 features are per-instrument time series. Market-wide context is qualitatively different information and likely the highest-value addition.

#### Dispersion / Correlation Regime

The correlation eigenvalue structure is already computed for portfolio sizing. The same concept applied as a feature:

- **N_effective as a feature**: computed daily across all instruments at different lookback windows (e.g., 1-week, 4-week rolling). Tells the model whether the market is in "one-factor mode" (crisis, everything correlated, N_eff low) or "dispersion mode" (idiosyncratic, N_eff high).
- **Average pairwise correlation**: across the full universe and within asset classes (average FX correlation, average index correlation).
- **Rate of change of the above**: is the market moving toward or away from one-factor mode. Transitions may be more predictable than steady states.

#### VIX and Implied Volatility

- **VIX daily close**: available from CBOE, free historical data. Powerful regime indicator even without trading it.
- **VIX term structure slope**: VIX vs VIX3M (or VIX9D). Contango is "normal"; backwardation signals stress. One of the better-documented regime indicators.
- **VIX relative to its own recent range**: the `pos_in_range` concept applied to VIX.
- **Equivalent implied vols for other asset classes**: MOVE index (bonds), currency implied vols if accessible.

#### Yield Curve / Rates

- **2y-10y slope** (or 3m-10y): well-known macro regime indicator, cycles slowly enough to be meaningful on a 5-day horizon.
- **Change in yield curve slope** over recent weeks.

#### Cross-Asset Correlations

- **Equity-bond correlation**: rolling correlation between equity index returns and bond yield changes. Positive = "normal" risk-on/off; negative = unusual. Regime tends to be persistent, changes character once or twice a year.
- **USD-equity correlation**: rolling correlation between USD index and equity indices.

**Implementation note**: these are all date-level features (same value for every instrument on a given date). Would add ~15-25 new columns. The RF can learn interactions like "Strategy 1 works well when VIX is elevated but correlation is low" without explicit specification.

### 2. Technical Indicators (Medium Priority)

These add per-instrument information but from angles not currently covered.

#### Hurst Exponent

Measures whether an instrument is trending (H > 0.5) or mean-reverting (H < 0.5) at a given timescale. `momersion` captures a related idea, but Hurst is better grounded and gives different information at different timescales. Computing at 20-day and 65-day windows would reveal whether the trending/mean-reverting character is consistent across scales or diverging.

#### Autocorrelation of Returns

Autocorrelation at lags 1-5. Positive = trending, negative = mean-reverting. Different from `trend_*_r2` because it captures serial dependence directly without fitting a line. Could be computed as a rolling window (e.g., 20-day rolling autocorrelation at lag 1).

#### Realised Variance Decomposition

Separate total realised variance into continuous and jump components (bipower variation vs realised variance). The ratio tells you whether recent vol came from steady moves or discrete jumps. Likely matters for whether trend-following or mean-reversion strategies work in the near term.

#### Cross-Sectional Rank Features

Where does this instrument's 20-day momentum rank vs all other instruments in the same asset class? An instrument with `psi_20 = 0.5` means something different if everything is up 0.5 (broad rally) vs if it's the only one (idiosyncratic move). Could apply the same idea to volatility rank, trend strength rank, etc.

### 3. Strategy Historical Performance as a Feature

**Idea**: for a given strategy, add a feature measuring how well it has performed over the past 2-5 years (e.g., rolling win rate, rolling Sharpe, or rolling gross P&L).

**Potential benefit**: some strategies may go through extended periods where they don't work (e.g., Strategy 5 reportedly should have been faded). A performance feature could let the RF learn to discount a strategy's signal during its weak regimes.

**Overfitting risk**: high. This is a meta-feature that is slow-moving and has very few effective degrees of freedom. Over a 5-year window, there are only a handful of independent regime observations. The RF could latch onto in-sample strategy performance patterns that don't generalise. A few mitigations to consider:

- Use a very long window (3-5 years) so it changes slowly and can't overfit to short-term noise.
- Use a simple metric (win rate or gross P&L) rather than something complex.
- Test rigorously with true out-of-sample (the backtest already does walk-forward, so this is testable).
- Consider using it as a criteria filter rather than an RF feature: e.g., "only trade Strategy 5 when its rolling 3-year win rate is above 50%". This is more transparent and less prone to RF overfitting than burying it as one feature among 120.

**A related, possibly safer idea**: rather than strategy-level performance, use the **regime features above** (VIX, correlation, dispersion) which capture the *why* behind strategy performance variation. If Strategy 5 fails during high-dispersion periods, the RF can learn that from VIX and correlation features without the circularity of using the strategy's own track record as input.

### 4. Lower Priority / Skeptical

#### GARCH Conditional Volatility

Extensive vol features already exist (deciles, ratios, up/down vol, diffs). GARCH would likely be highly correlated with what's already there.

#### Classic Indicators (RSI, MACD, Stochastic)

These are transformations of price momentum and moving averages. The splines + psi features already capture this in a more principled way.

#### Seasonality (Day-of-Week, Month-of-Year)

Effects exist but are weak and well-arbitraged. With a 5-day hold, day-of-week effects mostly wash out. Month effects are marginal. The RF would need a lot of data to learn them reliably and the overfitting risk is high.

---

## New Strategies

Given the framework (criteria define a regime, RF predicts direction, 5-day hold), these are regimes worth testing as new strategy definitions.

### 1. Volatility Regime Change

VIX (or realised vol) has just crossed a threshold -- either spiking into a new regime or collapsing back to calm. The transition period is often more predictable than the steady state.

**Possible criteria**:
- VIX level above/below a threshold
- VIX term structure in backwardation
- Rate of change of VIX or realised vol

**Rationale**: complements existing Strategies 4 and 5 which focus on quiet markets. A spike strategy would capture the other side.

**Requires**: VIX data as a feature (see above).

### 2. Cross-Asset Divergence

An instrument's recent move is inconsistent with what correlated instruments are doing. One index sells off while all others are flat/up, or one FX pair moves while its usual co-movers don't follow.

**Possible criteria**:
- Deviation from beta-adjusted return vs asset class average exceeds a threshold
- The divergence has persisted for N days (not just noise)
- Instrument's own trend is otherwise intact

**Rationale**: bets on mean-reversion of the residual within 5 days. Exploits information from the cross-section that no current strategy uses.

**Requires**: cross-sectional rank features or factor model residuals.

### 3. Post-Gap Continuation/Reversal

Instrument opens with a significant gap (open far from previous close), conditional on whether the gap is with or against the prevailing trend.

**Possible criteria**:
- Gap size exceeds a threshold (could use `open/lag(close) - 1`)
- Gap direction relative to slow spline slope
- Volatility regime (gaps in quiet markets vs volatile markets behave differently)

**Rationale**: gap-with-trend in strong trends tends to continue; gap-against-trend in quiet markets tends to reverse. Existing features (`psi_1`, spline slopes) can build this.

**Requires**: open-vs-previous-close feature (may need to add if not already computed).

### 4. Carry + Trend for FX

Interest rate differential (carry) combined with trend direction. High-carry currencies in an uptrend tend to continue; high-carry currencies breaking down tend to crash hard (carry unwind).

**Possible criteria**:
- Carry (forward points or rate differential) above a threshold
- Trend direction (spline slope) aligned or diverging from carry direction
- Volatility regime (carry works in low-vol, unwinds in high-vol)

**Rationale**: carry is one of the most robust FX factors. Combining it with trend and vol regime conditioning within the existing framework could work well.

**Requires**: adding carry/rate differential data. Could approximate from forward points if available, or source central bank rate data.

### 5. Breadth Divergence for Indices

An index makes a new high but the fraction of instruments also at highs has declined (narrowing breadth). Classic distribution/top signal.

**Possible criteria**:
- Instrument near its own 65-day or 130-day high
- Fraction of instruments above their 65-day spline has been declining
- The divergence has persisted for at least N days

**Rationale**: classic technical analysis concept (advance-decline divergence), recast as quantitative criteria. Could capture distribution phases before reversals.

**Requires**: cross-sectional breadth feature (fraction of universe above spline, or similar).

---

## Implementation Priority

Suggested order based on expected value-add and implementation effort:

1. **VIX + VIX term structure** -- single data source, date-level features, well-documented regime indicator
2. **Dispersion / N_eff as feature** -- already have the computation in `V.portfolioSizing`, just need to expose it as a feature in `T.calcTechnicals`
3. **Cross-sectional rank features** -- straightforward to compute, adds genuinely new information
4. **Autocorrelation / Hurst** -- per-instrument, moderate computation
5. **Yield curve slope** -- single data source, date-level
6. **New strategies** -- can start testing once features 1-3 are in place
7. **Strategy performance as feature** -- test last, high overfitting risk, may be better addressed by regime features indirectly
