#!/usr/bin/env python3
"""
Portfolio Sizing Module - Eigenvalue-Based Risk Allocation

Ports the R functions T.calcHistoricalCorrelationsMatrix and V.portfolioSizing to Python.

When multiple correlated signals fire together (e.g., "buy all indices" during a crash),
this recognizes it's effectively 1-2 independent bets, not 50, and sizes accordingly.

Key concept: N_effective = (sum(eigenvalues))^2 / sum(eigenvalues^2)
- Perfectly correlated signals: N_eff -> 1
- Perfectly uncorrelated signals: N_eff = n

Usage:
    from portfolio_sizing import PortfolioSizer

    sizer = PortfolioSizer(db_connection)

    signals = pd.DataFrame({
        'instrument_id': [1, 2, 3],
        'buy_sell': [1, 1, -1],
        'notional': [10000, 15000, 12000]  # notional for $1K P&L
    })

    sized = sizer.size_portfolio(signals, aum_total=1_000_000)
"""

import numpy as np
import pandas as pd
from dataclasses import dataclass
from typing import Optional, Tuple
import logging

logger = logging.getLogger(__name__)


@dataclass
class SizingConfig:
    """Configuration for portfolio sizing."""
    risk_per_bet_pct: float = 0.5       # Risk per independent bet as % of AUM
    max_daily_risk_pct: float = 5.0     # Maximum total daily risk as % of AUM
    correlation_adjustment: float = 0.0  # Adjust correlations: 0 = historical, 0.2 = inflate 20%
    min_weight: float = 0.0             # Minimum weight per signal (0 = allow exclusion)
    lookback_weeks: int = 104           # Weeks of history for correlation calc
    shrinkage: float = 0.1              # Shrinkage toward identity matrix


class PortfolioSizer:
    """
    Eigenvalue-based portfolio sizing.

    Calculates the effective number of independent bets from trade correlations
    and allocates risk accordingly.
    """

    def __init__(self, db_select_func=None):
        """
        Args:
            db_select_func: Function to execute SQL queries, returns DataFrame.
                           Signature: db_select_func(sql_query) -> pd.DataFrame
        """
        self.db_select = db_select_func
        self._cached_cor_matrix = None
        self._cached_instrument_ids = None

    def calc_historical_correlations(
        self,
        instrument_ids: list,
        lookback_weeks: int = 104,
        shrinkage: float = 0.1,
        as_of_date: Optional[str] = None
    ) -> np.ndarray:
        """
        Calculate weekly return correlation matrix across instruments.

        Uses Tuesday close to avoid Friday data release noise.
        Applies shrinkage toward identity matrix for numerical stability.

        Args:
            instrument_ids: List of instrument IDs to include
            lookback_weeks: Number of weeks of history
            shrinkage: Shrinkage factor (0 = pure sample, 1 = identity matrix)
            as_of_date: Calculate as of this date (YYYY-MM-DD), default = yesterday

        Returns:
            Correlation matrix as numpy array, with instrument_ids as row/column order
        """
        if self.db_select is None:
            logger.warning("No database connection, returning identity matrix")
            n = len(instrument_ids)
            return np.eye(n), instrument_ids

        if as_of_date is None:
            as_of_date = pd.Timestamp.now().normalize() - pd.Timedelta(days=1)
            as_of_date = as_of_date.strftime("%Y-%m-%d")

        lookback_days = lookback_weeks * 7 + 30  # buffer for weekends/holidays
        start_date = (pd.Timestamp(as_of_date) - pd.Timedelta(days=lookback_days)).strftime("%Y-%m-%d")

        # Load prices from database
        instrument_ids_str = ",".join(map(str, instrument_ids))
        sql = f"""
            SELECT instrument_id, date, close
            FROM histo_px_daily
            WHERE date >= '{start_date}' AND date <= '{as_of_date}'
            AND instrument_id IN ({instrument_ids_str})
        """

        dat = self.db_select(sql)

        if dat is None or len(dat) == 0:
            logger.warning("No price data found, returning identity matrix")
            n = len(instrument_ids)
            return np.eye(n), instrument_ids

        # Calculate weekly returns (Tuesday to Tuesday to avoid Friday noise)
        dat['date'] = pd.to_datetime(dat['date'])
        dat['weekday'] = dat['date'].dt.weekday  # Monday = 0, Tuesday = 1
        dat['year_week'] = dat['date'].dt.strftime('%Y-%W')

        # Prefer Tuesday (weekday 1), fallback to closest available
        def get_weekly_close(group):
            tuesday = group[group['weekday'] == 1]
            if len(tuesday) > 0:
                return tuesday.iloc[-1]['close']
            return group.iloc[-1]['close']

        weekly = dat.groupby(['instrument_id', 'year_week']).apply(
            get_weekly_close
        ).reset_index(name='close')

        # Pivot to wide format
        weekly_wide = weekly.pivot(index='year_week', columns='instrument_id', values='close')

        # Calculate returns
        returns = weekly_wide.pct_change().dropna()

        # Filter to requested instruments that have data
        available_ids = [i for i in instrument_ids if i in returns.columns]
        returns = returns[available_ids]

        if len(returns.columns) < 2:
            logger.warning("Insufficient instruments with data, returning identity matrix")
            n = len(instrument_ids)
            return np.eye(n), instrument_ids

        # Calculate sample correlation
        sample_cor = returns.corr().values

        # Apply shrinkage toward identity
        n = sample_cor.shape[0]
        identity = np.eye(n)
        shrunk_cor = (1 - shrinkage) * sample_cor + shrinkage * identity

        # Store column order for mapping
        self._cached_cor_matrix = shrunk_cor
        self._cached_instrument_ids = list(returns.columns)

        return shrunk_cor, list(returns.columns)

    def _net_antagonist_signals(self, signals: pd.DataFrame) -> pd.DataFrame:
        """
        Net signals on the same asset: if strategies disagree, sum the directions.

        +1 + +1 + -1 = +1 (net buy)
        +1 + -1 = 0 (cancel, remove)
        """
        netted = signals.groupby('instrument_id').agg({
            'buy_sell': 'sum',
            'notional': 'mean'
        }).reset_index()

        netted = netted[netted['buy_sell'] != 0].copy()
        netted['buy_sell'] = np.sign(netted['buy_sell']).astype(int)

        return netted

    def _build_trade_correlation_matrix(
        self,
        signals: pd.DataFrame,
        asset_cor_matrix: np.ndarray,
        asset_ids: list
    ) -> np.ndarray:
        """
        Build trade correlation matrix: direction_i * direction_j * asset_cor[i,j]
        """
        n = len(signals)
        trade_cor = np.zeros((n, n))

        instrument_ids = signals['instrument_id'].values
        directions = signals['buy_sell'].values

        # Create mapping from instrument_id to matrix index
        id_to_idx = {inst_id: idx for idx, inst_id in enumerate(asset_ids)}

        for i in range(n):
            for j in range(i, n):
                inst_i = instrument_ids[i]
                inst_j = instrument_ids[j]
                dir_i = directions[i]
                dir_j = directions[j]

                if inst_i == inst_j:
                    # Same instrument: correlation depends on direction
                    trade_cor[i, j] = dir_i * dir_j
                else:
                    # Different instruments: look up asset correlation
                    idx_i = id_to_idx.get(inst_i)
                    idx_j = id_to_idx.get(inst_j)

                    if idx_i is not None and idx_j is not None:
                        asset_cor = asset_cor_matrix[idx_i, idx_j]
                        trade_cor[i, j] = dir_i * dir_j * asset_cor
                    else:
                        # Instrument not found, assume uncorrelated
                        trade_cor[i, j] = 0

                trade_cor[j, i] = trade_cor[i, j]

        return trade_cor

    def _apply_correlation_adjustment(
        self,
        trade_cor: np.ndarray,
        adjustment: float
    ) -> np.ndarray:
        """
        Adjust off-diagonal correlations by multiplier (1 + adjustment).
        Bounded to [-1, 1].
        """
        if adjustment == 0:
            return trade_cor

        adjusted = trade_cor.copy()
        n = trade_cor.shape[0]

        for i in range(n):
            for j in range(n):
                if i != j:
                    raw_cor = trade_cor[i, j]
                    scaled_cor = raw_cor * (1 + adjustment)
                    # Bound to [-1, 1]
                    adjusted[i, j] = np.sign(scaled_cor) * min(abs(scaled_cor), 1)

        return adjusted

    def _calc_effective_num_bets(self, trade_cor: np.ndarray) -> float:
        """
        Calculate effective number of independent bets using eigenvalues.
        N_eff = (sum of eigenvalues)^2 / sum(eigenvalues^2)
        """
        eigenvalues = np.linalg.eigvalsh(trade_cor)
        # Clean up numerical issues (small negative eigenvalues)
        eigenvalues = np.maximum(eigenvalues, 0)

        sum_lambda = np.sum(eigenvalues)
        sum_lambda_sq = np.sum(eigenvalues ** 2)

        if sum_lambda_sq == 0:
            return trade_cor.shape[0]

        n_eff = sum_lambda ** 2 / sum_lambda_sq
        return n_eff

    def _solve_min_variance(
        self,
        trade_cor: np.ndarray,
        min_weight: float = 0
    ) -> np.ndarray:
        """
        Solve minimum variance optimization for weight distribution.

        minimize: w' * Sigma * w
        subject to: sum(w) = 1, w >= min_weight
        """
        n = trade_cor.shape[0]

        try:
            # Try using cvxopt or scipy for quadratic programming
            from scipy.optimize import minimize

            # Make matrix positive definite
            D = trade_cor + np.eye(n) * 1e-8

            def objective(w):
                return w @ D @ w

            def grad(w):
                return 2 * D @ w

            # Constraints
            constraints = [
                {'type': 'eq', 'fun': lambda w: np.sum(w) - 1}  # sum = 1
            ]

            # Bounds
            bounds = [(min_weight, 1) for _ in range(n)]

            # Initial guess: equal weights
            w0 = np.ones(n) / n

            result = minimize(
                objective, w0,
                method='SLSQP',
                jac=grad,
                bounds=bounds,
                constraints=constraints
            )

            if result.success:
                weights = result.x
                weights = np.maximum(weights, min_weight)
                weights = weights / np.sum(weights)  # renormalize
                return weights
            else:
                logger.warning(f"Optimization failed: {result.message}, using equal weights")
                return np.ones(n) / n

        except Exception as e:
            logger.warning(f"Optimization error: {e}, using equal weights")
            return np.ones(n) / n

    def size_portfolio(
        self,
        signals: pd.DataFrame,
        aum_total: float = 1_000_000,
        config: Optional[SizingConfig] = None,
        cor_matrix: Optional[np.ndarray] = None,
        cor_instrument_ids: Optional[list] = None
    ) -> pd.DataFrame:
        """
        Size a portfolio of signals using eigenvalue-based risk allocation.

        Args:
            signals: DataFrame with columns: instrument_id, buy_sell (+1/-1), notional
                     (notional = position size for $1K P&L)
            aum_total: Total AUM in USD
            config: SizingConfig object (uses defaults if None)
            cor_matrix: Pre-computed correlation matrix (optional)
            cor_instrument_ids: Instrument IDs corresponding to cor_matrix columns

        Returns:
            signals DataFrame with added columns:
            - weight: allocation weight (sums to 1)
            - sized_notional: actual notional to trade
            - n_effective: effective number of independent bets
        """
        if config is None:
            config = SizingConfig()

        signals = signals.copy()
        n_signals = len(signals)

        # Edge case: no signals
        if n_signals == 0:
            signals['weight'] = []
            signals['sized_notional'] = []
            signals['n_effective'] = []
            return signals

        # Edge case: single signal
        if n_signals == 1:
            total_risk_pct = min(config.risk_per_bet_pct, config.max_daily_risk_pct)
            total_risk_usd = aum_total * total_risk_pct / 100
            signals['weight'] = 1.0
            signals['sized_notional'] = signals['notional'] * total_risk_usd / 1000
            signals['n_effective'] = 1.0
            return signals

        # Get or compute correlation matrix
        if cor_matrix is None:
            instrument_ids = signals['instrument_id'].unique().tolist()
            cor_matrix, cor_instrument_ids = self.calc_historical_correlations(
                instrument_ids,
                lookback_weeks=config.lookback_weeks,
                shrinkage=config.shrinkage
            )

        # Net antagonist signals
        signals_netted = self._net_antagonist_signals(signals)
        n_netted = len(signals_netted)

        # Handle edge cases after netting
        if n_netted == 0:
            logger.warning("All signals cancelled out after netting")
            signals['weight'] = 0.0
            signals['sized_notional'] = 0.0
            signals['n_effective'] = 0.0
            return signals

        if n_netted == 1:
            total_risk_pct = min(config.risk_per_bet_pct, config.max_daily_risk_pct)
            total_risk_usd = aum_total * total_risk_pct / 100

            netted_inst = signals_netted['instrument_id'].iloc[0]
            netted_dir = signals_netted['buy_sell'].iloc[0]

            # Signals matching netted direction get weight
            matches = (signals['instrument_id'] == netted_inst) & (np.sign(signals['buy_sell']) == netted_dir)
            n_matching = matches.sum()

            signals['weight'] = np.where(matches, 1.0 / n_matching, 0.0)
            signals['sized_notional'] = signals['weight'] * signals['notional'] * total_risk_usd / 1000
            signals['n_effective'] = 1.0
            return signals

        # Build trade correlation matrix on netted signals
        trade_cor = self._build_trade_correlation_matrix(
            signals_netted, cor_matrix, cor_instrument_ids
        )

        # Apply correlation adjustment
        trade_cor_adjusted = self._apply_correlation_adjustment(
            trade_cor, config.correlation_adjustment
        )

        # Calculate effective number of independent bets
        n_effective = self._calc_effective_num_bets(trade_cor_adjusted)

        # Calculate total risk based on N_effective
        total_risk_pct = min(n_effective * config.risk_per_bet_pct, config.max_daily_risk_pct)
        total_risk_usd = aum_total * total_risk_pct / 100

        # Solve min-variance optimization for weight distribution
        weights_netted = self._solve_min_variance(trade_cor_adjusted, config.min_weight)

        # Map weights back to original signals
        signals_netted = signals_netted.copy()
        signals_netted['weight_netted'] = weights_netted

        # Merge weights back
        signals = signals.merge(
            signals_netted[['instrument_id', 'buy_sell', 'weight_netted']].rename(
                columns={'buy_sell': 'buy_sell_netted'}
            ),
            on='instrument_id',
            how='left'
        )

        # Only signals matching the netted direction get weight
        signals['matches_netted'] = np.sign(signals['buy_sell']) == signals['buy_sell_netted']

        # Count matching signals per instrument for weight splitting
        match_counts = signals.groupby('instrument_id')['matches_netted'].sum()
        signals['n_matching'] = signals['instrument_id'].map(match_counts)

        # Calculate final weights
        signals['weight'] = np.where(
            signals['matches_netted'],
            signals['weight_netted'] / signals['n_matching'],
            0.0
        )

        # Calculate sized notional
        signals['sized_notional'] = signals['weight'] * signals['notional'] * total_risk_usd / 1000
        signals['n_effective'] = n_effective

        # Clean up temporary columns
        signals = signals.drop(columns=['buy_sell_netted', 'weight_netted', 'matches_netted', 'n_matching'])

        return signals


def size_signals(
    signals: pd.DataFrame,
    aum_total: float = 1_000_000,
    risk_per_bet_pct: float = 0.5,
    max_daily_risk_pct: float = 5.0,
    db_select_func=None
) -> pd.DataFrame:
    """
    Convenience function to size a portfolio of signals.

    Args:
        signals: DataFrame with columns: instrument_id, buy_sell (+1/-1), notional
        aum_total: Total AUM in USD
        risk_per_bet_pct: Risk per independent bet as % of AUM
        max_daily_risk_pct: Maximum total daily risk as % of AUM
        db_select_func: Function to query database for correlation data

    Returns:
        signals with weight, sized_notional, n_effective columns added
    """
    sizer = PortfolioSizer(db_select_func)
    config = SizingConfig(
        risk_per_bet_pct=risk_per_bet_pct,
        max_daily_risk_pct=max_daily_risk_pct
    )
    return sizer.size_portfolio(signals, aum_total=aum_total, config=config)


if __name__ == "__main__":
    # Example usage without database
    print("Portfolio Sizing Module - Eigenvalue-Based Risk Allocation")
    print("=" * 60)

    # Create sample signals
    signals = pd.DataFrame({
        'instrument_id': [1, 2, 3, 4, 5],
        'buy_sell': [1, 1, 1, -1, 1],      # 4 buys, 1 sell
        'notional': [10000, 15000, 12000, 8000, 20000]
    })

    print("\nInput signals:")
    print(signals)

    # Create sizer without DB (will use identity matrix for correlations)
    sizer = PortfolioSizer(db_select_func=None)

    # Size the portfolio
    sized = sizer.size_portfolio(
        signals,
        aum_total=1_000_000,
        config=SizingConfig(risk_per_bet_pct=0.5, max_daily_risk_pct=5.0)
    )

    print("\nSized signals:")
    print(sized[['instrument_id', 'buy_sell', 'notional', 'weight', 'sized_notional', 'n_effective']])
    print(f"\nEffective number of bets: {sized['n_effective'].iloc[0]:.2f}")
    print(f"Total weight: {sized['weight'].sum():.4f}")
