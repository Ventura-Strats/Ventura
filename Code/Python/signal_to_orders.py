#!/usr/bin/env python3
"""
Signal to Orders Converter

Converts trading signals into Order objects ready for execution.
Integrates portfolio sizing with eigenvalue-based risk allocation.

Data flow:
1. Read signals from DB or combined_orders CSV
2. Apply portfolio sizing (eigenvalue-based)
3. Convert to Order objects
4. Execute via order_execution module

Usage:
    from signal_to_orders import SignalConverter

    converter = SignalConverter(account_id=1, execution_time_id=7)
    orders = converter.prepare_orders(aum_total=100_000)

    # Execute
    from order_execution import execute_orders
    results = execute_orders(orders, port=7497)
"""

import pandas as pd
import numpy as np
from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import List, Optional, Dict
from pathlib import Path
import logging

# Local imports
try:
    from order_execution import Order, AssetClass, OrderStatus
    from portfolio_sizing import PortfolioSizer, SizingConfig
    import db
    import init
except ImportError as e:
    logging.warning(f"Import error (may be OK for standalone testing): {e}")

logger = logging.getLogger(__name__)


@dataclass
class ExecutionConfig:
    """Configuration for order execution."""
    # Portfolio sizing
    risk_per_bet_pct: float = 0.5
    max_daily_risk_pct: float = 5.0
    correlation_adjustment: float = 0.0

    # Order pricing
    initial_ticks_from_mid: int = 2        # Start order X ticks better than mid
    limit_price_pct_from_mid: float = 0.25  # Max slippage as % (0.25 = 0.25%)

    # IB connection
    ib_host: str = "127.0.0.1"
    ib_port_account_1: int = 7496
    ib_port_account_2: int = 7497


class SignalConverter:
    """
    Converts trading signals to Order objects with portfolio sizing.
    """

    def __init__(
        self,
        account_id: int = 1,
        execution_time_id: Optional[int] = None,
        config: Optional[ExecutionConfig] = None,
        data_dir: Optional[str] = None
    ):
        self.account_id = account_id
        self.execution_time_id = execution_time_id
        self.config = config or ExecutionConfig()

        # Set data directory
        if data_dir is None:
            try:
                self.data_dir = Path(init.DIRECTORY_DATA)
            except:
                self.data_dir = Path("/home/fls/Mount/Synology/Models/Ventura/Data")
        else:
            self.data_dir = Path(data_dir)

        # Database connection
        self._db_select = None
        try:
            self._db_select = db.select
        except:
            logger.warning("Database module not available")

        # Portfolio sizer
        self.sizer = PortfolioSizer(db_select_func=self._db_select)

        # Instrument metadata cache
        self._instruments = None
        self._tick_sizes = None

    def _get_today_str(self) -> str:
        """Get today's date string."""
        try:
            return init.TODAY_STR
        except:
            return datetime.now().strftime("%Y-%m-%d")

    def _find_execution_time(self) -> int:
        """Determine current execution time ID based on time of day."""
        if self.execution_time_id is not None:
            return self.execution_time_id

        # Default logic: execution times are typically hourly
        # This should match your schedule_execution table
        hour = datetime.now().hour
        return max(1, min(hour, 24))

    def _get_ib_port(self) -> int:
        """Get IB port for this account."""
        if self.account_id == 1:
            return self.config.ib_port_account_1
        return self.config.ib_port_account_2

    def load_combined_orders(self) -> pd.DataFrame:
        """
        Load combined orders from CSV file.

        Returns DataFrame with columns:
            order_id, account_id, instrument_id, ticker, future_id, conid,
            tick_size, buy_sell, buy_sell_action, size_to_do, px_order, ...
        """
        exec_time_id = self._find_execution_time()
        today = datetime.now()

        file_path = (
            self.data_dir / "Orders" / "Combined" /
            today.strftime("%Y-%m") /
            today.strftime("%Y-%m-%d") /
            f"combined_orders_{self.account_id}_{today.strftime('%Y%m%d')}-{exec_time_id:02d}.csv"
        )

        if not file_path.exists():
            logger.warning(f"Combined orders file not found: {file_path}")
            return pd.DataFrame()

        df = pd.read_csv(file_path)
        logger.info(f"Loaded {len(df)} orders from {file_path}")
        return df

    def load_signals_from_db(self) -> pd.DataFrame:
        """
        Load signals directly from database predictions.

        Returns DataFrame with: instrument_id, buy_sell, notional, price, etc.
        """
        if self._db_select is None:
            logger.error("Database connection not available")
            return pd.DataFrame()

        exec_time_id = self._find_execution_time()

        # Get instruments for this execution time
        sql_instruments = f"""
            SELECT instrument_id, ticker, pair, asset_class, conid_spot AS conid
            FROM static_instrument
            WHERE execution_time_id = {exec_time_id}
            AND use_for_trading_ib = 1
        """
        instruments = self._db_select(sql_instruments)

        if instruments is None or len(instruments) == 0:
            logger.warning(f"No tradable instruments for execution_time_id {exec_time_id}")
            return pd.DataFrame()

        instrument_ids = ",".join(map(str, instruments['instrument_id']))

        # Get predictions
        sql_predictions = f"""
            SELECT P.instrument_id, T.outcome AS predict, P.close AS price,
                   P.t_up, P.t_dn, P.proba_up, P.proba_down
            FROM live_predict P
            LEFT JOIN static_trade_outcome T ON T.outcome_id = P.outcome_id
            WHERE P.score = 10
            AND P.use_weights = 0
            AND P.instrument_id IN ({instrument_ids})
        """
        predictions = self._db_select(sql_predictions)

        if predictions is None or len(predictions) == 0:
            logger.warning("No predictions found")
            return pd.DataFrame()

        # Merge with instrument info
        signals = predictions.merge(instruments, on='instrument_id', how='left')

        # Calculate buy_sell and notional
        signals['buy_sell'] = 0
        signals.loc[signals['predict'] == 'up', 'buy_sell'] = 1
        signals.loc[signals['predict'] == 'down', 'buy_sell'] = -1

        # Filter to actionable signals
        signals = signals[signals['buy_sell'] != 0].copy()

        # Calculate notional for $1K P&L (simplified)
        signals['tp_pct'] = signals['t_up'] / signals['price'] - 1
        signals['notional'] = 1000 / signals['tp_pct'].abs()

        return signals

    def get_tick_size(self, instrument_id: int, conid: int) -> float:
        """Get tick size for an instrument."""
        if self._tick_sizes is None:
            self._load_tick_sizes()

        # Try instrument_id first, then conid
        if instrument_id in self._tick_sizes:
            return self._tick_sizes[instrument_id]
        if conid in self._tick_sizes:
            return self._tick_sizes[conid]

        # Default tick sizes by asset class (crude fallback)
        return 0.0001  # Default for FX

    def _load_tick_sizes(self):
        """Load tick sizes from database."""
        self._tick_sizes = {}

        if self._db_select is None:
            return

        # From instrument attributes
        sql = """
            SELECT A.instrument_id, A.value AS tick_size
            FROM instrument_attribute_dbl A
            JOIN instrument_attribute_type T ON T.attribute_id = A.attribute_id
            WHERE T.attribute = 'tick_size'
        """
        result = self._db_select(sql)
        if result is not None:
            for _, row in result.iterrows():
                self._tick_sizes[row['instrument_id']] = row['tick_size']

        # From futures
        sql = """
            SELECT E.conid, C.tick_size
            FROM static_future_expiry E
            JOIN static_future_contract C ON C.future_id = E.future_id
        """
        result = self._db_select(sql)
        if result is not None:
            for _, row in result.iterrows():
                self._tick_sizes[row['conid']] = row['tick_size']

    def get_asset_class(self, row: pd.Series) -> AssetClass:
        """Determine asset class from signal data."""
        asset_class_str = str(row.get('asset_class', '')).lower()

        if 'fx' in asset_class_str:
            return AssetClass.FX
        elif 'index' in asset_class_str or 'equity' in asset_class_str:
            return AssetClass.INDEX
        elif 'future' in asset_class_str:
            return AssetClass.FUTURE
        else:
            return AssetClass.STOCK

    def calc_order_prices(
        self,
        mid_price: float,
        buy_sell: int,
        tick_size: float
    ) -> tuple:
        """
        Calculate initial order price and limit price.

        Args:
            mid_price: Current mid price
            buy_sell: +1 for buy, -1 for sell
            tick_size: Minimum price increment

        Returns:
            (order_price, limit_price)
        """
        # Initial price: a few ticks better than mid
        ticks_away = self.config.initial_ticks_from_mid
        order_price = mid_price - buy_sell * ticks_away * tick_size

        # Limit price: worst acceptable price
        limit_pct = self.config.limit_price_pct_from_mid / 100
        limit_price = mid_price * (1 + buy_sell * limit_pct)

        # Round to tick
        order_price = round(order_price / tick_size) * tick_size
        limit_price = round(limit_price / tick_size) * tick_size

        return order_price, limit_price

    def signals_to_orders(
        self,
        signals: pd.DataFrame,
        aum_total: float
    ) -> List[Order]:
        """
        Convert signals DataFrame to list of Order objects.

        Applies portfolio sizing before creating orders.
        """
        if len(signals) == 0:
            return []

        # Ensure required columns exist
        required_cols = ['instrument_id', 'buy_sell']
        for col in required_cols:
            if col not in signals.columns:
                raise ValueError(f"Missing required column: {col}")

        # Add notional if missing (use sized_notional or default)
        if 'notional' not in signals.columns:
            if 'notional_for_1k_pnl' in signals.columns:
                signals['notional'] = signals['notional_for_1k_pnl']
            elif 'size_to_do' in signals.columns:
                signals['notional'] = signals['size_to_do']
            else:
                signals['notional'] = 10000  # Default

        # Apply portfolio sizing
        sizing_config = SizingConfig(
            risk_per_bet_pct=self.config.risk_per_bet_pct,
            max_daily_risk_pct=self.config.max_daily_risk_pct,
            correlation_adjustment=self.config.correlation_adjustment
        )

        signals_sized = self.sizer.size_portfolio(
            signals[['instrument_id', 'buy_sell', 'notional']].copy(),
            aum_total=aum_total,
            config=sizing_config
        )

        # Merge sizing back
        signals = signals.merge(
            signals_sized[['instrument_id', 'buy_sell', 'weight', 'sized_notional', 'n_effective']],
            on=['instrument_id', 'buy_sell'],
            how='left'
        )

        # Convert to Order objects
        orders = []
        for idx, row in signals.iterrows():
            # Skip zero-weight signals
            if row.get('weight', 0) <= 0 or row.get('sized_notional', 0) <= 0:
                continue

            # Get prices
            mid_price = row.get('px_order', row.get('price', row.get('close', 0)))
            if mid_price <= 0:
                logger.warning(f"Invalid price for instrument {row['instrument_id']}, skipping")
                continue

            tick_size = row.get('tick_size', self.get_tick_size(
                row['instrument_id'],
                row.get('conid', 0)
            ))

            order_price, limit_price = self.calc_order_prices(
                mid_price, row['buy_sell'], tick_size
            )

            # Create Order object
            order = Order(
                order_id=int(row.get('order_id', idx + 1)),
                instrument_id=int(row['instrument_id']),
                conid=int(row.get('conid', 0)),
                direction=int(row['buy_sell']),
                size=float(row['sized_notional']),
                price=order_price,
                price_limit=limit_price,
                tick_size=tick_size,
                asset_class=self.get_asset_class(row),
                symbol=str(row.get('ticker', row.get('pair', ''))),
                currency=str(row.get('currency', 'USD'))
            )

            orders.append(order)

        logger.info(f"Created {len(orders)} orders from {len(signals)} signals")
        return orders

    def prepare_orders(
        self,
        aum_total: float,
        source: str = "file"
    ) -> List[Order]:
        """
        Prepare orders ready for execution.

        Args:
            aum_total: Total AUM for sizing
            source: "file" to load from combined_orders CSV, "db" for fresh from DB

        Returns:
            List of Order objects ready for execution
        """
        if source == "file":
            signals = self.load_combined_orders()
        else:
            signals = self.load_signals_from_db()

        if len(signals) == 0:
            logger.warning("No signals to process")
            return []

        return self.signals_to_orders(signals, aum_total)


def prepare_and_execute(
    account_id: int = 1,
    aum_total: float = 100_000,
    dry_run: bool = True,
    execution_time_id: Optional[int] = None
) -> List:
    """
    Complete workflow: prepare signals and execute orders.

    Args:
        account_id: IB account (1 or 2)
        aum_total: Total AUM for sizing
        dry_run: If True, don't actually execute (just return orders)
        execution_time_id: Override execution time ID

    Returns:
        List of Order objects (with results if not dry_run)
    """
    from order_execution import execute_orders

    # Prepare orders
    converter = SignalConverter(
        account_id=account_id,
        execution_time_id=execution_time_id
    )
    orders = converter.prepare_orders(aum_total=aum_total)

    if len(orders) == 0:
        logger.info("No orders to execute")
        return []

    logger.info(f"Prepared {len(orders)} orders:")
    for order in orders:
        logger.info(f"  {order.action} {order.size:.0f} {order.symbol} @ {order.price:.5f}")

    if dry_run:
        logger.info("Dry run - not executing")
        return orders

    # Execute
    port = converter._get_ib_port()
    results = execute_orders(orders, port=port)

    return results


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)

    print("Signal to Orders Converter")
    print("=" * 60)

    # Example: create sample signals and convert to orders
    sample_signals = pd.DataFrame({
        'order_id': [1, 2, 3],
        'instrument_id': [101, 102, 103],
        'ticker': ['EURUSD', 'GBPUSD', 'USDJPY'],
        'asset_class': ['fx_dm', 'fx_dm', 'fx_dm'],
        'conid': [12087792, 12087797, 15016062],
        'buy_sell': [1, -1, 1],
        'px_order': [1.0850, 1.2650, 150.25],
        'tick_size': [0.00005, 0.00005, 0.005],
        'notional_for_1k_pnl': [100000, 80000, 65000]
    })

    print("\nSample signals:")
    print(sample_signals)

    # Create converter (without DB connection for demo)
    converter = SignalConverter(account_id=1)
    converter._db_select = None  # Disable DB for demo

    # Convert to orders
    orders = converter.signals_to_orders(sample_signals, aum_total=500_000)

    print(f"\nGenerated {len(orders)} orders:")
    for order in orders:
        print(f"  {order.order_id}: {order.action} {order.size:,.0f} {order.symbol}")
        print(f"         Price: {order.price:.5f}, Limit: {order.price_limit:.5f}")
