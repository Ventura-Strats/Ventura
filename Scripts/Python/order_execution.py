#!/usr/bin/env python3
"""
Order Execution Module using ib_insync

Clean implementation for executing orders via Interactive Brokers API.
Uses a "chase" strategy: start at favorable price, tick toward limit until filled.

Usage:
    from order_execution import Order, OrderExecutor

    executor = OrderExecutor(host='127.0.0.1', port=7497, client_id=10)
    executor.connect()

    order = Order(
        order_id=1,
        instrument_id=123,
        conid=12087792,  # EUR.USD
        direction=1,     # buy
        size=25000,
        price=1.0850,
        price_limit=1.0870,
        tick_size=0.00005
    )

    result = executor.execute(order)
    executor.disconnect()
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Optional, List
import time
import logging

from ib_insync import IB, Contract, Forex, Stock, Future, LimitOrder, Trade

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class OrderStatus(Enum):
    INITIALIZED = "initialized"
    SUBMITTED = "submitted"
    WORKING = "working"
    PARTIAL = "partial"
    FILLED = "filled"
    CANCELLED = "cancelled"
    ERROR = "error"


class AssetClass(Enum):
    FX = "fx"
    STOCK = "stock"
    FUTURE = "future"
    INDEX = "index"


@dataclass
class Order:
    """Represents a single order to be executed."""

    # Required fields
    order_id: int                    # Your internal order ID
    instrument_id: int               # Your DB instrument reference
    conid: int                       # IB contract ID
    direction: int                   # +1 = buy, -1 = sell
    size: float                      # Number of units (shares, contracts, or currency amount for FX)
    price: float                     # Starting order price
    price_limit: float               # Worst acceptable execution price
    tick_size: float                 # Minimum price increment for this instrument

    # Optional fields with defaults
    asset_class: AssetClass = AssetClass.FX
    symbol: str = ""                 # For display/logging (e.g., "EUR.USD")
    currency: str = "USD"            # Settlement currency
    exchange: str = "IDEALPRO"       # IB exchange (IDEALPRO for FX, SMART for stocks)

    # State tracking (managed by executor)
    status: OrderStatus = OrderStatus.INITIALIZED
    size_filled: float = 0.0
    avg_fill_price: float = 0.0
    ib_order_id: Optional[int] = None
    ib_trade: Optional[Trade] = None

    # Timestamps
    created_at: datetime = field(default_factory=datetime.now)
    submitted_at: Optional[datetime] = None
    filled_at: Optional[datetime] = None

    # Error tracking
    error_message: str = ""

    @property
    def action(self) -> str:
        """IB action string: 'BUY' or 'SELL'."""
        return "BUY" if self.direction > 0 else "SELL"

    @property
    def size_remaining(self) -> float:
        """Size left to fill."""
        return self.size - self.size_filled

    @property
    def is_complete(self) -> bool:
        """True if order is filled, cancelled, or errored."""
        return self.status in (OrderStatus.FILLED, OrderStatus.CANCELLED, OrderStatus.ERROR)

    def tick_toward_limit(self) -> bool:
        """
        Move price one tick toward the limit price.
        Returns True if price was moved, False if already at limit.
        """
        new_price = self.price + (self.direction * self.tick_size)
        new_price = round(new_price, 10)  # Avoid floating point issues

        # Check if we've hit the limit
        if self.direction > 0:  # Buying: price increasing toward limit
            if new_price > self.price_limit:
                return False
        else:  # Selling: price decreasing toward limit
            if new_price < self.price_limit:
                return False

        self.price = new_price
        return True

    def tick_away_from_limit(self) -> None:
        """Move price one tick away from limit (more favorable)."""
        self.price = self.price - (self.direction * self.tick_size)
        self.price = round(self.price, 10)


class OrderExecutor:
    """
    Executes orders using Interactive Brokers API via ib_insync.

    Execution strategy:
    1. Submit limit order at starting price (slightly better than mid)
    2. Wait for fill or timeout
    3. If not filled, modify order price one tick toward limit
    4. Repeat until filled or limit reached
    """

    def __init__(
        self,
        host: str = "127.0.0.1",
        port: int = 7497,
        client_id: int = 10,
        wait_seconds: float = 3.0,
        max_chase_seconds: float = 120.0
    ):
        self.host = host
        self.port = port
        self.client_id = client_id
        self.wait_seconds = wait_seconds
        self.max_chase_seconds = max_chase_seconds

        self.ib = IB()
        self._connected = False

    def connect(self) -> bool:
        """Connect to IB Gateway/TWS."""
        try:
            self.ib.connect(self.host, self.port, clientId=self.client_id)
            self._connected = True
            logger.info(f"Connected to IB at {self.host}:{self.port}")
            return True
        except Exception as e:
            logger.error(f"Failed to connect to IB: {e}")
            return False

    def disconnect(self) -> None:
        """Disconnect from IB."""
        if self._connected:
            self.ib.disconnect()
            self._connected = False
            logger.info("Disconnected from IB")

    def _build_contract(self, order: Order) -> Contract:
        """Build IB Contract from Order."""
        if order.asset_class == AssetClass.FX:
            # For FX, symbol should be like "EURUSD" (no dot)
            symbol = order.symbol.replace(".", "")
            contract = Forex(symbol)
        elif order.asset_class == AssetClass.STOCK:
            contract = Stock(order.symbol, order.exchange, order.currency)
        elif order.asset_class == AssetClass.FUTURE:
            # For futures, you'd need expiry info - simplified here
            contract = Future(conId=order.conid)
        else:
            # Fallback: create contract from conId
            contract = Contract(conId=order.conid)

        # Qualify the contract to fill in missing details
        self.ib.qualifyContracts(contract)
        return contract

    def _build_ib_order(self, order: Order) -> LimitOrder:
        """Build IB LimitOrder from Order."""
        return LimitOrder(
            action=order.action,
            totalQuantity=order.size_remaining,
            lmtPrice=order.price,
            tif="DAY"
        )

    def _update_order_from_trade(self, order: Order, trade: Trade) -> None:
        """Update Order state from IB Trade object."""
        status = trade.orderStatus

        order.size_filled = status.filled
        order.avg_fill_price = status.avgFillPrice if status.avgFillPrice else 0.0

        if status.status == "Filled":
            order.status = OrderStatus.FILLED
            order.filled_at = datetime.now()
        elif status.status == "Cancelled":
            order.status = OrderStatus.CANCELLED
        elif status.status in ("Inactive", "ApiCancelled"):
            order.status = OrderStatus.ERROR
            order.error_message = f"Order rejected: {status.status}"
        elif status.filled > 0:
            order.status = OrderStatus.PARTIAL
        elif status.status in ("Submitted", "PreSubmitted"):
            order.status = OrderStatus.WORKING

    def execute(self, order: Order) -> Order:
        """
        Execute a single order using chase strategy.

        Returns the order with updated status and fill information.
        """
        if not self._connected:
            order.status = OrderStatus.ERROR
            order.error_message = "Not connected to IB"
            return order

        logger.info(f"Executing order {order.order_id}: {order.action} {order.size} {order.symbol} @ {order.price}")

        # Build contract
        try:
            contract = self._build_contract(order)
        except Exception as e:
            order.status = OrderStatus.ERROR
            order.error_message = f"Failed to build contract: {e}"
            return order

        start_time = datetime.now()

        # Initial submission
        ib_order = self._build_ib_order(order)
        trade = self.ib.placeOrder(contract, ib_order)
        order.ib_trade = trade
        order.ib_order_id = trade.order.orderId
        order.submitted_at = datetime.now()
        order.status = OrderStatus.SUBMITTED

        logger.info(f"Order {order.order_id} submitted with IB order ID {order.ib_order_id}")

        # Chase loop
        while not order.is_complete:
            # Check timeout
            elapsed = (datetime.now() - start_time).total_seconds()
            if elapsed > self.max_chase_seconds:
                logger.warning(f"Order {order.order_id} timed out after {elapsed:.1f}s")
                self._cancel_order(order, trade)
                break

            # Wait for fills
            self.ib.sleep(self.wait_seconds)

            # Update order state from trade
            self._update_order_from_trade(order, trade)

            if order.is_complete:
                break

            # Try to tick toward limit
            if order.tick_toward_limit():
                # Modify order in place (no cancel needed)
                ib_order.lmtPrice = order.price
                trade = self.ib.placeOrder(contract, ib_order)
                order.ib_trade = trade
                logger.info(f"Order {order.order_id} price adjusted to {order.price}")
            else:
                # At limit price, keep waiting a bit longer then give up
                logger.info(f"Order {order.order_id} at limit price {order.price_limit}, waiting...")
                self.ib.sleep(self.wait_seconds * 2)
                self._update_order_from_trade(order, trade)

                if not order.is_complete:
                    logger.warning(f"Order {order.order_id} not filled at limit, cancelling")
                    self._cancel_order(order, trade)

        self._log_result(order)
        return order

    def execute_batch(self, orders: List[Order]) -> List[Order]:
        """
        Execute multiple orders sequentially.

        For parallel execution, you'd want to submit all orders first,
        then monitor them together. This is a simpler sequential approach.
        """
        results = []
        for order in orders:
            result = self.execute(order)
            results.append(result)
        return results

    def _cancel_order(self, order: Order, trade: Trade) -> None:
        """Cancel an order."""
        try:
            self.ib.cancelOrder(trade.order)
            self.ib.sleep(1.0)  # Wait for cancellation to process
            self._update_order_from_trade(order, trade)
            if order.status != OrderStatus.FILLED:
                order.status = OrderStatus.CANCELLED
        except Exception as e:
            logger.error(f"Error cancelling order {order.order_id}: {e}")

    def _log_result(self, order: Order) -> None:
        """Log the final result of an order."""
        if order.status == OrderStatus.FILLED:
            logger.info(
                f"Order {order.order_id} FILLED: {order.size_filled} @ {order.avg_fill_price:.5f}"
            )
        elif order.status == OrderStatus.PARTIAL:
            logger.warning(
                f"Order {order.order_id} PARTIAL: {order.size_filled}/{order.size} @ {order.avg_fill_price:.5f}"
            )
        elif order.status == OrderStatus.CANCELLED:
            logger.warning(f"Order {order.order_id} CANCELLED: filled {order.size_filled}/{order.size}")
        else:
            logger.error(f"Order {order.order_id} ERROR: {order.error_message}")


# Convenience function for quick execution
def execute_orders(
    orders: List[Order],
    host: str = "127.0.0.1",
    port: int = 7497,
    client_id: int = 10
) -> List[Order]:
    """
    Execute a list of orders and return results.

    Example:
        orders = [
            Order(order_id=1, instrument_id=1, conid=12087792, direction=1,
                  size=25000, price=1.0850, price_limit=1.0870, tick_size=0.00005,
                  symbol="EUR.USD", asset_class=AssetClass.FX)
        ]
        results = execute_orders(orders)
    """
    executor = OrderExecutor(host=host, port=port, client_id=client_id)

    if not executor.connect():
        for order in orders:
            order.status = OrderStatus.ERROR
            order.error_message = "Failed to connect to IB"
        return orders

    try:
        results = executor.execute_batch(orders)
    finally:
        executor.disconnect()

    return results


if __name__ == "__main__":
    # Example usage - won't run without IB connection
    print("Order Execution Module")
    print("=" * 50)
    print("Install ib_insync: pip install ib_insync")
    print()
    print("Example:")
    print("""
    from order_execution import Order, OrderExecutor, AssetClass

    executor = OrderExecutor(port=7497)
    executor.connect()

    order = Order(
        order_id=1,
        instrument_id=123,
        conid=12087792,
        direction=1,  # buy
        size=25000,
        price=1.0850,
        price_limit=1.0870,
        tick_size=0.00005,
        symbol="EUR.USD",
        asset_class=AssetClass.FX
    )

    result = executor.execute(order)
    print(f"Status: {result.status}, Filled: {result.size_filled}")

    executor.disconnect()
    """)
