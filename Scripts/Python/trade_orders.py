#!/usr/bin/env python3
"""
Trade Order Module - Place entry and exit orders to Interactive Brokers.

Exit orders are placed as a target (LMT) + stop (STP) linked by OCA group.
Entry orders delegate to the chase algorithm in order_execution.py.

Usage:
    import sys
    sys.path.insert(0, '/home/fls/Models/Ventura/HD/Scripts/Python')
    from trade_orders import show_live_trades, exit_orders, exit_orders_all

    show_live_trades(account_id=1)
    exit_orders(42, account_id=1)                  # dry run
    exit_orders(42, account_id=1, dry_run=False)   # place orders
"""

from dataclasses import dataclass, field
from datetime import datetime
from typing import Optional, List, Dict
import logging
import pandas as pd

from ib_insync import IB, Contract, Forex, Stock, Future, LimitOrder, StopOrder

import db

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class TradeInfo:
    """All details needed to place orders for a trade, fetched from DB."""
    trade_id: int
    account_id: int
    instrument_id: int
    pair: str
    instrument_type: str        # "FX", "Future", "ETF"
    conid: int                  # IB contract ID (spot instrument_id for FX, active future conid for futures, etf conid for ETFs)
    buy_sell: int               # +1 (bought) or -1 (sold)
    size: float
    price_entry: float          # weighted average entry price
    target_pct: float           # as decimal, e.g. 0.02
    target_price: float
    stop_price: float
    tick_size: float
    strategy_id: int
    date_entry: str
    future_id: Optional[int] = None
    ib_symbol: Optional[str] = None


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def round_to_tick(price: float, tick_size: float) -> float:
    """Round price to nearest tick_size increment."""
    return round(round(price / tick_size) * tick_size, 10)


def _load_instrument_lookup():
    """Load instrument, future, and ETF reference data from local CSVs."""
    instruments = db.loadTableLocal("INSTRUMENTS")
    futures = db.loadTableLocal("future_contract")
    future_active = db.loadTableLocal("future_active")
    future_expiry = db.loadTableLocal("future_expiry")
    etf = db.loadTableLocal("ETF")
    return instruments, futures, future_active, future_expiry, etf


def _resolve_instrument(identifier, instruments, futures, future_active, future_expiry, etf):
    """
    Resolve a book_trade_leg identifier to instrument details.

    For FX: identifier = instrument_id
    For Futures: identifier = conid (from future_expiry)
    For ETFs: identifier = conid (from ETF table)

    Returns dict with: instrument_id, pair, instrument_type, conid, tick_size,
                        future_id, ib_symbol, or None if not found.
    """
    identifier = int(identifier)

    # Try futures first: identifier is a conid in future_expiry
    fut_match = future_expiry[future_expiry["conid"] == identifier]
    if len(fut_match) > 0:
        future_id = int(fut_match.iloc[0]["future_id"])
        fut_detail = futures[futures["future_id"] == future_id]
        if len(fut_detail) > 0:
            instrument_id = int(fut_detail.iloc[0]["instrument_id"])
            inst = instruments[instruments["instrument_id"] == instrument_id]
            # Get active contract conid for current orders
            active = future_active[future_active["future_id"] == future_id]
            active_conid = int(active.iloc[0]["conid"]) if len(active) > 0 else identifier
            return {
                "instrument_id": instrument_id,
                "pair": inst.iloc[0]["pair"] if len(inst) > 0 else f"FUT_{future_id}",
                "instrument_type": "Future",
                "conid": active_conid,
                "tick_size": float(fut_detail.iloc[0]["tick_size"]),
                "future_id": future_id,
                "ib_symbol": str(fut_detail.iloc[0]["ib_symbol"]),
            }

    # Try ETF: identifier is a conid in ETF table
    etf_match = etf[etf["conid"] == identifier]
    if len(etf_match) > 0:
        instrument_id = int(etf_match.iloc[0]["instrument_id"])
        inst = instruments[instruments["instrument_id"] == instrument_id]
        return {
            "instrument_id": instrument_id,
            "pair": inst.iloc[0]["pair"] if len(inst) > 0 else f"ETF_{instrument_id}",
            "instrument_type": "ETF",
            "conid": identifier,
            "tick_size": 0.01,  # ETFs typically trade in cents
            "future_id": None,
            "ib_symbol": None,
        }

    # FX: identifier is instrument_id
    inst = instruments[instruments["instrument_id"] == identifier]
    if len(inst) > 0:
        # For FX, check if there's a future contract for tick_size
        fut_detail = futures[futures["instrument_id"] == identifier]
        tick_size = float(fut_detail.iloc[0]["tick_size"]) if len(fut_detail) > 0 else 0.00005
        return {
            "instrument_id": identifier,
            "pair": inst.iloc[0]["pair"],
            "instrument_type": "FX",
            "conid": identifier,  # for FX, we use the pair to build contract, not conid
            "tick_size": tick_size,
            "future_id": None,
            "ib_symbol": None,
        }

    raise ValueError(f"Could not resolve identifier {identifier} to any instrument")


# ---------------------------------------------------------------------------
# TradeOrderManager
# ---------------------------------------------------------------------------

class TradeOrderManager:
    """
    Place entry and exit orders for Ventura trades via Interactive Brokers.

    Usage:
        mgr = TradeOrderManager(account_id=1)
        mgr.show_live_trades()
        mgr.place_exit_orders(42)                  # dry run
        mgr.place_exit_orders(42, dry_run=False)   # live
    """

    def __init__(self, account_id: int = 1, client_id: int = 20):
        self.account_id = account_id
        self.client_id = client_id
        self.host = "127.0.0.1"
        self.port = 7496 + account_id - 1
        self.ib = IB()
        self._connected = False

    # --- Connection ---

    def connect(self) -> bool:
        """Connect to IB Gateway."""
        try:
            self.ib.connect(self.host, self.port, clientId=self.client_id)
            self._connected = True
            logger.info(f"Connected to IB Gateway at {self.host}:{self.port} (account {self.account_id})")
            return True
        except Exception as e:
            raise ConnectionError(f"Failed to connect to IB Gateway at {self.host}:{self.port}: {e}")

    def disconnect(self) -> None:
        """Disconnect from IB Gateway."""
        if self._connected:
            self.ib.disconnect()
            self._connected = False
            logger.info("Disconnected from IB Gateway")

    # --- Database lookups ---

    def get_trade_info(self, trade_id: int) -> TradeInfo:
        """
        Load all trade details from DB for a given trade_id.
        Aggregates multiple entry legs into weighted average price and total size.
        Filters to self.account_id.
        """
        sql = f"""
            SELECT
                T.trade_id,
                T.strategy_id,
                T.target_pct,
                T.date_entry,
                M.trade_category_id,
                L.account_id,
                L.identifier,
                L.buy_sell,
                L.size,
                L.price
            FROM book_trade T
            JOIN book_trade_map M ON M.trade_id = T.trade_id
            JOIN book_trade_leg L ON L.leg_id = M.leg_id
            WHERE T.trade_id = {trade_id}
            AND T.trade_outcome_id = 0
            AND M.trade_category_id = 1
        """
        dat = db.select(sql)

        if dat is None or len(dat) == 0:
            raise ValueError(f"Trade {trade_id} not found or not live (trade_outcome_id != 0)")

        # Filter to this account
        dat_acct = dat[dat["account_id"] == self.account_id]
        if len(dat_acct) == 0:
            available = dat["account_id"].unique().tolist()
            raise ValueError(
                f"Trade {trade_id} has no entry legs for account {self.account_id}. "
                f"Available accounts: {available}"
            )

        # Aggregate: weighted average price, total size
        total_size = dat_acct["size"].sum()
        wavg_price = (dat_acct["size"] * dat_acct["price"]).sum() / total_size
        buy_sell = int(dat_acct.iloc[0]["buy_sell"])
        strategy_id = int(dat_acct.iloc[0]["strategy_id"])
        target_pct = float(dat_acct.iloc[0]["target_pct"]) / 100.0
        date_entry = str(dat_acct.iloc[0]["date_entry"])
        identifier = int(dat_acct.iloc[0]["identifier"])

        # Resolve instrument details
        instruments, futures, future_active, future_expiry, etf = _load_instrument_lookup()
        info = _resolve_instrument(identifier, instruments, futures, future_active, future_expiry, etf)

        # Calculate target and stop (matches Book.R lines 691-692)
        target_price = wavg_price * (1 + buy_sell * target_pct)
        stop_price = 2 * wavg_price - target_price

        return TradeInfo(
            trade_id=trade_id,
            account_id=self.account_id,
            instrument_id=info["instrument_id"],
            pair=info["pair"],
            instrument_type=info["instrument_type"],
            conid=info["conid"],
            buy_sell=buy_sell,
            size=abs(float(total_size)),
            price_entry=float(wavg_price),
            target_pct=target_pct,
            target_price=float(target_price),
            stop_price=float(stop_price),
            tick_size=info["tick_size"],
            strategy_id=strategy_id,
            date_entry=date_entry,
            future_id=info["future_id"],
            ib_symbol=info["ib_symbol"],
        )

    def get_live_trades(self) -> pd.DataFrame:
        """
        List all live trades for self.account_id with target and stop prices.
        """
        sql = """
            SELECT
                T.trade_id,
                T.strategy_id,
                T.target_pct,
                T.date_entry,
                L.account_id,
                L.identifier,
                L.buy_sell,
                L.size,
                L.price
            FROM book_trade T
            JOIN book_trade_map M ON M.trade_id = T.trade_id
            JOIN book_trade_leg L ON L.leg_id = M.leg_id
            WHERE T.trade_outcome_id = 0
            AND T.date_exit IS NULL
            AND M.trade_category_id = 1
        """
        dat = db.select(sql)
        if dat is None or len(dat) == 0:
            print("No live trades found.")
            return pd.DataFrame()

        # Filter to account
        dat = dat[dat["account_id"] == self.account_id].copy()
        if len(dat) == 0:
            print(f"No live trades for account {self.account_id}.")
            return pd.DataFrame()

        # Resolve instrument details
        instruments, futures, future_active, future_expiry, etf = _load_instrument_lookup()

        rows = []
        for trade_id in dat["trade_id"].unique():
            td = dat[dat["trade_id"] == trade_id]
            total_size = td["size"].sum()
            wavg_price = (td["size"] * td["price"]).sum() / total_size
            buy_sell = int(td.iloc[0]["buy_sell"])
            target_pct = float(td.iloc[0]["target_pct"]) / 100.0
            identifier = int(td.iloc[0]["identifier"])

            try:
                info = _resolve_instrument(identifier, instruments, futures, future_active, future_expiry, etf)
                pair = info["pair"]
                inst_type = info["instrument_type"]
            except ValueError:
                pair = f"?_{identifier}"
                inst_type = "?"

            target_price = wavg_price * (1 + buy_sell * target_pct)
            stop_price = 2 * wavg_price - target_price

            rows.append({
                "trade_id": trade_id,
                "strategy_id": int(td.iloc[0]["strategy_id"]),
                "pair": pair,
                "type": inst_type,
                "direction": "BUY" if buy_sell == 1 else "SELL",
                "size": abs(float(total_size)),
                "price_entry": round(wavg_price, 6),
                "target_price": round(target_price, 6),
                "stop_price": round(stop_price, 6),
                "target_pct": target_pct,
                "date_entry": str(td.iloc[0]["date_entry"]),
            })

        result = pd.DataFrame(rows)
        return result

    # --- Contract building ---

    def build_contract(self, info: TradeInfo) -> Contract:
        """Build and qualify an IB contract from TradeInfo."""
        if info.instrument_type == "FX":
            symbol = info.pair.replace(".", "")
            contract = Forex(symbol)
        elif info.instrument_type == "Future":
            contract = Future(conId=info.conid)
        elif info.instrument_type == "ETF":
            contract = Stock(conId=info.conid)
        else:
            contract = Contract(conId=info.conid)

        qualified = self.ib.qualifyContracts(contract)
        if not qualified:
            raise ValueError(f"Failed to qualify contract for {info.pair} (conid={info.conid})")
        return contract

    # --- Exit orders (target LMT + stop STP as OCA) ---

    def place_exit_orders(self, trade_id: int, dry_run: bool = True) -> Dict:
        """
        Place target (LMT) + stop (STP) as OCA pair for a live trade.

        Args:
            trade_id: The trade to place exit orders for.
            dry_run: If True (default), only print what would be done.

        Returns:
            Dict with order details and status.
        """
        info = self.get_trade_info(trade_id)

        exit_action = "SELL" if info.buy_sell == 1 else "BUY"
        target_rounded = round_to_tick(info.target_price, info.tick_size)
        stop_rounded = round_to_tick(info.stop_price, info.tick_size)
        oca_group = f"ventura_t{trade_id}_{datetime.now().strftime('%Y%m%d%H%M%S')}"

        target_pct_display = abs(info.target_pct) * 100
        entry_str = "BUY" if info.buy_sell == 1 else "SELL"

        # Confirmation output
        print("=" * 68)
        print(f"  EXIT ORDERS for Trade {trade_id}")
        print("=" * 68)
        print(f"  Pair:          {info.pair}")
        print(f"  Type:          {info.instrument_type}")
        print(f"  Account:       {info.account_id}")
        print(f"  Strategy:      {info.strategy_id}")
        print(f"  Entry:         {entry_str} {info.size:,.0f} @ {info.price_entry:.6f}")
        print(f"  Target (LMT):  {exit_action} {info.size:,.0f} @ {target_rounded:.6f}  ({'+' if info.buy_sell == 1 else '-'}{target_pct_display:.2f}%)")
        print(f"  Stop (STP):    {exit_action} {info.size:,.0f} @ {stop_rounded:.6f}  ({'-' if info.buy_sell == 1 else '+'}{target_pct_display:.2f}%)")
        print(f"  OCA Group:     {oca_group}")
        print(f"  Time in Force: GTC")
        print("=" * 68)

        result = {
            "trade_id": trade_id,
            "pair": info.pair,
            "account_id": info.account_id,
            "exit_action": exit_action,
            "size": info.size,
            "target_price": target_rounded,
            "stop_price": stop_rounded,
            "oca_group": oca_group,
            "target_ib_order_id": None,
            "stop_ib_order_id": None,
            "target_status": None,
            "stop_status": None,
            "dry_run": dry_run,
        }

        if dry_run:
            print("  DRY RUN - no orders placed. Call with dry_run=False to execute.")
            print("=" * 68)
            return result

        # Connect if needed
        connected_here = False
        if not self._connected:
            self.connect()
            connected_here = True

        try:
            contract = self.build_contract(info)

            # Target: limit order
            target_order = LimitOrder(
                action=exit_action,
                totalQuantity=info.size,
                lmtPrice=target_rounded,
                tif="GTC",
                ocaGroup=oca_group,
                ocaType=1,
            )

            # Stop order
            stop_order = StopOrder(
                action=exit_action,
                totalQuantity=info.size,
                stopPrice=stop_rounded,
                tif="GTC",
                ocaGroup=oca_group,
                ocaType=1,
            )

            target_trade = self.ib.placeOrder(contract, target_order)
            stop_trade = self.ib.placeOrder(contract, stop_order)

            self.ib.sleep(2)

            result["target_ib_order_id"] = target_trade.order.orderId
            result["stop_ib_order_id"] = stop_trade.order.orderId
            result["target_status"] = target_trade.orderStatus.status
            result["stop_status"] = stop_trade.orderStatus.status

            print(f"  ORDERS PLACED")
            print(f"  Target IB Order ID: {result['target_ib_order_id']}  Status: {result['target_status']}")
            print(f"  Stop IB Order ID:   {result['stop_ib_order_id']}  Status: {result['stop_status']}")
            print("=" * 68)

        finally:
            if connected_here:
                self.disconnect()

        return result

    def place_exit_orders_batch(self, trade_ids: List[int], dry_run: bool = True) -> List[Dict]:
        """Place exit orders for multiple trades. Connects once."""
        results = []

        if not dry_run:
            connected_here = False
            if not self._connected:
                self.connect()
                connected_here = True

        try:
            for tid in trade_ids:
                try:
                    result = self.place_exit_orders(tid, dry_run=dry_run)
                    results.append(result)
                except Exception as e:
                    logger.error(f"Trade {tid}: {e}")
                    results.append({"trade_id": tid, "error": str(e)})
        finally:
            if not dry_run and connected_here:
                self.disconnect()

        return results

    # --- Entry orders (delegate to chase algo) ---

    def place_entry_order(
        self,
        instrument_id: int,
        direction: int,
        size: float,
        price: float,
        price_limit: float,
        dry_run: bool = True,
    ) -> Dict:
        """
        Place entry order using chase strategy from order_execution.py.

        Args:
            instrument_id: Your DB instrument_id
            direction: +1 for buy, -1 for sell
            size: Order size
            price: Starting price (favorable)
            price_limit: Worst acceptable price
            dry_run: If True (default), only print what would be done.
        """
        from order_execution import Order as ExecOrder, OrderExecutor, AssetClass

        instruments, futures, future_active, future_expiry, etf = _load_instrument_lookup()
        inst = instruments[instruments["instrument_id"] == instrument_id]
        if len(inst) == 0:
            raise ValueError(f"Instrument {instrument_id} not found")

        pair = inst.iloc[0]["pair"]

        # Determine asset class, conid, tick_size
        fut_detail = futures[futures["instrument_id"] == instrument_id]
        etf_match = etf[etf["instrument_id"] == instrument_id]

        if len(fut_detail) > 0:
            asset_class = AssetClass.FUTURE
            active = future_active[future_active["future_id"] == int(fut_detail.iloc[0]["future_id"])]
            conid = int(active.iloc[0]["conid"]) if len(active) > 0 else 0
            tick_size = float(fut_detail.iloc[0]["tick_size"])
        elif len(etf_match) > 0:
            asset_class = AssetClass.STOCK
            conid = int(etf_match.iloc[0]["conid"])
            tick_size = 0.01
        else:
            asset_class = AssetClass.FX
            conid = instrument_id
            tick_size = float(fut_detail.iloc[0]["tick_size"]) if len(fut_detail) > 0 else 0.00005

        action = "BUY" if direction == 1 else "SELL"

        print("=" * 68)
        print(f"  ENTRY ORDER")
        print("=" * 68)
        print(f"  Pair:          {pair}")
        print(f"  Action:        {action} {size:,.0f}")
        print(f"  Price:         {price:.6f} (start)")
        print(f"  Price Limit:   {price_limit:.6f} (worst)")
        print(f"  Tick Size:     {tick_size}")
        print(f"  Account:       {self.account_id}")
        print("=" * 68)

        order = ExecOrder(
            order_id=0,
            instrument_id=instrument_id,
            conid=conid,
            direction=direction,
            size=size,
            price=price,
            price_limit=price_limit,
            tick_size=tick_size,
            asset_class=asset_class,
            symbol=pair,
        )

        if dry_run:
            print("  DRY RUN - no order placed. Call with dry_run=False to execute.")
            print("=" * 68)
            return {"order": order, "dry_run": True}

        executor = OrderExecutor(
            host=self.host,
            port=self.port,
            client_id=self.client_id,
        )
        if not executor.connect():
            raise ConnectionError("Failed to connect to IB for entry order")

        try:
            result = executor.execute(order)
        finally:
            executor.disconnect()

        print(f"  Status: {result.status.value}, Filled: {result.size_filled} @ {result.avg_fill_price:.6f}")
        print("=" * 68)
        return {"order": result, "dry_run": False}

    # --- Cancel exit orders ---

    def cancel_exit_orders(self, trade_id: int) -> int:
        """
        Cancel all open OCA exit orders for a trade.
        Returns the number of orders cancelled.
        """
        if not self._connected:
            self.connect()

        oca_prefix = f"ventura_t{trade_id}_"
        cancelled = 0

        for trade in self.ib.openTrades():
            if trade.order.ocaGroup and trade.order.ocaGroup.startswith(oca_prefix):
                self.ib.cancelOrder(trade.order)
                cancelled += 1
                logger.info(f"Cancelled order {trade.order.orderId} (OCA: {trade.order.ocaGroup})")

        if cancelled > 0:
            self.ib.sleep(1)
        else:
            print(f"No open OCA orders found for trade {trade_id}")

        return cancelled


# ---------------------------------------------------------------------------
# Module-level convenience functions
# ---------------------------------------------------------------------------

def show_live_trades(account_id: int = 1) -> pd.DataFrame:
    """Display all live trades with target/stop prices."""
    mgr = TradeOrderManager(account_id=account_id)
    trades = mgr.get_live_trades()
    if len(trades) > 0:
        print(trades.to_string(index=False))
    return trades


def exit_orders(trade_id: int, account_id: int = 1, dry_run: bool = True) -> Dict:
    """Place exit orders (target LMT + stop STP as OCA) for a single trade."""
    mgr = TradeOrderManager(account_id=account_id)
    return mgr.place_exit_orders(trade_id, dry_run=dry_run)


def exit_orders_all(account_id: int = 1, dry_run: bool = True) -> List[Dict]:
    """Place exit orders for ALL live trades on this account."""
    mgr = TradeOrderManager(account_id=account_id)
    trades = mgr.get_live_trades()
    if len(trades) == 0:
        return []
    trade_ids = trades["trade_id"].unique().tolist()
    print(f"Placing exit orders for {len(trade_ids)} trades: {trade_ids}")
    print()
    return mgr.place_exit_orders_batch(trade_ids, dry_run=dry_run)
