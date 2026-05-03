"""IB contract resolution using ib_insync.

Replaces the old ibapi callback classes (IBContracts, IBAppRetrieveContracts)
from Signal_List.py with simple synchronous ib_insync calls.

Also consolidates the _build_contract() helpers from order_execution.py
and trade_orders.py.
"""

from __future__ import annotations

import logging
from typing import Sequence

from ib_insync import IB, Contract, Forex, Stock, Future

logger = logging.getLogger(__name__)


class ContractResolver:
    """Qualify and resolve IB contracts via ib_insync.

    Requires an active ``IB`` connection (pass the ``.ib`` attribute
    from an ``IBConnection``).

    Usage::

        with IBConnection(port=7497, client_id=5) as conn:
            resolver = ContractResolver(conn.ib)

            # Qualify a single FX contract
            contract = resolver.qualify_fx("EUR", "USD")

            # Qualify by conid (futures, ETFs, anything)
            contract = resolver.qualify_conid(12087792)

            # Batch: get conids for FX pairs
            conids = resolver.fx_pairs_to_conids(["EURUSD", "GBPUSD"])

            # Batch: qualify multiple conids at once
            contracts = resolver.qualify_conids([12087792, 38709539])
    """

    def __init__(self, ib: IB) -> None:
        self.ib = ib

    # -- Single contract helpers -------------------------------------------

    def qualify_fx(self, symbol: str, currency: str) -> Contract:
        """Build and qualify an FX (CASH) contract.

        Args:
            symbol: Base currency, e.g. ``"EUR"``
            currency: Quote currency, e.g. ``"USD"``

        Returns:
            Qualified ``Forex`` contract.
        """
        contract = Forex(pair=f"{symbol}{currency}")
        qualified = self.ib.qualifyContracts(contract)
        if not qualified:
            raise ValueError(f"Failed to qualify FX contract {symbol}.{currency}")
        return qualified[0]

    def qualify_fx_pair(self, pair: str) -> Contract:
        """Qualify from a 6-char pair string like ``"EURUSD"`` or ``"EUR.USD"``."""
        pair = pair.replace(".", "")
        return self.qualify_fx(pair[:3], pair[3:])

    def qualify_conid(self, conid: int) -> Contract:
        """Qualify any contract by its IB conid."""
        contract = Contract(conId=conid)
        qualified = self.ib.qualifyContracts(contract)
        if not qualified:
            raise ValueError(f"Failed to qualify conid {conid}")
        return qualified[0]

    def qualify_stock(self, conid: int) -> Contract:
        """Qualify a stock/ETF by conid."""
        contract = Stock(conId=conid)
        qualified = self.ib.qualifyContracts(contract)
        if not qualified:
            raise ValueError(f"Failed to qualify stock conid {conid}")
        return qualified[0]

    def qualify_future(self, conid: int) -> Contract:
        """Qualify a futures contract by conid."""
        contract = Future(conId=conid)
        qualified = self.ib.qualifyContracts(contract)
        if not qualified:
            raise ValueError(f"Failed to qualify future conid {conid}")
        return qualified[0]

    def build_contract(
        self,
        instrument_type: str,
        conid: int,
        pair: str = "",
    ) -> Contract:
        """Build and qualify a contract by type.

        This consolidates the ``_build_contract`` logic that was duplicated
        in ``order_execution.py`` and ``trade_orders.py``.

        Args:
            instrument_type: One of ``"FX"``, ``"Future"``, ``"ETF"``.
            conid: IB contract ID.
            pair: FX pair string like ``"EUR.USD"`` (only needed for FX).
        """
        if instrument_type == "FX":
            return self.qualify_fx_pair(pair)
        elif instrument_type == "Future":
            return self.qualify_future(conid)
        elif instrument_type == "ETF":
            return self.qualify_stock(conid)
        else:
            return self.qualify_conid(conid)

    # -- Batch operations --------------------------------------------------

    def fx_pairs_to_conids(self, pairs: Sequence[str]) -> list[int | None]:
        """Resolve a list of FX pair strings to conids.

        Replaces the old ``IBContracts`` class (80+ lines of ibapi callbacks).

        Args:
            pairs: e.g. ``["EURUSD", "GBPUSD", None]``.
                   ``None`` entries pass through as ``None``.

        Returns:
            List of conids (or ``None`` for unresolvable entries).
        """
        results: list[int | None] = []
        for pair in pairs:
            if pair is None:
                results.append(None)
                continue
            try:
                contract = self.qualify_fx_pair(pair)
                results.append(contract.conId)
            except Exception as exc:
                logger.warning("Could not resolve FX pair %s: %s", pair, exc)
                results.append(None)
        return results

    def qualify_conids(self, conids: Sequence[int | None]) -> list[Contract | None]:
        """Qualify multiple contracts by conid.

        Replaces the old ``IBAppRetrieveContracts`` class (50+ lines of
        ibapi callbacks).

        Args:
            conids: List of IB conids. ``None`` entries pass through as ``None``.

        Returns:
            List of qualified ``Contract`` objects (or ``None``).
        """
        results: list[Contract | None] = []
        for conid in conids:
            if conid is None:
                results.append(None)
                continue
            try:
                contract = self.qualify_conid(conid)
                results.append(contract)
            except Exception as exc:
                logger.warning("Could not qualify conid %s: %s", conid, exc)
                results.append(None)
        return results
