"""IB Gateway connection manager using ib_insync.

Centralizes connection setup, port/client_id lookup, and lifecycle
management that was previously duplicated across order_execution.py,
trade_orders.py, and execution_utils.py.
"""

from __future__ import annotations

import logging

from ib_insync import IB

from ventura.db import Database

logger = logging.getLogger(__name__)


class IBConnection:
    """Managed connection to IB Gateway / TWS via ib_insync.

    Usage::

        conn = IBConnection(port=7497, client_id=10)
        conn.connect()
        try:
            conn.ib.qualifyContracts(contract)
        finally:
            conn.disconnect()

    Or as a context manager::

        with IBConnection(port=7497, client_id=10) as conn:
            conn.ib.qualifyContracts(contract)
    """

    def __init__(
        self,
        host: str = "127.0.0.1",
        port: int = 7497,
        client_id: int = 1,
        timeout: float = 10.0,
        readonly: bool = True,
    ) -> None:
        self.host = host
        self.port = port
        self.client_id = client_id
        self.timeout = timeout
        self.readonly = readonly
        self.ib = IB()
        self._connected = False

    # -- Lifecycle ----------------------------------------------------------

    def connect(self) -> None:
        """Connect to IB Gateway/TWS."""
        self.ib.connect(
            self.host,
            self.port,
            clientId=self.client_id,
            timeout=self.timeout,
            readonly=self.readonly,
        )
        self._connected = True
        logger.info(
            "Connected to IB at %s:%s (client_id=%s, readonly=%s)",
            self.host, self.port, self.client_id, self.readonly,
        )

    def disconnect(self) -> None:
        """Disconnect from IB Gateway/TWS."""
        if self._connected:
            self.ib.disconnect()
            self._connected = False
            logger.info("Disconnected from IB")

    @property
    def connected(self) -> bool:
        return self._connected and self.ib.isConnected()

    # -- Context manager ----------------------------------------------------

    def __enter__(self) -> IBConnection:
        self.connect()
        return self

    def __exit__(self, *exc) -> None:
        self.disconnect()

    # -- Port / client_id lookup -------------------------------------------

    @staticmethod
    def get_port(account_id: int, this_computer: str) -> int:
        """Return the IB Gateway port for a given account.

        Production (machine I): 7496 + (account_id - 1)
        Dev (machine H):        7498 + (account_id - 1)
        """
        base = 7498 if this_computer == "H" else 7496
        return base + account_id - 1

    @staticmethod
    def get_client_id(script_name: str, db: Database, data_dir: str) -> int:
        """Look up the ``ib_client_id`` for *script_name* from the script table."""
        scripts = db.load_table_local("script", data_dir)
        row = scripts.loc[scripts["script"] == script_name, "ib_client_id"]
        if len(row) == 0:
            raise ValueError(f"Script '{script_name}' not found in script table")
        return int(row.values[0])
