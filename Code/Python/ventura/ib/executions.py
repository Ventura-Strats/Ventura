"""IB Execution reader using ib_insync.

Replaces the old IBAppExec class (ibapi callbacks) with synchronous
ib_insync calls.  Reads today's fills from IB, matches them to Ventura
instruments, saves to CSV and inserts new legs into ``book_trade_leg``.
"""

from __future__ import annotations

import logging
from datetime import datetime
from typing import List, Optional

import pandas as pd
import pytz

from ventura.config import VenturaConfig
from ventura.db import Database
from ventura.ib.connection import IBConnection
from ventura.utils import print_banner, convert_tz

logger = logging.getLogger(__name__)

# Timezone used for stored timestamps
TZ_LOCAL = "Europe/London"


class ExecutionReader:
    """Read and persist IB execution fills.

    Usage::

        reader = ExecutionReader(db=db, cfg=cfg, start_time=start_time)
        for account_id in [1, 2]:
            with IBConnection(port=get_port(account_id), ...) as conn:
                reader.read_executions(account_id, conn)

        reader.save_to_files()
        reader.save_to_db()
    """

    def __init__(
        self,
        db: Database,
        cfg: VenturaConfig,
        start_time: datetime,
    ) -> None:
        self.db = db
        self.cfg = cfg
        self.start_time = start_time

        # Reference data for identifier resolution
        self._instruments = cfg.tables.instruments
        self._future_expiry = db.load_table_local("future_expiry", cfg.data_dir)
        etf_proxy = db.load_table_local("etf_proxy", cfg.data_dir)[["instrument_id", "conid"]]
        etf_direct = db.select(
            "SELECT instrument_id, value AS conid "
            "FROM static_instrument_attribute_int "
            "WHERE attribute_id = 7"
        )
        self._etf = pd.concat([etf_proxy, etf_direct], ignore_index=True)

        # Output paths
        self._path = cfg.data_dir + "Executions_IBAPI/"
        ts_tag = start_time.strftime("%Y%m%d_%H%M%S")
        self._file_dated = f"executions_{ts_tag}.csv"
        self._file_last = "executions_last.csv"

        # Accumulated results
        self._results: List[pd.DataFrame] = []

    # -- Main entry point per account --------------------------------------

    def read_executions(self, account_id: int, conn: IBConnection) -> pd.DataFrame:
        """Fetch executions for one account and accumulate.

        Each account requires its own IBConnection (different port per account).
        """
        print_banner(f"Reading executions for account {account_id}")

        fills = conn.ib.fills()
        if not fills:
            logger.info("No fills returned for account %s", account_id)
            return pd.DataFrame()

        rows = []
        for fill in fills:
            contract = fill.contract
            execution = fill.execution

            # Only process FX, futures, stocks
            if contract.secType not in ("CASH", "FUT", "STK"):
                continue

            identifier = self._find_identifier(contract)
            buy_sell = 1 if execution.side == "BOT" else (-1 if execution.side == "SLD" else None)
            timestamp = self._parse_execution_time(execution.time)

            rows.append({
                "account_id": account_id,
                "ib_trade_id": execution.permId,
                "ib_exec_id": execution.execId,
                "ib_order_id": execution.orderId,
                "identifier": identifier,
                "timestamp": timestamp,
                "conid": contract.conId,
                "symbol": contract.symbol,
                "sec_type": contract.secType,
                "ccy": contract.currency,
                "buy_sell": buy_sell,
                "size": float(execution.shares),
                "price": float(execution.price),
            })

        df = pd.DataFrame(rows)
        if not df.empty:
            df = self._dedup(df)
        self._results.append(df)

        logger.info("Got %d executions for account %s", len(df), account_id)
        return df

    def all_executions(self) -> pd.DataFrame:
        """Return all accumulated executions, sorted."""
        if not self._results:
            return pd.DataFrame()
        df = pd.concat(self._results, ignore_index=True)
        if not df.empty:
            df = df.sort_values(["identifier", "account_id"]).reset_index(drop=True)
        return df

    # -- Identifier resolution ---------------------------------------------

    def _find_identifier(self, contract) -> Optional[int]:
        """Map an IB contract to a Ventura identifier.

        FX:      instrument_id matched by pair (e.g. EURUSD)
        Future:  conid if it exists in future_expiry table
        Stock:   conid if it exists in ETF table
        """
        if contract.secType == "CASH":
            return self._match_fx_pair(contract)
        elif contract.secType == "FUT":
            return self._match_future_conid(contract)
        elif contract.secType == "STK":
            return self._match_stock_conid(contract)
        return None

    def _match_fx_pair(self, contract) -> Optional[int]:
        pair = contract.symbol + contract.currency
        matches = self._instruments[self._instruments["pair"] == pair]
        if len(matches) > 0:
            return int(matches["instrument_id"].values[0])
        return None

    def _match_future_conid(self, contract) -> Optional[int]:
        if contract.conId in self._future_expiry["conid"].values:
            return int(contract.conId)
        return None

    def _match_stock_conid(self, contract) -> Optional[int]:
        if contract.conId in self._etf["conid"].values:
            return int(contract.conId)
        return None

    # -- Timestamp parsing -------------------------------------------------

    @staticmethod
    def _parse_execution_time(time_val) -> datetime:
        """Convert IB execution time to naive local datetime.

        ib_insync provides ``execution.time`` as a timezone-aware
        ``datetime`` object (UTC).  Convert to local TZ and strip tzinfo.

        Also handles the legacy string format ``"20260106  06:15:51 Europe/London"``
        just in case.
        """
        if isinstance(time_val, datetime):
            # ib_insync gives us a tz-aware datetime (typically UTC)
            if time_val.tzinfo is None:
                dt = pytz.timezone(TZ_LOCAL).localize(time_val)
            else:
                dt = time_val.astimezone(pytz.timezone(TZ_LOCAL))
            return dt.replace(tzinfo=None)

        # Legacy string format fallback
        time_str = str(time_val)
        parts = time_str.rsplit(" ", 1)
        if len(parts) == 2 and "/" in parts[1]:
            datetime_str = parts[0].strip()
            tz_str = parts[1]
        else:
            datetime_str = time_str.strip()
            tz_str = "Asia/Hong_Kong"

        dt = datetime.strptime(datetime_str, "%Y%m%d  %H:%M:%S")
        return convert_tz(dt, tz_str, TZ_LOCAL).replace(tzinfo=None)

    # -- Deduplication -----------------------------------------------------

    @staticmethod
    def _dedup(df: pd.DataFrame) -> pd.DataFrame:
        """Keep only the last row per ib_exec_id (mirrors old logic)."""
        if df.empty:
            return df
        return (
            df
            .sort_values(["ib_exec_id", "ib_trade_id", "timestamp"])
            .groupby("ib_exec_id")
            .tail(1)
            .drop_duplicates()
            .sort_values("ib_exec_id")
            .reset_index(drop=True)
        )

    # -- Persistence -------------------------------------------------------

    def save_to_files(self) -> None:
        """Save all executions to CSV (timestamped + _last)."""
        df = self.all_executions()
        if df.empty:
            logger.info("No executions to save to files")
            return
        df.to_csv(self._path + self._file_dated, index=False)
        df.to_csv(self._path + self._file_last, index=False)
        logger.info("Saved executions CSV: %s", self._file_dated)

    def save_to_db(self) -> None:
        """Insert new execution legs into book_trade_leg.

        Skips legs whose ``ib_exec_id`` already exists in the DB
        (from yesterday onward).
        """
        df = self.all_executions()
        if df.empty:
            logger.info("No executions to save to DB")
            return

        # Find which exec_ids are already in DB
        yesterday_str = self.cfg.yesterday.strftime("%Y-%m-%d %H:%M:%S")
        existing = self.db.select(
            f"SELECT DISTINCT ib_exec_id FROM book_trade_leg "
            f"WHERE timestamp >= '{yesterday_str}'"
        )
        new_ids = set(df["ib_exec_id"]) - set(existing["ib_exec_id"])
        df = df[df["ib_exec_id"].isin(new_ids)]

        if df.empty:
            logger.info("All executions already in DB — nothing new to insert")
            return

        # Drop rows with no identifier (unmatched instruments)
        df = df.dropna(subset=["identifier"])
        if df.empty:
            logger.info("No matched executions to insert")
            return

        # Assign leg_ids
        max_leg_id = int(
            self.db.select("SELECT MAX(leg_id) AS leg_id FROM book_trade_leg")
            ["leg_id"][0]
        )
        df = df.assign(
            leg_id=range(max_leg_id + 1, max_leg_id + 1 + len(df)),
            fees=0,
            fees_ccy_id=1,
            tax=0,
        )[
            [
                "leg_id", "ib_trade_id", "ib_exec_id", "account_id",
                "identifier", "timestamp", "buy_sell", "size", "price",
                "fees", "fees_ccy_id", "tax",
            ]
        ]

        self.db.append("book_trade_leg", df)
        logger.info("Inserted %d new execution legs into book_trade_leg", len(df))
