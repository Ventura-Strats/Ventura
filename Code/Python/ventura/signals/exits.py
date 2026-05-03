"""Prepare exit orders for open trades.

Extracts the ``PrepareExitOrdersList`` class from old Signal_List.py.
"""

from __future__ import annotations

import logging
from datetime import timedelta

import pandas as pd

from ventura.config import VenturaConfig
from ventura.db import Database
from ventura.signals.order_list import save_order_list
from ventura.utils import print_banner, force_column_type

logger = logging.getLogger(__name__)

COL_EXIT_ODA = [
    "trade_id", "account_id", "strategy_id", "instrument_id", "ticker",
    "future_id", "conid", "buy_sell", "size_to_do", "px_order",
]


class ExitOrdersBuilder:
    """Build exit orders for open trades due for closing today.

    Ported from ``PrepareExitOrdersList`` in old Signal_List.py.
    """

    def __init__(
        self,
        db: Database,
        cfg: VenturaConfig,
        execution_time_id: int,
        account_id: int,
    ) -> None:
        self.db = db
        self.cfg = cfg
        self.execution_time_id = execution_time_id
        self.account_id = account_id
        self.orders_dir = cfg.data_dir + "Orders/"
        self.orders_exit = pd.DataFrame(columns=COL_EXIT_ODA)

    def run(self) -> pd.DataFrame:
        """Execute the full pipeline and return exit orders."""
        self._load_from_db()
        if self.orders_exit.empty:
            logger.info("No exit trades found for account %s", self.account_id)
            self._save()
            return self.orders_exit

        self._keep_for_execution_time()
        self._fill_conid_for_fx_spot()
        self._format()
        self._save()
        return self.orders_exit

    # -- Load open trades from DB ------------------------------------------

    def _load_from_db(self) -> None:
        """Load trades due for exit today from book_trade + book_trade_leg."""
        # date_entry: 7 days ago (rough; to be replaced with proper calendars)
        date_entry = (self.cfg.today - timedelta(days=7)).strftime("%Y-%m-%d")

        sql = f"""
            SELECT T.trade_id, L.account_id, T.strategy_id,
                IFNULL(I.instrument_id, C.instrument_id) AS instrument_id,
                IFNULL(I.ticker, I2.ticker) AS ticker,
                X.conid, X.future_id, T.date_entry,
                -AVG(L.buy_sell) AS buy_sell,
                SUM(L.size) AS size_to_do,
                IFNULL(AVG(F.price), AVG(P.close)) AS px_order
            FROM book_trade T
            LEFT JOIN book_trade_map M ON M.trade_id = T.trade_id
            LEFT JOIN book_trade_leg L ON L.leg_id = M.leg_id
            LEFT JOIN live_px P ON P.instrument_id = L.identifier
            LEFT JOIN static_instrument I ON I.instrument_id = L.identifier
            LEFT JOIN static_future_expiry X ON X.conid = L.identifier
            LEFT JOIN static_future_contract C ON C.future_id = X.future_id
            LEFT JOIN static_instrument I2 ON I2.instrument_id = C.instrument_id
            LEFT JOIN (
                SELECT A1.conid, A1.price
                FROM live_px_future A1
                RIGHT JOIN (
                    SELECT conid, MAX(timestamp) AS timestamp
                    FROM live_px_future GROUP BY conid
                ) A2 ON A1.conid = A2.conid AND A1.timestamp = A2.timestamp
            ) F ON F.conid = L.identifier
            WHERE T.strategy_id != 0
              AND T.trade_outcome_id = 0
              AND T.date_exit IS NULL
              AND T.date_entry = '{date_entry}'
              AND M.trade_category_id = 1
              AND L.account_id = {self.account_id}
            GROUP BY T.trade_id, T.strategy_id, L.identifier
        """
        self.orders_exit = self.db.select(sql)

    # -- Filters -----------------------------------------------------------

    def _keep_for_execution_time(self) -> None:
        """Keep only instruments that match this execution_time_id."""
        instruments = self.cfg.tables.instruments
        inst_keep = instruments.loc[
            instruments["execution_time_id"] == self.execution_time_id,
            "instrument_id",
        ]
        self.orders_exit = self.orders_exit[
            self.orders_exit["instrument_id"].isin(inst_keep)
        ]

    def _fill_conid_for_fx_spot(self) -> None:
        """Fill in conid for FX/spot instruments from conid_spot."""
        instruments = self.cfg.tables.instruments
        df = self.orders_exit.merge(
            instruments[["instrument_id", "asset_class", "conid_spot"]],
            on="instrument_id", how="left",
        )
        missing = df["conid"].isna()
        df.loc[missing, "conid"] = df.loc[missing, "conid_spot"]
        df = df.drop(columns="conid_spot")
        self.orders_exit = df

    # -- Format + save -----------------------------------------------------

    def _format(self) -> None:
        self.orders_exit = self.orders_exit[COL_EXIT_ODA]
        int_cols = ["trade_id", "account_id", "strategy_id", "instrument_id",
                    "future_id", "conid", "buy_sell"]
        float_cols = ["size_to_do", "px_order"]
        self.orders_exit = force_column_type(self.orders_exit, int_cols, int)
        self.orders_exit = force_column_type(self.orders_exit, float_cols, float)

    def _save(self) -> None:
        print_banner(f"Exit orders for account {self.account_id}:", False)
        save_order_list(
            self.account_id, self.orders_exit, "Exit",
            self.cfg, self.execution_time_id, self.orders_dir,
        )
