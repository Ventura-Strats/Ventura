"""Combine new + exit orders, net positions, add tick sizes and contracts.

Extracts the ``PrepareCombinedOrdersList`` class from old Signal_List.py.
Uses ib_insync (via ventura.ib.contracts) instead of old ibapi callbacks.
"""

from __future__ import annotations

import logging

import numpy as np
import pandas as pd

from ventura.config import VenturaConfig
from ventura.db import Database
from ventura.ib.connection import IBConnection
from ventura.ib.contracts import ContractResolver
from ventura.signals.order_list import read_order_list, save_order_list
from ventura.utils import print_banner, force_column_type, sign

logger = logging.getLogger(__name__)

COL_COMBINED_ODA = [
    "order_id", "ib_order_id", "account_id", "instrument_id", "ticker",
    "future_id", "conid", "contract", "tick_size",
    "buy_sell", "buy_sell_action", "size_to_do", "px_order", "px_live", "px_avg",
    "initial_position", "position", "filled", "remaining", "status", "ib_status",
]


class CombinedOrdersBuilder:
    """Combine new and exit orders into a single execution-ready list.

    Nets positions by conid, adds tick sizes, resolves IB contracts,
    and saves the combined CSV.

    Ported from ``PrepareCombinedOrdersList`` in old Signal_List.py.
    """

    def __init__(
        self,
        db: Database,
        cfg: VenturaConfig,
        conn: IBConnection,
        execution_time_id: int,
        account_id: int,
    ) -> None:
        self.db = db
        self.cfg = cfg
        self.conn = conn
        self.execution_time_id = execution_time_id
        self.account_id = account_id
        self.orders_dir = cfg.data_dir + "Orders/"

        # Reference tables for tick sizes
        self._futures = db.load_table_local("future_contract", cfg.data_dir)
        self._futures_expiry = db.load_table_local("future_expiry", cfg.data_dir)
        self._tick_size_fx = db.load_table_local("instrument_attribute_dbl", cfg.data_dir)
        self._attributes = db.load_table_local("instrument_attribute_type", cfg.data_dir)
        self._etf = db.load_table_local("ETF", cfg.data_dir)

        self._tick_attrid = int(
            self._attributes.loc[
                self._attributes["attribute"] == "tick_size", "attribute_id"
            ].values[0]
        )

        self.int_cols = ["account_id", "strategy_id", "instrument_id",
                         "future_id", "conid", "buy_sell"]
        self.float_cols = ["size_to_do", "px_order"]

        self.orders = pd.DataFrame()
        self.tick_size = pd.DataFrame(columns=["conid", "tick_size"])

    def run(self) -> pd.DataFrame:
        """Execute the full pipeline and return combined orders."""
        self._read_new_orders()
        self._read_exit_orders()
        self._combine()
        self._net_by_instrument()
        self._add_buy_sell_action()
        self._add_tick_size()
        self._get_contract_details()
        self._remove_no_contract()
        self._prepare_execution_columns()
        self._save()
        return self.orders

    # -- Read order files --------------------------------------------------

    def _fmt(self, df: pd.DataFrame) -> pd.DataFrame:
        df = force_column_type(df, self.int_cols, int)
        df = force_column_type(df, self.float_cols, float)
        return df

    def _read_new_orders(self) -> None:
        try:
            df = read_order_list(
                self.account_id, "New",
                self.cfg, self.execution_time_id, self.orders_dir,
            )
            self.orders_new = self._fmt(df)
        except Exception:
            self.orders_new = pd.DataFrame()

    def _read_exit_orders(self) -> None:
        try:
            df = read_order_list(
                self.account_id, "Exit",
                self.cfg, self.execution_time_id, self.orders_dir,
            )
            if "trade_id" in df.columns:
                df = df.drop(columns="trade_id")
            self.orders_exit = self._fmt(df)
        except Exception:
            self.orders_exit = pd.DataFrame()

    def _combine(self) -> None:
        self.orders = pd.concat(
            [self.orders_new, self.orders_exit], ignore_index=True,
        )
        self.orders = self._fmt(self.orders)

    # -- Net positions by conid --------------------------------------------

    def _net_by_instrument(self) -> None:
        if self.orders.empty:
            return

        # Summarize positions
        df_pos = self.orders.copy()
        df_pos["size_to_do"] = df_pos["size_to_do"] * df_pos["buy_sell"]
        df_pos = df_pos.groupby("conid", as_index=False).agg({"size_to_do": "sum"})
        df_pos["buy_sell"] = df_pos["size_to_do"].map(sign)
        df_pos["size_to_do"] = df_pos["size_to_do"].abs()
        df_pos = force_column_type(df_pos, ["conid"], int)

        # Summarize prices
        df_px = (
            self.orders[["conid", "px_order"]]
            .dropna()
            .groupby("conid", as_index=False)
            .agg({"px_order": "mean"})
        )

        # Instrument info
        df_inst = (
            self.orders[["account_id", "instrument_id", "future_id", "ticker", "conid"]]
            .drop_duplicates()
        )
        df_inst = force_column_type(df_inst, ["conid"], int)

        self.orders = (
            df_pos[df_pos["size_to_do"] > 0]
            .merge(df_px, on="conid", how="left")
            .merge(df_inst, on="conid", how="left")
            [["account_id", "instrument_id", "future_id", "ticker",
              "asset_class", "conid", "buy_sell", "size_to_do", "px_order"]]
        )

    # -- Buy/sell action ---------------------------------------------------

    def _add_buy_sell_action(self) -> None:
        if self.orders.empty:
            return
        df = self.orders.copy()
        df["buy_sell_action"] = "NOTHING"
        df.loc[df["buy_sell"] == 1, "buy_sell_action"] = "BUY"
        df.loc[df["buy_sell"] == -1, "buy_sell_action"] = "SELL"
        self.orders = df[df["buy_sell_action"].isin({"BUY", "SELL"})]

    # -- Tick sizes --------------------------------------------------------

    def _build_tick_size_map(self) -> None:
        instruments = self.cfg.tables.instruments

        # Futures
        df_fut = (
            self._futures_expiry
            .merge(self._futures, on="future_id", how="left")
            [["conid", "tick_size"]]
        )
        parts = [df_fut]

        # FX + direct ETF
        df_fx = (
            self._tick_size_fx[self._tick_size_fx["attribute_id"] == self._tick_attrid]
            .merge(instruments, on="instrument_id", how="left")
            .rename(columns={"conid_spot": "conid", "value": "tick_size"})
            [["conid", "tick_size"]]
            .dropna()
        )
        parts.append(df_fx)

        # ETF proxy
        df_etf = self._etf[["conid", "tick_size"]].dropna()
        parts.append(df_etf)

        self.tick_size = pd.concat(parts, ignore_index=True)

    def _add_tick_size(self) -> None:
        if self.orders.empty:
            return
        self._build_tick_size_map()
        self.orders = self.orders.merge(self.tick_size, on="conid", how="left")
        self.orders = self.orders.dropna(subset=["tick_size"])

    # -- IB contract details (ib_insync) -----------------------------------

    def _get_contract_details(self) -> None:
        """Qualify contracts via ib_insync (replaces old IBAppRetrieveContracts)."""
        if self.orders.empty:
            return

        conid_list = list(self.orders["conid"].astype(int))
        resolver = ContractResolver(self.conn.ib)
        contracts = resolver.qualify_conids(conid_list)

        df_contracts = pd.DataFrame({
            "conid": conid_list,
            "contract": contracts,
        })
        self.orders = self.orders.merge(df_contracts, on="conid", how="left")

    def _remove_no_contract(self) -> None:
        if not self.orders.empty:
            self.orders = self.orders.dropna(subset=["contract"])

    # -- Execution columns -------------------------------------------------

    def _prepare_execution_columns(self) -> None:
        if self.orders.empty:
            return

        self.orders["initial_position"] = 0
        self.orders["position"] = 0
        self.orders["filled"] = 0
        self.orders["remaining"] = self.orders["size_to_do"]
        self.orders["px_live"] = np.nan
        self.orders["px_avg"] = np.nan
        self.orders["ib_order_id"] = np.nan
        self.orders["order_id"] = range(1, 1 + len(self.orders))
        self.orders["status"] = "not yet started"
        self.orders["ib_status"] = ""

        self.orders = self._fmt(self.orders)

        # Ensure column exists even if not in data (for empty orders case)
        for col in COL_COMBINED_ODA:
            if col not in self.orders.columns:
                self.orders[col] = np.nan

        self.orders = self.orders[COL_COMBINED_ODA]

    # -- Save --------------------------------------------------------------

    def _save(self) -> None:
        save_order_list(
            self.account_id, self.orders, "Combined",
            self.cfg, self.execution_time_id, self.orders_dir,
        )
