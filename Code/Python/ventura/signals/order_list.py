"""Prepare new entry orders from signals.

Extracts the ``PrepareNewOrdersList`` class and helper functions
from old Signal_List.py.  Handles spot, future, ETF direct, and
ETF proxy order preparation.
"""

from __future__ import annotations

import logging
from datetime import timedelta

import numpy as np
import pandas as pd

from ventura.config import VenturaConfig
from ventura.db import Database
from ventura.signals.predictions import get_model_predictions
from ventura.utils import print_banner, force_column_type, sign, round_to_multiple, ensure_dir

logger = logging.getLogger(__name__)

MAX_NB_TRADES = 10
MAX_LEVERAGE_PER_TRADE = 4

COLNAMES_ODA_NEW = [
    "trade_id", "asset_class", "date_trade", "strategy_id", "ticker",
    "instrument_id", "future_id", "expiry", "conid",
    "predict", "buy_sell", "price_entry",
    "notional_for_1k_pnl", "timestamp_px", "timestamp_signal",
]

EMPTY_SIGNAL_NEW = pd.DataFrame(columns=COLNAMES_ODA_NEW)


# ---------------------------------------------------------------------------
# Helpers (shared with exits / combined)
# ---------------------------------------------------------------------------

def add_back_instrument_id(df: pd.DataFrame, instruments: pd.DataFrame) -> pd.DataFrame:
    """Add instrument_id column if missing, by merging on pair or ticker."""
    if "instrument_id" in df.columns:
        return df
    merge_on = "pair" if "pair" in df.columns else "ticker"
    return df.merge(instruments[[merge_on, "instrument_id"]], on=merge_on, how="left")


def keep_only_buy_sell(df: pd.DataFrame) -> pd.DataFrame:
    if df is not None and not df.empty:
        return df[df["predict"].isin(["up", "down"])]
    return df


def keep_only_tradable(df: pd.DataFrame, instruments: pd.DataFrame) -> pd.DataFrame:
    tradable = instruments.loc[instruments["use_for_trading_ib"] == 1, "instrument_id"]
    return df[df["instrument_id"].isin(tradable)]


def prepare_order_directories(base_path: str, cfg: VenturaConfig) -> str:
    today = cfg.today
    path = f"{base_path}{today.strftime('%Y-%m')}/{cfg.today_str}/"
    ensure_dir(path)
    return path


def get_order_file_path(
    account_id: int,
    file_type: str,
    cfg: VenturaConfig,
    execution_time_id: int,
    orders_dir: str,
) -> str:
    """Return the full path for an order CSV file."""
    type_map = {
        "New": ("New/", "new"),
        "new": ("New/", "new"),
        "Exit": ("Exit/", "exit"),
        "Combined": ("Combined/", "combined"),
    }
    sub_dir, prefix = type_map.get(file_type, ("New/", "new"))
    path = prepare_order_directories(orders_dir + sub_dir, cfg)
    file_name = "{}_{}_orders_{}_{}-{:02d}.csv".format(
        prefix, "", account_id,
        cfg.today.strftime("%Y%m%d"), execution_time_id,
    )
    # Clean up double underscore from empty string
    file_name = file_name.replace("__", "_")
    # Actually match old format: "{prefix}_orders_{account_id}_{date}-{et}.csv"
    file_name = f"{prefix}_orders_{account_id}_{cfg.today.strftime('%Y%m%d')}-{execution_time_id:02d}.csv"
    return path + file_name


def save_order_list(
    account_id: int,
    df: pd.DataFrame,
    file_type: str,
    cfg: VenturaConfig,
    execution_time_id: int,
    orders_dir: str,
) -> None:
    path = get_order_file_path(account_id, file_type, cfg, execution_time_id, orders_dir)
    df.to_csv(path, index=False)
    logger.info("Saved %s orders: %s (%d rows)", file_type, path, len(df))


def read_order_list(
    account_id: int,
    file_type: str,
    cfg: VenturaConfig,
    execution_time_id: int,
    orders_dir: str,
) -> pd.DataFrame:
    path = get_order_file_path(account_id, file_type, cfg, execution_time_id, orders_dir)
    return pd.read_csv(path)


def round_notionals(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    df["size_to_do"] = round_to_multiple(df["size_to_do"] - 0.25, 1)
    return df[df["size_to_do"] > 0]


def format_order_list_before_saving(df: pd.DataFrame, account_id: int) -> pd.DataFrame:
    df = df.rename(columns={"price_entry": "px_order", "strategy": "strategy_id"})
    df["account_id"] = account_id
    return df[
        ["account_id", "strategy_id", "instrument_id", "ticker",
         "future_id", "conid", "buy_sell", "size_to_do", "px_order"]
    ]


# ---------------------------------------------------------------------------
# NewOrdersBuilder (replaces PrepareNewOrdersList)
# ---------------------------------------------------------------------------

class NewOrdersBuilder:
    """Build new entry orders from filtered signals.

    Ported from ``PrepareNewOrdersList`` in old Signal_List.py.
    """

    def __init__(
        self,
        db: Database,
        cfg: VenturaConfig,
        execution_time_id: int,
        start_time,
        fx_vs_usd: pd.DataFrame,
    ) -> None:
        self.db = db
        self.cfg = cfg
        self.execution_time_id = execution_time_id
        self.start_time = start_time
        self.fx_vs_usd = fx_vs_usd
        self.orders_dir = cfg.data_dir + "Orders/"
        self.github_dir = cfg.data_dir + "Git/Ventura/"

        # Reference tables
        self._accounts = db.load_table_local("account", cfg.data_dir)
        self._futures = db.load_table_local("future_contract", cfg.data_dir)
        self._futures_expiry = db.load_table_local("future_expiry", cfg.data_dir)
        self._futures_active = db.load_table_local("future_active", cfg.data_dir)
        self._execution_times = db.load_table_local("schedule_execution", cfg.data_dir)
        self._etf = db.load_table_local("ETF", cfg.data_dir)

        threshold_minutes = 5
        time_limit = start_time - timedelta(minutes=threshold_minutes)
        sql = f"""
            SELECT C.conid, L.price
            FROM (
                SELECT conid, MAX(timestamp) AS timestamp
                FROM live_px_future GROUP BY conid
            ) C
            LEFT JOIN live_px_future L ON L.conid = C.conid AND L.timestamp = C.timestamp
            WHERE L.timestamp >= '{time_limit}'
        """
        self._futures_price = db.select(sql)

        self.signals: pd.DataFrame | None = None
        self.nav = pd.DataFrame()
        self.n_trd_before = 0
        self.n_trd_now = 0
        self.n_trd_later = 0
        self.n_trd_total = 0

    def run(self) -> None:
        """Execute the full pipeline: read signals, size, split, save."""
        self._read_github_signals(self.execution_time_id)
        if self.signals is None or self.signals.empty:
            logger.info("No signals to process")
            return

        self._initial_formatting()
        self._scale_down_if_too_many()
        self._cap_notional_vs_nav()
        self._get_latest_nav()
        self._split_by_trade_type()
        self._keep_with_price()
        self._final_formatting()
        self._save_by_account()

    # -- Read GitHub signals -----------------------------------------------

    def _read_github_signals(self, execution_time_id: int) -> pd.DataFrame:
        today = self.cfg.today
        path = f"{self.github_dir}trades_new/{today.strftime('%Y-%m')}/{self.cfg.today_str}/"
        file_name = f"new_{today.strftime('%Y%m%d')}-{execution_time_id:02d}.csv"
        print_banner(f"Reading: {path}{file_name}", False)

        df = pd.read_csv(path + file_name)
        for col in ["strategy", "buy_sell"]:
            df[col] = df[col].astype(int)

        if execution_time_id == self.execution_time_id:
            self.signals = df
        return df

    def _initial_formatting(self) -> None:
        instruments = self.cfg.tables.instruments
        df = add_back_instrument_id(self.signals, instruments)
        df = keep_only_buy_sell(df)
        df = keep_only_tradable(df, instruments)

        df = (
            df
            .merge(
                instruments[["instrument_id", "ccy", "asset_class", "trade_instrument_type"]],
                on="instrument_id", how="left",
            )
            .rename(columns={"ccy": "code", "strategy": "strategy_id"})
            .merge(self.fx_vs_usd, on="code", how="left")
            .rename(columns={"code": "ccy_instrument", "fx": "fx_instrument"})
            .assign(future_id=np.nan, expiry=np.nan)
        )
        self.signals = df

    # -- Trade count scaling -----------------------------------------------

    def _count_trades_before(self) -> None:
        i = 1
        while i < self.execution_time_id:
            try:
                dat_i = self._read_github_signals(i)
                dat_i = add_back_instrument_id(dat_i, self.cfg.tables.instruments)
                dat_i = keep_only_tradable(dat_i, self.cfg.tables.instruments)
                if dat_i is not None:
                    self.n_trd_before += len(dat_i)
            except Exception:
                pass
            i += 1
        self.n_trd_total += self.n_trd_before

    def _count_trades_now(self) -> None:
        if self.signals is not None:
            self.n_trd_now = len(self.signals)
        self.n_trd_total += self.n_trd_now

    def _estimate_trades_later(self) -> None:
        max_et = self._execution_times["execution_time_id"].max()
        i = self.execution_time_id + 1
        while i <= max_et:
            try:
                dat_i = get_model_predictions(
                    self.db, self.cfg, i, self.execution_time_id,
                )
                dat_i = keep_only_buy_sell(dat_i)
                dat_i = keep_only_tradable(dat_i, self.cfg.tables.instruments)
                if dat_i is not None:
                    self.n_trd_later += len(dat_i)
            except Exception:
                pass
            i += 1
        self.n_trd_total += self.n_trd_later

    def _scale_down_if_too_many(self) -> None:
        self._count_trades_before()
        self._count_trades_now()
        self._estimate_trades_later()
        adj_ratio = MAX_NB_TRADES / max(MAX_NB_TRADES, self.n_trd_total)
        self.signals["notional_for_1k_pnl"] *= adj_ratio

    def _cap_notional_vs_nav(self) -> None:
        if self.signals is None or self.signals.empty:
            return
        if len(self.signals) > 0:
            notional_usd = (
                self.signals["notional_for_1k_pnl"]
                * self.signals["price_entry"]
                * self.signals["fx_instrument"]
            )
            leverage = notional_usd / 100000
            adj_ratio = leverage.clip(upper=MAX_LEVERAGE_PER_TRADE) / leverage
            self.signals["notional_for_1k_pnl"] *= adj_ratio

    def _get_latest_nav(self) -> None:
        sql = """
            SELECT N.account_id, N.nav_usd AS nav
            FROM (
                SELECT account_id, MAX(timestamp) AS timestamp
                FROM book_nav WHERE HOUR(timestamp) <= 6
                GROUP BY account_id
            ) D
            LEFT JOIN book_nav N ON N.account_id = D.account_id AND N.timestamp = D.timestamp
        """
        self.nav = self.db.select(sql)

    # -- Split by trade type -----------------------------------------------

    def _for_type(self, trade_type: str) -> pd.DataFrame:
        return self.signals[self.signals["trade_instrument_type"] == trade_type]

    def _prepare_spot_or_direct_etf(self, order_type: str) -> pd.DataFrame:
        instruments = self.cfg.tables.instruments
        return (
            self._for_type(order_type)
            .merge(instruments[["instrument_id", "conid_spot"]], on="instrument_id", how="left")
            .rename(columns={"conid_spot": "conid"})
            [COLNAMES_ODA_NEW]
        )

    def _prepare_futures(self) -> pd.DataFrame:
        instruments = self.cfg.tables.instruments
        currencies = self.cfg.tables.currencies
        df = self._for_type("Future").drop(columns=["future_id", "expiry"], errors="ignore")

        # Build futures data
        df_fut = (
            df[["instrument_id"]].drop_duplicates()
            .merge(self._futures[self._futures["tradable"] == 1], on="instrument_id", how="left")
            .dropna(subset=["future_id"])
            .merge(self._futures_active, on="future_id", how="left")
            .merge(self._futures_expiry, on=["future_id", "conid"], how="left")
            .merge(self._futures_price, on="conid", how="left")
            .rename(columns={"notional": "notional_future"})
            [["instrument_id", "future_id", "ib_symbol", "expiry", "conid",
              "inverse_quote", "price", "notional_future", "ccy_id"]]
        )

        df = (
            df
            .merge(df_fut, on="instrument_id", how="left")
            .merge(currencies[["ccy_id", "ccy"]], on="ccy_id", how="left")
            .drop(columns="ccy_id")
            .rename(columns={"ccy": "code"})
            .merge(self.fx_vs_usd, on="code", how="left")
            .rename(columns={"code": "ccy_future", "fx": "fx_future"})
        )

        # Invert direction for inverse-quoted futures
        is_inv = df["inverse_quote"] == 1
        df.loc[is_inv, "buy_sell"] = -df.loc[is_inv, "buy_sell"]
        df.loc[is_inv, "notional_for_1k_pnl"] = 1 / df.loc[is_inv, "notional_for_1k_pnl"]
        df.loc[df["buy_sell"] == 1, "predict"] = "up"
        df.loc[df["buy_sell"] == -1, "predict"] = "down"

        # Recompute notional for futures
        df["notional_for_1k_pnl"] = (
            df["notional_for_1k_pnl"]
            / df["notional_future"]
            * df["fx_instrument"]
            / df["fx_future"]
        )

        df["price_entry"] = df["price"]
        return df[COLNAMES_ODA_NEW]

    def _split_by_trade_type(self) -> None:
        oda_spot = self._prepare_spot_or_direct_etf("Spot")
        oda_future = self._prepare_futures()
        oda_etf_direct = self._prepare_spot_or_direct_etf("Stock")
        oda_etf_index = (
            self._for_type("ETF")
            .merge(self._etf[["instrument_id", "conid"]], on="instrument_id", how="left")
            [COLNAMES_ODA_NEW]
        )

        self.signals = (
            pd.concat([oda_spot, oda_future, oda_etf_direct, oda_etf_index], ignore_index=True)
            .sort_values("trade_id")
        )

    # -- Final steps -------------------------------------------------------

    def _keep_with_price(self) -> None:
        self.signals = self.signals.dropna(subset=["price_entry"])

    def _final_formatting(self) -> None:
        int_cols = ["strategy_id", "instrument_id", "future_id", "conid", "buy_sell"]
        self.signals = force_column_type(self.signals, int_cols, int)

    def _save_by_account(self) -> None:
        print_banner("Exporting new orders by account")
        for account_id in self._accounts["account_id"]:
            nav_val = self.nav.loc[self.nav["account_id"] == account_id, "nav"].values[0]
            df = self.signals.copy()
            df["size_to_do"] = df["notional_for_1k_pnl"] * nav_val / 100000

            df = format_order_list_before_saving(df, account_id)
            df = round_notionals(df)
            df = df.dropna(subset=["conid", "size_to_do", "px_order"])
            save_order_list(
                account_id, df, "New",
                self.cfg, self.execution_time_id, self.orders_dir,
            )
