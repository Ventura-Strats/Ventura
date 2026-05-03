"""Prepare and export signals to GitHub.

Extracts the ``PrepareGithubSignals`` class from old Signal_List.py.
"""

from __future__ import annotations

import logging
import os
from datetime import timedelta

import pandas as pd

from ventura.config import VenturaConfig
from ventura.utils import print_banner, ensure_dir

logger = logging.getLogger(__name__)

COLS_GITHUB_FILE = [
    "trade_id", "date_trade", "strategy", "ticker", "price_entry",
    "predict", "buy_sell", "target", "stop_loss", "date_exit_latest",
    "range_dn", "range_up", "tp_pct", "notional_for_1k_pnl",
    "timestamp_px", "timestamp_signal",
]


class GithubSignalExporter:
    """Prepare a GitHub-format signals file and push it.

    Ported from ``PrepareGithubSignals`` in old Signal_List.py.
    """

    def __init__(
        self,
        signals: pd.DataFrame,
        fx_vs_usd: pd.DataFrame,
        cfg: VenturaConfig,
        execution_time_id: int,
    ) -> None:
        self.signals = signals.copy()
        self.github = signals.copy()
        self.fx_vs_usd = fx_vs_usd
        self.cfg = cfg
        self.execution_time_id = execution_time_id
        self.path_github = cfg.data_dir + "Git/Ventura/"
        self.file_path: str | None = None
        self.file_name: str | None = None

    def run(self) -> None:
        """Execute full pipeline: format, save, push."""
        if len(self.signals) > 0:
            self._prepare_table()
        else:
            self.github = pd.DataFrame(columns=COLS_GITHUB_FILE)
        self._save_file()
        self._push_to_github()

    # -- Formatting --------------------------------------------------------

    def _prepare_table(self) -> None:
        self._add_basic_columns()
        self._calc_targets_and_notionals()
        self._add_trade_id()
        self._add_latest_exit_date()
        self._final_formatting()

    def _add_basic_columns(self) -> None:
        instruments = self.cfg.tables.instruments
        markets = self.cfg.tables.markets
        regions = self.cfg.tables.regions

        df = (
            self.github
            .merge(instruments, on=["execution_time_id", "instrument_id"], how="left")
            .merge(markets, on=["market", "market_id"], how="left")
            .merge(regions, on="region_id", how="left")
        )

        df["code"] = df["pair"].str[:3]
        df["buy_sell"] = 0
        df.loc[df["predict"] == "up", "buy_sell"] = 1
        df.loc[df["predict"] == "down", "buy_sell"] = -1

        df = df.merge(self.fx_vs_usd, on="code", how="left")
        self.github = df

    def _calc_targets_and_notionals(self) -> None:
        df = self.github.copy()
        is_flat = df["predict"] == "flat"

        df["target"] = df["close"] + df["buy_sell"] * (df["t_up"] - df["close"])
        df.loc[is_flat, "target"] = df.loc[is_flat, "t_up"]
        df["stop_loss"] = 2 * df["close"] - df["target"]
        df["pnl_tgt_ccy_1"] = 1000 / df["fx"]
        df["tp_pct"] = df["t_up"] / df["close"] - 1
        df["notional_for_1k_pnl"] = df["pnl_tgt_ccy_1"] / df["tp_pct"]
        self.github = df

    def _add_trade_id(self) -> None:
        today_str = self.cfg.today.strftime("%Y%m%d")
        et_str = f"{self.execution_time_id:02d}"
        base = f"V{today_str}{et_str}"
        self.github["trade_id"] = [
            f"{base}{i + 1:03d}" for i in range(len(self.github))
        ]

    def _add_latest_exit_date(self) -> None:
        self.github["date_exit_latest"] = self.github["date"].map(
            lambda d: d + timedelta(days=7)
        )

    def _final_formatting(self) -> None:
        self.github = (
            self.github
            .rename(columns={
                "date": "date_trade",
                "strategy_id": "strategy",
                "close": "price_entry",
                "t_up": "range_up",
                "t_dn": "range_dn",
                "timestamp": "timestamp_signal",
            })
            [COLS_GITHUB_FILE]
        )

    # -- Save + push -------------------------------------------------------

    def _prepare_dir(self) -> str:
        today = self.cfg.today
        path = f"{self.path_github}trades_new/{today.strftime('%Y-%m')}/{self.cfg.today_str}/"
        ensure_dir(path)
        return path

    def _save_file(self) -> None:
        self.file_path = self._prepare_dir()
        et_str = f"{self.execution_time_id:02d}"
        self.file_name = f"new_{self.cfg.today.strftime('%Y%m%d')}-{et_str}.csv"
        full_path = self.file_path + self.file_name
        self.github.to_csv(full_path, index=False)
        logger.info("Saved GitHub file: %s", full_path)

    def _push_to_github(self) -> None:
        export_path = self.file_path[self.file_path.find("trades_new"):]
        os.system(f"GitPushVentura.sh {export_path} {self.file_name}")
        print_banner("Sent to Github - Done")
