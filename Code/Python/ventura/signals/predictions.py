"""Load and filter model predictions from the database.

Extracts the ``PrepareSignalsList`` class and ``getModelPredictions``
logic from the old Signal_List.py.
"""

from __future__ import annotations

import logging
from datetime import datetime, timedelta

import numpy as np
import pandas as pd

from ventura.config import VenturaConfig
from ventura.db import Database
from ventura.utils import print_banner

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Model predictions from DB
# ---------------------------------------------------------------------------

def get_markets_to_do(cfg: VenturaConfig, execution_time_id: int) -> str:
    """Return comma-separated market_ids for the given execution time."""
    instruments = cfg.tables.instruments
    market_ids = instruments.loc[
        instruments["execution_time_id"] == execution_time_id, "market_id"
    ].unique()
    return ",".join(map(str, market_ids))


def get_model_predictions_one(
    db: Database,
    execution_time_id: int,
    use_weights: int,
    markets_id: str,
) -> pd.DataFrame | None:
    """Load predictions from live_predict for one weight setting."""
    sql = f"""
        SELECT P.instrument_id, P.strategy_id, P.date, P.timestamp, P.timestamp_px,
               P.close, P.t_up, P.t_dn, T.outcome AS predict,
               P.proba_up, P.proba_flat, P.proba_down
        FROM live_predict P
        LEFT JOIN static_instrument I ON I.instrument_id = P.instrument_id
        LEFT JOIN static_market M ON M.market_id = I.market_id
        LEFT JOIN static_trade_outcome T ON T.outcome_id = P.outcome_id
        WHERE P.score = 10
          AND P.use_weights = {use_weights}
          AND M.market_id IN ({markets_id})
    """
    dat = db.select(sql)

    if use_weights == 1 and dat is not None and not dat.empty:
        dat = (
            dat[["instrument_id", "strategy_id", "date", "timestamp", "timestamp_px",
                 "predict", "proba_up", "proba_flat", "proba_down"]]
            .rename(columns={
                "predict": "predict_w",
                "proba_up": "proba_up_w",
                "proba_flat": "proba_flat_w",
                "proba_down": "proba_down_w",
            })
        )
    return dat


def get_model_predictions(
    db: Database,
    cfg: VenturaConfig,
    execution_time_id: int,
    current_execution_time_id: int,
) -> pd.DataFrame | None:
    """Load and merge weighted + unweighted predictions.

    Parameters
    ----------
    current_execution_time_id:
        The execution time id of the current run (for verbose logging).
    """
    probas = db.load_table_local("probability_threshold", cfg.data_dir)
    markets_id = get_markets_to_do(cfg, execution_time_id)

    dat_no_w = get_model_predictions_one(db, execution_time_id, 0, markets_id)
    dat_w = get_model_predictions_one(db, execution_time_id, 1, markets_id)

    merge_cols = ["strategy_id", "instrument_id", "date", "timestamp", "timestamp_px"]

    dat = (
        dat_no_w
        .merge(dat_w, on=merge_cols, how="left")
        .merge(probas, on="strategy_id", how="left")
        .rename(columns={"proba_threshold": "threshold"})
    )

    if execution_time_id == current_execution_time_id:
        print_banner("getModelPredictions result:", False)
        logger.info("Predictions: %d rows", len(dat))

    return dat


# ---------------------------------------------------------------------------
# PrepareSignalsList — filter predictions into actionable signals
# ---------------------------------------------------------------------------

class PrepareSignalsList:
    """Filter model predictions into actionable signals.

    Ported from the ``PrepareSignalsList`` class in old Signal_List.py.
    """

    def __init__(
        self,
        db: Database,
        cfg: VenturaConfig,
        execution_time_id: int,
        start_time: datetime,
        threshold_minutes: int = 5,
        hour_new_day: int = 6,
    ) -> None:
        self.db = db
        self.cfg = cfg
        self.execution_time_id = execution_time_id
        self.start_time = start_time
        self.threshold_minutes = threshold_minutes
        self.hour_new_day = hour_new_day
        self.signals: pd.DataFrame | None = None

    def run(self) -> pd.DataFrame | None:
        """Execute the full pipeline and return filtered signals."""
        self.signals = get_model_predictions(
            self.db, self.cfg, self.execution_time_id, self.execution_time_id,
        )
        if self.signals is None or self.signals.empty:
            return self.signals

        self._compare_times()
        self._keep_recent_only()
        self._keep_signals_fitting_criteria()
        self._format_predictions()
        self._remove_bonds()
        return self.signals

    # -- Time comparison ---------------------------------------------------

    def _convert_db_times(self, col_name: str) -> list[float]:
        """Calculate minutes between now and a timestamp column."""
        today_str = self.cfg.today_str
        diffs = []
        for val in self.signals[col_name]:
            ts_str = today_str + " " + str(val)[-8:]
            dt = datetime.strptime(ts_str, "%Y-%m-%d %H:%M:%S")
            if dt.hour < self.hour_new_day:
                dt += timedelta(days=1)
            diff = (self.start_time - dt).total_seconds() / 60
            diffs.append(diff)
        return diffs

    def _compare_times(self) -> None:
        self.signals["diff_vs_now_px"] = self._convert_db_times("timestamp_px")
        self.signals["diff_vs_now_predict"] = self._convert_db_times("timestamp")

    def _keep_recent_only(self) -> None:
        t = self.threshold_minutes
        df = self.signals
        self.signals = df[
            (df.diff_vs_now_px > 0) & (df.diff_vs_now_px < t)
            & (df.diff_vs_now_predict > 0) & (df.diff_vs_now_predict < t)
        ].copy()

    # -- Probability filters -----------------------------------------------

    def _work_probabilities(self) -> None:
        df = self.signals.copy()
        for suffix in ["", "_w"]:
            predict_col = "predict" + suffix
            proba_up_col = "proba_up" + suffix
            proba_down_col = "proba_down" + suffix
            signal_col = "proba_signal" + suffix
            anti_col = "proba_antisignal" + suffix
            diff_col = "proba_diff" + suffix

            df[signal_col] = np.nan
            df[anti_col] = np.nan

            pos_up = df[predict_col] == "up"
            pos_down = df[predict_col] == "down"

            df.loc[pos_up, signal_col] = df.loc[pos_up, proba_up_col]
            df.loc[pos_down, signal_col] = df.loc[pos_down, proba_down_col]
            df.loc[pos_up, anti_col] = df.loc[pos_up, proba_down_col]
            df.loc[pos_down, anti_col] = df.loc[pos_down, proba_up_col]

            df[diff_col] = df[signal_col] - df[anti_col]

        df["max_proba_diff"] = df[["proba_diff", "proba_diff_w"]].max(axis=1)
        self.signals = df

    def _keep_models_agree(self) -> None:
        self.signals = self.signals[self.signals.predict == self.signals.predict_w].copy()

    def _keep_good_proba(self) -> None:
        df = self.signals
        self.signals = df[
            (df.predict == "flat") | (df.max_proba_diff >= df.threshold)
        ].copy()

    def _keep_signals_fitting_criteria(self) -> None:
        self._work_probabilities()
        self._keep_models_agree()
        self._keep_good_proba()

    # -- Final formatting --------------------------------------------------

    def _format_predictions(self) -> None:
        self.signals["execution_time_id"] = self.execution_time_id
        cols = [
            "execution_time_id", "strategy_id", "instrument_id", "date",
            "timestamp", "timestamp_px", "close", "t_up", "t_dn", "predict",
        ]
        self.signals = self.signals[cols]

    def _remove_bonds(self) -> None:
        """Remove bond instruments (temporary until bond history is clean)."""
        instruments = self.cfg.tables.instruments
        self.signals = (
            self.signals
            .merge(instruments[["instrument_id", "asset_class"]], on="instrument_id", how="left")
            .query("asset_class != 'bond'")
            .drop(columns="asset_class")
        )
