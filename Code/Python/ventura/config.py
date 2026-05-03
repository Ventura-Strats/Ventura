"""Ventura configuration: frozen dataclass replacing init.py globals."""

from __future__ import annotations

import os
from dataclasses import dataclass, field
from datetime import date

import pandas as pd

from ventura.db import Database
from ventura.utils import date_today, calc_previous_date, print_banner


# ---------------------------------------------------------------------------
# Path resolution (reads system files once)
# ---------------------------------------------------------------------------

def _read_system_file(name: str) -> str:
    """Read a single-line file from ~/Data/System/."""
    home = os.path.expanduser("~")
    path = os.path.join(home, "Data", "System", name)
    with open(path) as f:
        return f.read().strip()


def _resolve_project_dir() -> str:
    """Return the project root, e.g. /home/fls/Models/Ventura/HD/."""
    return _read_system_file("Location_Projects.txt") + "Ventura/HD/"


def _resolve_data_dir() -> str:
    """Return the data root, e.g. /home/fls/Data/Ventura/HD/."""
    return _read_system_file("Location_Data.txt") + "Ventura/HD/"


def _resolve_computer() -> str:
    """Return the single-char machine identifier (H, X, Z, …)."""
    return _read_system_file("this_computer.txt")[0]


# ---------------------------------------------------------------------------
# Reference tables (loaded once from local CSV cache)
# ---------------------------------------------------------------------------

def _load_instruments(db: Database, data_dir: str) -> pd.DataFrame:
    """Load INSTRUMENTS table with integer coercion (mirrors old init.py)."""
    df = db.load_table_local("INSTRUMENTS", data_dir)
    int_cols = [
        "instrument_id", "use_for_training", "use_for_trading",
        "use_for_trading_ib", "market_id", "execution_time_id",
        "conid_spot", "is_etf",
    ]
    for col in int_cols:
        if col in df.columns:
            df[col] = df[col].fillna(0).astype(int)
    return df


@dataclass(frozen=True)
class RefTables:
    """Static reference tables loaded from local CSV cache."""

    instruments: pd.DataFrame
    currencies: pd.DataFrame
    assets: pd.DataFrame
    strategies: pd.DataFrame
    trade_outcomes: pd.DataFrame
    tech_param: pd.DataFrame
    markets: pd.DataFrame
    regions: pd.DataFrame

    @classmethod
    def load(cls, db: Database, data_dir: str) -> RefTables:
        print_banner("Loading main data tables", False)
        return cls(
            instruments=_load_instruments(db, data_dir),
            currencies=db.load_table_local("currency", data_dir),
            assets=db.load_table_local("asset", data_dir),
            strategies=db.load_table_local("strategy", data_dir),
            trade_outcomes=db.load_table_local("trade_outcome", data_dir),
            tech_param=db.load_table_local("technical_parameters", data_dir),
            markets=db.load_table_local("market", data_dir),
            regions=db.load_table_local("region", data_dir),
        )


# ---------------------------------------------------------------------------
# VenturaConfig
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class VenturaConfig:
    """Immutable configuration — replaces init.py global variables.

    Usage::

        cfg = VenturaConfig.load()
        print(cfg.today, cfg.this_computer, cfg.project_dir)
        df = cfg.tables.instruments
    """

    this_computer: str
    today: date
    today_str: str
    yesterday: date
    project_dir: str     # e.g. /home/fls/Models/Ventura/HD/
    data_dir: str        # e.g. /home/fls/Data/Ventura/HD/
    db: Database
    tables: RefTables

    @classmethod
    def load(cls, db: Database | None = None) -> VenturaConfig:
        """Build config from system files + DB.

        If *db* is not provided, a new ``Database()`` is created.
        """
        print_banner("Initializing application parameters", False)

        computer = _resolve_computer()
        today = date_today()
        yesterday = calc_previous_date(today)
        project_dir = _resolve_project_dir()
        data_dir = _resolve_data_dir()

        print_banner(f"This computer: {computer}", False)
        print_banner(f"System date: {today}", False)

        if db is None:
            db = Database()

        tables = RefTables.load(db, data_dir)

        return cls(
            this_computer=computer,
            today=today,
            today_str=today.strftime("%Y-%m-%d"),
            yesterday=yesterday,
            project_dir=project_dir,
            data_dir=data_dir,
            db=db,
            tables=tables,
        )
