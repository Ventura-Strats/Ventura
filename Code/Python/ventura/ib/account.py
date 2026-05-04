"""IB Account Data retrieval using ib_insync.

Replaces the old IBAppBook class (ibapi callbacks) with synchronous
ib_insync calls. Retrieves NAV, portfolio positions, and FX rates
from Interactive Brokers, saves to CSV and DB.
"""

from __future__ import annotations

import logging
from datetime import datetime
from typing import Dict

import pandas as pd

from ventura.config import VenturaConfig
from ventura.db import Database
from ventura.ib.connection import IBConnection
from ventura.utils import print_banner

logger = logging.getLogger(__name__)

# Columns for the px_position CSV output
_PX_POSITION_COLS = [
    "account_id", "conid", "symbol", "secType",
    "primaryExchange", "currency", "price", "position",
]


class AccountDataRetriever:
    """Retrieve account data (NAV, positions, FX) from IB and persist.

    Usage::

        retriever = AccountDataRetriever(db=db, cfg=cfg, timestamp_str=ts)
        for account_id in [1, 2]:
            with IBConnection(port=get_port(account_id), ...) as conn:
                retriever.get_account_data(account_id, conn)
        retriever.save_fx_to_db()
    """

    def __init__(
        self,
        db: Database,
        cfg: VenturaConfig,
        timestamp_str: str,
    ) -> None:
        self.db = db
        self.cfg = cfg
        self.timestamp_str = timestamp_str

        # Output paths
        data_dir = cfg.data_dir + "Account_Data/"
        self.path_account = data_dir + "Account/"
        self.path_px_position = data_dir + "Px_Position/"

        # Accumulated across accounts for FX/position DB writes
        self._account_dfs: Dict[int, pd.DataFrame] = {}

    # -- Main entry point per account --------------------------------------

    def get_account_data(self, account_id: int, conn: IBConnection) -> None:
        """Retrieve and save account data for one IB account.

        Each account requires its own IBConnection (different port per account).
        """
        print_banner(f"Retrieving data for account {account_id}")

        # Request account summary + portfolio from IB
        account_values = self._fetch_account_values(conn)
        portfolio = self._fetch_portfolio(conn, account_id)

        if account_values.empty or portfolio.empty:
            print_banner(
                f"No data received for account {account_id} — skipping",
                False,
            )
            return

        # Save CSVs
        self._save_account_csv(account_values, account_id)
        self._save_px_position_csv(portfolio, account_id)

        # Save NAV to DB
        self._save_nav_to_db(account_values, account_id)

        # Store for later FX aggregation
        self._account_dfs[account_id] = account_values

    # -- IB data fetching --------------------------------------------------

    def _fetch_account_values(self, conn: IBConnection) -> pd.DataFrame:
        """Fetch account values (key/value/currency rows) via ib_insync."""
        values = conn.ib.accountValues()
        if not values:
            return pd.DataFrame()

        rows = [
            {"key": v.tag, "value": v.value, "ccy": v.currency}
            for v in values
        ]
        return pd.DataFrame(rows)

    def _fetch_portfolio(self, conn: IBConnection, account_id: int) -> pd.DataFrame:
        """Fetch portfolio positions via ib_insync, format to match old CSV."""
        positions = conn.ib.portfolio()
        if not positions:
            return pd.DataFrame()

        rows = []
        for item in positions:
            c = item.contract
            price = item.marketPrice
            # FTSE correction: Z symbol prices need 100x multiplier
            if c.symbol == "Z":
                price = price * 100
            rows.append({
                "account_id": account_id,
                "conid": c.conId,
                "symbol": c.symbol,
                "secType": c.secType,
                "primaryExchange": c.primaryExchange,
                "currency": c.currency,
                "price": price,
                "position": item.position,
            })

        return pd.DataFrame(rows, columns=_PX_POSITION_COLS)

    # -- CSV persistence ---------------------------------------------------

    def _save_account_csv(self, df: pd.DataFrame, account_id: int) -> None:
        """Save account data to timestamped + _last CSV files."""
        ts_name = f"account_{account_id}_{self.timestamp_str}.csv"
        last_name = f"account_{account_id}_last.csv"
        df.to_csv(self.path_account + ts_name, index=False)
        df.to_csv(self.path_account + last_name, index=False)
        logger.info("Saved account CSV: %s", ts_name)

    def _save_px_position_csv(self, df: pd.DataFrame, account_id: int) -> None:
        """Save position data to timestamped + _last CSV files."""
        ts_name = f"px_position_{account_id}_{self.timestamp_str}.csv"
        last_name = f"px_position_{account_id}_last.csv"
        df.to_csv(self.path_px_position + ts_name, index=False)
        df.to_csv(self.path_px_position + last_name, index=False)
        logger.info("Saved px_position CSV: %s", ts_name)

    # -- NAV to DB ---------------------------------------------------------

    def _save_nav_to_db(self, dat_account: pd.DataFrame, account_id: int) -> None:
        """Insert NAV (in local ccy and USD) into book_nav table."""
        nav_row = dat_account[dat_account["key"] == "NetLiquidation"]
        if nav_row.empty:
            logger.warning("No NetLiquidation found for account %s", account_id)
            return

        nav = float(nav_row["value"].values[0])
        fx = self._find_fx_from_account(dat_account, account_id)
        nav_usd = nav * fx

        sql = (
            "INSERT INTO book_nav "
            "(account_id, date, timestamp, nav_ccy, nav_usd) "
            f"VALUES ({account_id}, '{self.cfg.today_str}', "
            f"'{self.timestamp_str}', {nav}, {nav_usd})"
        )
        self.db.execute(sql)
        logger.info(
            "NAV saved: account=%s nav_ccy=%.2f fx=%.6f nav_usd=%.2f",
            account_id, nav, fx, nav_usd,
        )

    def _find_fx_from_account(
        self, dat_account: pd.DataFrame, account_id: int
    ) -> float:
        """Derive USD exchange rate from account data ExchangeRate rows."""
        accounts = self.db.load_table_local("account", self.cfg.data_dir)
        ccy_id = accounts.loc[
            accounts["account_id"] == account_id, "ccy_id"
        ].values[0]
        ccy = self.cfg.tables.currencies.loc[
            self.cfg.tables.currencies["ccy_id"] == ccy_id, "ccy"
        ].values[0]

        fx_rows = dat_account[dat_account["key"] == "ExchangeRate"]
        fx_ccy = float(
            fx_rows.loc[fx_rows["ccy"] == ccy, "value"].values[0]
        )
        fx_usd = float(
            fx_rows.loc[fx_rows["ccy"] == "USD", "value"].values[0]
        )
        return fx_ccy / fx_usd

    # -- FX prices + positions to DB (called after all accounts) -----------

    def save_fx_to_db(self) -> None:
        """Aggregate FX rates and positions across accounts, write to DB.

        Call this after ``get_account_data`` has been called for all accounts.
        """
        if not self._account_dfs:
            logger.warning("No account data collected — skipping FX save")
            return

        dat_fx = pd.DataFrame()
        dat_pos = pd.DataFrame()

        for account_id, dat_acct in self._account_dfs.items():
            dat_fx = self._extract_fx_data(dat_acct, dat_fx)
            dat_pos = self._extract_position_data(dat_acct, dat_pos, account_id)

        # Format and write FX prices
        dat_fx = self._format_fx_prices(dat_fx)
        self.db.append("live_px_fx_book", dat_fx)
        logger.info("Saved %d FX price rows to live_px_fx_book", len(dat_fx))

        # Format and write FX positions
        dat_pos = self._format_positions(dat_pos)
        self.db.append("book_live_position_fx", dat_pos)
        logger.info("Saved %d position rows to book_live_position_fx", len(dat_pos))

    def _extract_fx_data(
        self, dat: pd.DataFrame, accumulated: pd.DataFrame
    ) -> pd.DataFrame:
        """Extract ExchangeRate rows, normalize to USD base."""
        fx_rows = dat[
            (dat["key"] == "ExchangeRate") & (dat["ccy"] != "BASE")
        ].copy()
        fx_rows["value"] = fx_rows["value"].astype(float)
        fx_usd = fx_rows.loc[fx_rows["ccy"] == "USD", "value"].values[0]
        fx_rows["value"] = fx_rows["value"] / fx_usd
        return pd.concat([accumulated, fx_rows], ignore_index=True)

    def _extract_position_data(
        self,
        dat: pd.DataFrame,
        accumulated: pd.DataFrame,
        account_id: int,
    ) -> pd.DataFrame:
        """Extract TotalCashBalance rows."""
        pos_rows = dat[
            (dat["key"] == "TotalCashBalance") & (dat["ccy"] != "BASE")
        ].copy()
        pos_rows["account_id"] = account_id
        pos_rows["value"] = pos_rows["value"].astype(float)
        return pd.concat([accumulated, pos_rows], ignore_index=True)

    def _format_fx_prices(self, dat_fx: pd.DataFrame) -> pd.DataFrame:
        """Format FX data for live_px_fx_book table."""
        assets = self.cfg.tables.assets
        return (
            dat_fx
            .drop(columns="key")
            .rename(columns={"ccy": "code", "value": "price"})
            .groupby("code", as_index=False)
            .agg({"price": "mean"})
            .merge(assets, on="code", how="left")
            .assign(timestamp=self.timestamp_str)
            [["asset_id", "timestamp", "price"]]
        )

    def _format_positions(self, dat_pos: pd.DataFrame) -> pd.DataFrame:
        """Format position data for book_live_position_fx table."""
        assets = self.cfg.tables.assets
        return (
            dat_pos
            .drop(columns="key")
            .rename(columns={"ccy": "code", "value": "position"})
            .merge(assets, on="code", how="left")
            .assign(
                timestamp=self.timestamp_str,
                position=lambda x: x["position"].astype(float),
            )
            [["account_id", "asset_id", "timestamp", "position"]]
        )
