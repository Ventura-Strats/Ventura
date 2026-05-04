"""IB price retrieval using ib_insync.

Replaces the old ibapi callback classes (ibApp in Price_IB.py and
ibAppLive in Price_IB_Future.py) with synchronous ib_insync calls.

Two main classes:
- HistoricalPriceRetriever: daily OHLC for spot/index/metal/ETF instruments
- FuturePriceRetriever: live + historical prices for active futures
"""

from __future__ import annotations

import logging
from datetime import datetime, timedelta

import numpy as np
import pandas as pd
from ib_insync import IB, Contract, Forex

from ventura.config import VenturaConfig
from ventura.db import Database
from ventura.ib.connection import IBConnection
from ventura.utils import print_banner

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Historical spot/index/metal/ETF prices (replaces Price_IB.py)
# ---------------------------------------------------------------------------

class HistoricalPriceRetriever:
    """Retrieve daily historical OHLC from IB for spot instruments.

    Builds the asset list from DB, requests historical bars, and saves
    per-pair CSV files that the R pipeline picks up.

    Usage::

        with IBConnection(port=7497, client_id=10) as conn:
            retriever = HistoricalPriceRetriever(conn, db, cfg, account_id=1)
            retriever.run()
    """

    def __init__(
        self,
        conn: IBConnection,
        db: Database,
        cfg: VenturaConfig,
        account_id: int,
        bar_type: str = "1 day",
        histo_length: str = "2 W",
    ) -> None:
        self.conn = conn
        self.db = db
        self.cfg = cfg
        self.account_id = account_id
        self.bar_type = bar_type
        self.histo_length = histo_length

        self.file_path = cfg.data_dir + "Spot/IB/"
        self.date_format = "%Y%m%d" if "hour" not in bar_type else "%Y%m%d  %H:%M:%S"

    def run(self) -> None:
        """Fetch historical data for all instruments and save CSVs."""
        asset_list = self._build_asset_list()
        logger.info("Fetching historical prices for %d instruments", len(asset_list))

        for _, row in asset_list.iterrows():
            pair = row["pair"]
            print_banner(f"Fetching {pair} ...", False)
            try:
                contract = self._build_contract(row)
                self.conn.ib.qualifyContracts(contract)

                bars = self.conn.ib.reqHistoricalData(
                    contract,
                    endDateTime="",
                    durationStr=self.histo_length,
                    barSizeSetting=self.bar_type,
                    whatToShow=row["data_type"],
                    useRTH=False,
                    formatDate=1,
                )

                if not bars:
                    logger.warning("No data for %s", pair)
                    continue

                df = pd.DataFrame([
                    {
                        "pair": pair,
                        "date": b.date if isinstance(b.date, str) else b.date.strftime(self.date_format),
                        "open": b.open,
                        "high": b.high,
                        "low": b.low,
                        "close": b.close,
                    }
                    for b in bars
                ])
                df["date"] = pd.to_datetime(df["date"], format=self.date_format)

                file_name = f"{pair}_histo.csv"
                df.to_csv(self.file_path + file_name, index=False)
                logger.info("Saved %s (%d bars)", file_name, len(df))

            except Exception as exc:
                logger.warning("Failed for %s: %s", pair, exc)

    # -- Asset list building (mirrors old getDBAssetData + formatAssetDat) --

    def _build_asset_list(self) -> pd.DataFrame:
        """Build the list of instruments to fetch, from DB + ETF proxies."""
        dat = self._get_db_asset_data()
        dat = self._format_asset_data(dat)

        dat_etf_proxy = self._get_etf_proxy_data()
        dat = pd.concat([dat, dat_etf_proxy], ignore_index=True)
        return dat

    def _get_db_asset_data(self) -> pd.DataFrame:
        """Query instruments that have IB historical data."""
        if self.account_id == 2:
            where = (
                "WHERE T.asset_class = 'fx_dm' "
                "OR (T.asset_class = 'fx_em' AND V.attribute = 'have_ib_histo' AND U.value = 1)"
            )
        else:
            where = (
                "WHERE V.attribute = 'have_ib_histo' AND U.value = 1 "
                "AND T.asset_class NOT IN ('fx_dm', 'fx_em')"
            )

        sql = f"""
            SELECT CONCAT(A.code, C.ccy) AS pair, T.asset_class, M.market,
                   X.ib_symbol, Y.ib_exchange
            FROM static_instrument I
            LEFT JOIN static_currency C ON C.ccy_id = I.ccy_id
            LEFT JOIN static_asset A ON A.asset_id = I.asset_id
            LEFT JOIN static_asset_class T ON T.asset_class_id = I.asset_class_id
            LEFT JOIN static_market M ON M.market_id = I.market_id
            LEFT JOIN static_instrument_attribute_int U ON U.instrument_id = I.instrument_id
            LEFT JOIN static_instrument_attribute_type V ON V.attribute_id = U.attribute_id
            LEFT JOIN (
                SELECT instrument_id, value AS ib_symbol
                FROM static_instrument_attribute_chr WHERE attribute_id = 4
            ) X ON X.instrument_id = I.instrument_id
            LEFT JOIN (
                SELECT M2.instrument_id, N.ib_exchange
                FROM (
                    SELECT instrument_id, value AS exchange_id
                    FROM static_instrument_attribute_int WHERE attribute_id = 3
                ) M2
                LEFT JOIN static_exchange N ON N.exchange_id = M2.exchange_id
            ) Y ON Y.instrument_id = I.instrument_id
            {where}
            ORDER BY pair
        """
        df = self.db.select(sql)
        df = df.merge(
            self.cfg.tables.instruments[["pair", "is_etf"]],
            on="pair", how="left",
        )
        return df

    def _format_asset_data(self, dat: pd.DataFrame) -> pd.DataFrame:
        """Assign secType, exchange, symbol, data_type based on asset class."""
        dat = dat.copy()
        is_index = dat["asset_class"] == "index"
        is_metal = dat["asset_class"] == "metal"
        is_etf = dat["is_etf"] == 1

        # Symbol
        dat["symbol"] = dat["pair"].str[:3]
        dat.loc[is_index, "symbol"] = dat.loc[is_index, "ib_symbol"]
        dat.loc[is_metal, "symbol"] = dat.loc[is_metal, "pair"]

        # secType
        dat["secType"] = "CASH"
        dat.loc[is_index, "secType"] = "IND"
        dat.loc[is_metal, "secType"] = "CMDTY"
        dat.loc[is_etf, "secType"] = "STK"

        # exchange
        dat["exchange"] = "IDEALPRO"
        dat.loc[is_index, "exchange"] = dat.loc[is_index, "ib_exchange"]
        dat.loc[is_metal, "exchange"] = dat.loc[is_metal, "ib_exchange"]
        dat.loc[is_etf, "exchange"] = "SMART"

        nyse_pairs = [
            "AGGUSD", "BNDUSD", "BSVUSD", "EMBUSD", "HYDUSD", "HYGUSD",
            "IEFUSD", "JNKUSD", "LQDUSD", "MBBUSD", "TIPUSD", "TLTUSD",
            "VCIUSD", "VTIUSD",
        ]
        dat.loc[dat["pair"].isin(nyse_pairs), "exchange"] = "NYSE"

        # currency
        dat["currency"] = dat["pair"].str[-3:]

        # data_type
        dat["data_type"] = "MIDPOINT"
        dat.loc[is_index, "data_type"] = "TRADES"

        return dat[["pair", "symbol", "secType", "exchange", "currency", "data_type"]]

    def _get_etf_proxy_data(self) -> pd.DataFrame:
        """Build asset rows for ETF proxy instruments."""
        currencies = self.db.load_table_local("currency", self.cfg.data_dir)
        etf_proxy = self.db.load_table_local("etf_proxy", self.cfg.data_dir)
        etf_proxy = etf_proxy.merge(currencies[["ccy_id", "ccy"]], on="ccy_id", how="left")
        return pd.DataFrame({
            "pair": "ETF_" + etf_proxy["ib_symbol"],
            "symbol": etf_proxy["ib_symbol"],
            "secType": "STK",
            "exchange": "SMART",
            "currency": etf_proxy["ccy"],
            "data_type": "TRADES",
        })

    @staticmethod
    def _build_contract(row: pd.Series) -> Contract:
        """Build an IB Contract from an asset list row."""
        contract = Contract()
        contract.symbol = row["symbol"]
        contract.secType = row["secType"]
        contract.exchange = row["exchange"]
        contract.currency = row["currency"]
        return contract


# ---------------------------------------------------------------------------
# Future prices (replaces Price_IB_Future.py)
# ---------------------------------------------------------------------------

class FuturePriceRetriever:
    """Retrieve live + historical prices for active futures contracts.

    Uses ib_insync's synchronous API instead of the old ibapi callback
    pattern. Fetches bid/ask/last from market data, historical close,
    and portfolio book prices. Combines with recent DB prices using a
    weighted-average approach to get the best usable price.

    Usage::

        with IBConnection(port=7497, client_id=12) as conn:
            retriever = FuturePriceRetriever(conn, db, cfg, start_time)
            retriever.run()
    """

    def __init__(
        self,
        conn: IBConnection,
        db: Database,
        cfg: VenturaConfig,
        start_time: datetime,
        bar_type: str = "1 day",
        histo_length: str = "1 D",
        wait_seconds: float = 10.0,
        recent_threshold_minutes: int = 2,
    ) -> None:
        self.conn = conn
        self.db = db
        self.cfg = cfg
        self.start_time = start_time
        self.bar_type = bar_type
        self.histo_length = histo_length
        self.wait_seconds = wait_seconds
        self.recent_threshold_minutes = recent_threshold_minutes

        self.timestamp_str = start_time.strftime("%Y-%m-%d %H:%M:%S")
        self.file_path = cfg.data_dir + "Spot/IB_Future/"

    def run(self) -> None:
        """Fetch future prices, calculate best price, save to CSV + DB."""
        dat = self._get_contract_list()
        if dat.empty:
            logger.warning("No active futures found")
            return

        # Fetch live prices from IB
        dat = self._fetch_ib_prices(dat)

        # Merge with recent DB prices
        dat = self._read_investing_prices(dat)

        # Calculate best usable price
        dat = self._calc_prices(dat)

        # Drop rows without prices
        dat = dat.dropna(subset=["price"])

        # Save
        dat.to_csv(self.file_path + "px_future.csv", index=False)
        logger.info("Saved px_future.csv (%d rows)", len(dat))

        dat_db = dat[["conid", "timestamp", "price"]]
        self.db.append("live_px_future", dat_db)
        logger.info("Saved %d rows to live_px_future", len(dat_db))

    # -- Contract list building --------------------------------------------

    def _get_contract_list(self) -> pd.DataFrame:
        """Build the list of active future contracts with IB details."""
        futures = self.db.load_table_local("future_contract", self.cfg.data_dir)
        expiries = self.db.load_table_local("future_expiry", self.cfg.data_dir)
        active = self.db.load_table_local("future_active", self.cfg.data_dir)
        exchanges = self.db.load_table_local("exchange", self.cfg.data_dir)
        currencies = self.db.load_table_local("currency", self.cfg.data_dir)
        instruments = self.cfg.tables.instruments

        dat = (
            active
            .merge(expiries, on=["conid", "future_id"], how="left")
            .merge(futures, on="future_id", how="left")
            .merge(currencies, on="ccy_id", how="left")
            .merge(exchanges, on="exchange_id", how="left")
            .merge(instruments[["instrument_id", "asset_class"]], on="instrument_id", how="left")
        )

        dat["sectype"] = "FUT"
        dat["data_type"] = "MIDPOINT"
        dat.loc[dat["asset_class"] == "index", "data_type"] = "TRADES"

        dat = dat[
            ["instrument_id", "future_id", "ib_symbol", "conid",
             "sectype", "expiry", "ccy", "ib_exchange", "data_type", "tick_size"]
        ].copy()

        dat["timestamp"] = self.timestamp_str
        for col in ["bid", "ask", "mid", "last", "histo", "book"]:
            dat[col] = np.nan

        return dat

    # -- IB price fetching -------------------------------------------------

    def _fetch_ib_prices(self, dat: pd.DataFrame) -> pd.DataFrame:
        """Fetch bid/ask/last/histo/book for each future contract."""
        ib = self.conn.ib

        # Book prices from portfolio
        portfolio = ib.portfolio()
        for item in portfolio:
            mask = dat["conid"] == item.contract.conId
            if mask.any():
                dat.loc[mask, "book"] = item.marketPrice

        # Per-contract: market data + historical
        for i, row in dat.iterrows():
            conid = int(row["conid"])
            contract = Contract(
                symbol=row["ib_symbol"],
                currency=row["ccy"],
                secType="FUT",
                exchange=row["ib_exchange"],
                conId=conid,
            )

            try:
                qualified = ib.qualifyContracts(contract)
                if not qualified:
                    logger.warning("Could not qualify future conid %s", conid)
                    continue
            except Exception as exc:
                logger.warning("Qualify failed for conid %s: %s", conid, exc)
                continue

            # Market data snapshot
            try:
                ticker = ib.reqMktData(contract, snapshot=True)
                ib.sleep(2)
                if ticker.bid and ticker.bid > 0 and ticker.bid != -1:
                    dat.at[i, "bid"] = ticker.bid
                if ticker.ask and ticker.ask > 0 and ticker.ask != -1:
                    dat.at[i, "ask"] = ticker.ask
                if ticker.last and ticker.last > 0 and ticker.last != -1:
                    dat.at[i, "last"] = ticker.last
                ib.cancelMktData(contract)
            except Exception as exc:
                logger.warning("Market data failed for conid %s: %s", conid, exc)

            # Historical close (today only)
            try:
                bars = ib.reqHistoricalData(
                    contract,
                    endDateTime="",
                    durationStr=self.histo_length,
                    barSizeSetting=self.bar_type,
                    whatToShow=row["data_type"],
                    useRTH=False,
                    formatDate=1,
                )
                for bar in bars:
                    bar_date = pd.to_datetime(str(bar.date), format="%Y%m%d")
                    if bar_date.date() >= self.cfg.today:
                        dat.at[i, "histo"] = float(bar.close)
            except Exception as exc:
                logger.warning("Historical data failed for conid %s: %s", conid, exc)

        return dat

    # -- Investing.com DB fallback prices ----------------------------------

    def _read_investing_prices(self, dat: pd.DataFrame) -> pd.DataFrame:
        """Merge with recent prices already in live_px_future."""
        time_limit = self.start_time - timedelta(minutes=self.recent_threshold_minutes)
        time_str = time_limit.strftime("%Y-%m-%d %H:%M:%S")

        sql = f"""
            SELECT J.conid, L.price AS investing
            FROM (
                SELECT conid, MAX(timestamp) AS timestamp
                FROM live_px_future
                GROUP BY conid
            ) J
            LEFT JOIN live_px_future L ON L.conid = J.conid AND L.timestamp = J.timestamp
            WHERE J.timestamp >= '{time_str}'
        """
        dat_px = self.db.select(sql)
        dat["price"] = np.nan
        return dat.merge(dat_px, on="conid", how="left")

    # -- Best-price calculation (weighted average) -------------------------

    def _calc_prices(self, dat: pd.DataFrame) -> pd.DataFrame:
        """Calculate the best usable price per row using weighted average."""
        for i in range(len(dat)):
            self._calc_single_price(dat, i)
        return dat

    @staticmethod
    def _calc_single_price(dat: pd.DataFrame, i: int) -> None:
        """Determine best usable price for row i.

        Weights: mid=400, last=200, histo=200, book=100, investing=1.
        Bid/ask validity checks applied first.
        """
        bid = dat["bid"].values[i]
        ask = dat["ask"].values[i]
        last = dat["last"].values[i]
        histo = dat["histo"].values[i]
        book = dat["book"].values[i]
        tick_size = dat["tick_size"].values[i]
        investing = dat.get("investing", pd.Series(dtype=float)).values[i] if "investing" in dat.columns else np.nan

        mid = np.nan

        if np.isnan(bid) or np.isnan(ask):
            bid = ask = np.nan
        elif ask <= bid:
            bid = ask = np.nan
        else:
            mid = 0.5 * (bid + ask)
            spread_ticks = (ask - bid) / tick_size
            if spread_ticks > 10:
                bid = ask = mid = np.nan
            elif spread_ticks <= 1.5:
                # Tight spread — mid is very reliable, ignore others
                last = histo = book = np.nan

        if not np.isnan(mid):
            if not np.isnan(last) and not (bid <= last <= ask):
                last = np.nan
            if not np.isnan(histo) and not (bid <= histo <= ask):
                histo = np.nan
            if not np.isnan(book) and not (bid <= book <= ask):
                book = np.nan

        prices = pd.DataFrame({
            "weight": [400, 200, 200, 100, 1],
            "price": [mid, last, histo, book, investing],
        }).dropna(subset=["price"])

        price = np.nan
        if len(prices) > 0:
            price = (prices["weight"] * prices["price"]).sum() / prices["weight"].sum()

        dat["mid"].values[i] = mid
        dat["price"].values[i] = price


# ---------------------------------------------------------------------------
# Execution-time live prices (replaces Price_IB_Exec.py)
# ---------------------------------------------------------------------------

class ExecPriceRetriever:
    """Fetch live prices for instruments with active combined orders.

    Runs in a loop during order execution to provide real-time mark-to-market.
    Reads the combined_orders CSV to know which conids to price, fetches
    bid/ask/last/histo/book from IB, calculates a weighted-average best price,
    and saves to ``live_px_exec`` DB table + CSV.

    Usage::

        with IBConnection(port=7497, client_id=15) as conn:
            retriever = ExecPriceRetriever(conn, db, cfg, execution_time_id)
            retriever.run_once()  # single pass
            # or loop externally with run_once() + sleep
    """

    PRICE_COLS = ["bid", "ask", "mid", "last", "histo", "book"]

    def __init__(
        self,
        conn: IBConnection,
        db: Database,
        cfg: VenturaConfig,
        execution_time_id: int,
        start_time: datetime,
        account_ids: list = None,
        wait_seconds: float = 5.0,
        recent_threshold_minutes: int = 5,
    ) -> None:
        self.conn = conn
        self.db = db
        self.cfg = cfg
        self.execution_time_id = execution_time_id
        self.start_time = start_time
        self.account_ids = account_ids or [1, 2]
        self.wait_seconds = wait_seconds
        self.recent_threshold_minutes = recent_threshold_minutes

        self.timestamp_str = start_time.strftime("%Y-%m-%d %H:%M:%S")
        self.file_path = cfg.data_dir + "Spot/Live_Exec/"

    def run_once(self) -> pd.DataFrame:
        """Single pass: read orders, fetch prices, save. Returns priced df."""
        contract_list = self._read_instruments_list()
        if contract_list.empty:
            logger.info("No instruments to price")
            return contract_list

        contract_list = self._fetch_ib_prices(contract_list)
        contract_list = self._read_investing_prices(contract_list)
        contract_list = self._calc_prices(contract_list)
        contract_list = contract_list.dropna(subset=["price"])

        self._save(contract_list)
        return contract_list

    def erase_live_prices(self) -> None:
        """Clear previous execution prices (call at start of exec session)."""
        import os
        file_name = self.file_path + "px_live_exec.csv"
        try:
            os.remove(file_name)
        except OSError:
            pass
        self.db.execute("TRUNCATE TABLE live_px_exec")

    # -- Read combined orders to get instrument list --------------------------

    def _read_instruments_list(self) -> pd.DataFrame:
        """Read combined order CSVs for all accounts, deduplicate."""
        from ventura.signals.order_list import read_order_list

        frames = []
        for account_id in self.account_ids:
            try:
                df = read_order_list(
                    account_id, "Combined",
                    self.cfg, self.execution_time_id,
                    self.cfg.data_dir + "Orders/",
                )
                df = df[["instrument_id", "ticker", "asset_class",
                         "future_id", "conid"]].copy()
                for col in ["instrument_id", "future_id", "conid"]:
                    df[col] = df[col].astype(int)
                frames.append(df)
            except Exception as exc:
                logger.warning("Could not read combined orders for account %d: %s",
                               account_id, exc)

        if not frames:
            return pd.DataFrame()

        df = pd.concat(frames, ignore_index=True).drop_duplicates()
        return self._format_contracts(df)

    def _format_contracts(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add price columns and data_type."""
        df = df.copy()
        df["timestamp"] = self.timestamp_str
        for col in self.PRICE_COLS:
            df[col] = np.nan
        df["data_type"] = "MIDPOINT"
        df.loc[df["asset_class"] == "index", "data_type"] = "TRADES"
        return df

    # -- IB price fetching ----------------------------------------------------

    def _fetch_ib_prices(self, dat: pd.DataFrame) -> pd.DataFrame:
        """Fetch bid/ask/last/histo/book from IB for each conid."""
        ib = self.conn.ib

        # Book prices from portfolio
        portfolio = ib.portfolio()
        for item in portfolio:
            mask = dat["conid"] == item.contract.conId
            if mask.any():
                dat.loc[mask, "book"] = item.marketPrice

        # Per-contract market data + historical
        for i, row in dat.iterrows():
            conid = int(row["conid"])
            contract = Contract(conId=conid)

            try:
                qualified = ib.qualifyContracts(contract)
                if not qualified:
                    logger.warning("Could not qualify conid %d", conid)
                    continue
            except Exception as exc:
                logger.warning("Qualify failed for conid %d: %s", conid, exc)
                continue

            # Market data snapshot
            try:
                ticker = ib.reqMktData(contract, snapshot=True)
                ib.sleep(self.wait_seconds)
                if ticker.bid and ticker.bid > 0 and ticker.bid != -1:
                    dat.at[i, "bid"] = ticker.bid
                if ticker.ask and ticker.ask > 0 and ticker.ask != -1:
                    dat.at[i, "ask"] = ticker.ask
                if ticker.last and ticker.last > 0 and ticker.last != -1:
                    dat.at[i, "last"] = ticker.last
                ib.cancelMktData(contract)
            except Exception as exc:
                logger.warning("Market data failed for conid %d: %s", conid, exc)

            # Historical close (today only)
            try:
                bars = ib.reqHistoricalData(
                    contract,
                    endDateTime="",
                    durationStr="1 D",
                    barSizeSetting="1 day",
                    whatToShow=row["data_type"],
                    useRTH=False,
                    formatDate=1,
                )
                for bar in bars:
                    bar_date = pd.to_datetime(str(bar.date), format="%Y%m%d")
                    if bar_date.date() >= self.cfg.today:
                        dat.at[i, "histo"] = float(bar.close)
            except Exception as exc:
                logger.warning("Historical data failed for conid %d: %s", conid, exc)

        return dat

    # -- DB fallback prices ---------------------------------------------------

    def _read_investing_prices(self, dat: pd.DataFrame) -> pd.DataFrame:
        """Merge with recent prices from live_px_future as fallback."""
        time_limit = self.start_time - timedelta(minutes=self.recent_threshold_minutes)
        time_str = time_limit.strftime("%Y-%m-%d %H:%M:%S")

        sql = f"""
            SELECT J.conid, L.price AS investing
            FROM (
                SELECT conid, MAX(timestamp) AS timestamp
                FROM live_px_future
                GROUP BY conid
            ) J
            LEFT JOIN live_px_future L ON L.conid = J.conid AND L.timestamp = J.timestamp
            WHERE J.timestamp >= '{time_str}'
        """
        dat_px = self.db.select(sql)
        if dat_px.empty:
            dat["investing"] = np.nan
            return dat
        return dat.merge(dat_px, on="conid", how="left")

    # -- Price calculation (same logic as FuturePriceRetriever) ---------------

    def _calc_prices(self, dat: pd.DataFrame) -> pd.DataFrame:
        """Calculate weighted-average best price for each row."""
        dat["price"] = np.nan
        for i in range(len(dat)):
            self._calc_single_price(dat, i)
        return dat

    @staticmethod
    def _calc_single_price(dat: pd.DataFrame, i: int) -> None:
        """Determine best usable price for row i.

        Weights: mid=400, last=200, histo=200, book=100, investing=1.
        """
        bid = dat["bid"].values[i]
        ask = dat["ask"].values[i]
        last = dat["last"].values[i]
        histo = dat["histo"].values[i]
        book = dat["book"].values[i]
        investing = dat["investing"].values[i] if "investing" in dat.columns else np.nan

        mid = np.nan

        if np.isnan(bid) or np.isnan(ask):
            bid = ask = np.nan
        elif ask <= bid:
            bid = ask = np.nan
        else:
            mid = 0.5 * (bid + ask)
            bid_ask_pct = 100 * (ask - bid) / mid
            if bid_ask_pct > 0.1:
                bid = ask = mid = np.nan
            elif bid_ask_pct <= 0.002:
                last = histo = book = np.nan

        if not np.isnan(mid):
            if not np.isnan(last) and not (bid <= last <= ask):
                last = np.nan
            if not np.isnan(histo) and not (bid <= histo <= ask):
                histo = np.nan
            if not np.isnan(book) and not (bid <= book <= ask):
                book = np.nan

        prices = pd.DataFrame({
            "weight": [400, 200, 200, 100, 1],
            "price": [mid, last, histo, book, investing],
        }).dropna(subset=["price"])

        price = np.nan
        if len(prices) > 0:
            price = (prices["weight"] * prices["price"]).sum() / prices["weight"].sum()

        dat["mid"].values[i] = mid
        dat["price"].values[i] = price

    # -- Save -----------------------------------------------------------------

    def _save(self, dat: pd.DataFrame) -> None:
        """Save prices to CSV and DB."""
        out_cols = ["conid", "timestamp",
                    "bid", "ask", "mid", "last", "histo", "book",
                    "investing", "price"]
        # Ensure all columns exist
        for col in out_cols:
            if col not in dat.columns:
                dat[col] = np.nan

        dat.to_csv(self.file_path + "px_live_exec.csv", index=False)
        dat_db = dat[out_cols]
        self.db.replace("live_px_exec", dat_db)
        logger.info("Saved %d exec prices to live_px_exec", len(dat_db))
