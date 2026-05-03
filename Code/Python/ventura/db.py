"""Ventura database interface: connection pooling, queries, table operations."""

from __future__ import annotations

import os
import random
import string
from datetime import datetime

import pandas as pd
from sqlalchemy import create_engine, text
from sqlalchemy.engine import Engine

from ventura.utils import print_banner


# ---------------------------------------------------------------------------
# Database class
# ---------------------------------------------------------------------------

class Database:
    """MySQL interface with a single pooled engine.

    Usage::

        db = Database()          # reads credentials from env vars
        df = db.select("SELECT * FROM INSTRUMENTS")
        db.execute("UPDATE foo SET bar = 1 WHERE id = 2")
        db.append("my_table", df)
    """

    def __init__(
        self,
        user: str | None = None,
        password: str | None = None,
        host: str | None = None,
        database: str = "Ventura",
        pool_size: int = 5,
        pool_recycle: int = 1800,
    ) -> None:
        user = user or os.environ.get("VENTURA_DB_USER", "ventura")
        password = password or os.environ.get("VENTURA_DB_PASSWORD")
        host = host or os.environ.get("VENTURA_DB_HOST", "192.168.0.37")

        if not password:
            raise EnvironmentError(
                "VENTURA_DB_PASSWORD environment variable not set. "
                "Add to ~/.bashrc:\n"
                "  export VENTURA_DB_USER='ventura'\n"
                "  export VENTURA_DB_PASSWORD='your_password'"
            )

        url = f"mysql+pymysql://{user}:{password}@{host}/{database}"
        self._engine: Engine = create_engine(
            url,
            pool_size=pool_size,
            pool_recycle=pool_recycle,
            pool_pre_ping=True,
        )

    # -- Core operations ----------------------------------------------------

    def execute(self, sql: str) -> None:
        """Run a non-SELECT statement (INSERT, UPDATE, CREATE, DROP, …)."""
        with self._engine.connect() as conn:
            conn.execute(text(sql))
            conn.commit()

    def select(self, sql: str) -> pd.DataFrame:
        """Run a SELECT and return the result as a DataFrame."""
        with self._engine.connect() as conn:
            return pd.read_sql(text(sql), conn)

    # -- Table helpers ------------------------------------------------------

    def load_table(self, table: str) -> pd.DataFrame:
        """SELECT * from a database table."""
        return self.select(f"SELECT * FROM {table}")

    def drop_table(self, table: str) -> None:
        self.execute(f"DROP TABLE {table}")

    def append(self, table: str, df: pd.DataFrame) -> None:
        """Append rows to an existing table."""
        with self._engine.connect() as conn:
            df.to_sql(table, conn, if_exists="append", index=False)
            conn.commit()

    def replace(self, table: str, df: pd.DataFrame) -> None:
        """Replace (drop+recreate) a table with *df*."""
        with self._engine.connect() as conn:
            df.to_sql(table, conn, if_exists="replace", index=False)
            conn.commit()

    def write_new(self, table: str, df: pd.DataFrame) -> None:
        """Write *df* as a new table (fails if table exists)."""
        with self._engine.connect() as conn:
            df.to_sql(table, conn, if_exists="fail", index=False)
            conn.commit()

    # -- Local CSV tables ---------------------------------------------------

    def load_table_local(self, table: str, data_dir: str) -> pd.DataFrame:
        """Load a table from the local CSV cache.

        Mirrors the old convention: INSTRUMENTS and ETF are stored as-is,
        everything else is prefixed with ``static_``.
        """
        if table not in ("INSTRUMENTS", "ETF"):
            table = f"static_{table}"
        path = os.path.join(data_dir, "Tables_Local", f"{table}.csv")
        return pd.read_csv(path)

    # -- Utility ------------------------------------------------------------

    def test_write(self) -> bool:
        """Verify the DB is writable (create tmp table, insert, read, drop)."""
        tmp = _temp_table_name()
        try:
            self.execute(f"CREATE TABLE {tmp} (test_txt CHAR(1) NULL)")
            self.execute(f"INSERT INTO {tmp} SET test_txt = 'A'")
            result = self.load_table(tmp)
            self.drop_table(tmp)
            return len(result) == 1
        except Exception:
            return False

    def dispose(self) -> None:
        """Dispose the connection pool (call at shutdown)."""
        self._engine.dispose()


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _temp_table_name(script_name: str | None = None) -> str:
    """Generate a unique temporary table name (mirrors old nameTmpTable)."""
    rand = "".join(random.choices(string.ascii_lowercase + string.digits, k=6))
    tag = "noscript" if script_name is None else script_name[-15:]
    ts = datetime.now().strftime("%m%d_%H%M")
    return f"z{rand}_{tag}_{ts}"
