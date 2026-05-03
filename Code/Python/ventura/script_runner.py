"""Ventura script runner: Python equivalent of R's I.executeScript().

Provides the ``run_script()`` entry point that handles:
- Initialization (config, DB, reference tables)
- Pre-flight checks (DB writable, drives accessible)
- Script status logging to ``status_script`` table
- Graceful shutdown with banner
"""

from __future__ import annotations

import os
import sys
import time
from dataclasses import dataclass
from datetime import datetime
from typing import Callable

from ventura.config import VenturaConfig
from ventura.db import Database
from ventura.utils import print_banner


# ---------------------------------------------------------------------------
# ScriptContext — passed to the user's main function
# ---------------------------------------------------------------------------

@dataclass
class ScriptContext:
    """Runtime context available inside every script's ``main(ctx)``."""

    script_name: str
    config: VenturaConfig
    db: Database
    start_time: datetime

    # Status tracking (populated after DB registration)
    script_id: int | None = None
    machine_id: int | None = None
    script_pid: int | None = None
    script_run_id: int = 0


# ---------------------------------------------------------------------------
# Pre-flight checks
# ---------------------------------------------------------------------------

def _directory_not_empty(path: str) -> bool:
    try:
        return len(os.listdir(path)) > 0
    except Exception:
        return False


def _check_drives(cfg: VenturaConfig) -> bool:
    return (
        _directory_not_empty(cfg.project_dir)
        and _directory_not_empty(cfg.data_dir)
    )


def _check_can_execute(cfg: VenturaConfig) -> bool:
    drives_ok = _check_drives(cfg)
    db_ok = cfg.db.test_write()
    return drives_ok and db_ok


# ---------------------------------------------------------------------------
# DB status logging
# ---------------------------------------------------------------------------

def _get_script_id(db: Database, script_name: str, data_dir: str) -> int:
    scripts = db.load_table_local("script", data_dir)
    return int(scripts.loc[scripts["script"] == script_name, "script_id"].values[0])


def _get_machine_id(db: Database, computer: str, data_dir: str) -> int:
    machines = db.load_table_local("machine", data_dir)
    return int(machines.loc[machines["machine"] == computer, "machine_id"].values[0])


def _register_script(ctx: ScriptContext) -> None:
    """Insert a row into status_script at script start."""
    ctx.script_id = _get_script_id(ctx.db, ctx.script_name, ctx.config.data_dir)
    ctx.machine_id = _get_machine_id(ctx.db, ctx.config.this_computer, ctx.config.data_dir)
    ctx.script_pid = os.getpid()
    ctx.script_run_id = 0

    start_str = ctx.start_time.strftime("%Y-%m-%d %H:%M:%S")
    sql = (
        "INSERT INTO status_script "
        "(script_id, start, complete, machine_id, pid, run_id) "
        f"VALUES ({ctx.script_id}, '{start_str}', 0, "
        f"'{ctx.machine_id}', {ctx.script_pid}, {ctx.script_run_id})"
    )
    ctx.db.execute(sql)


def _mark_complete(ctx: ScriptContext) -> None:
    """Update status_script to mark the script as complete."""
    if ctx.script_id is None:
        print_banner("Script finished (DB status not updated — init incomplete)")
        return

    end_str = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    start_str = ctx.start_time.strftime("%Y-%m-%d %H:%M:%S")
    sql = (
        "UPDATE status_script "
        f"SET complete = 1, end = '{end_str}', last_completed_stage = 1 "
        f"WHERE script_id = {ctx.script_id} "
        f"AND start = '{start_str}' "
        f"AND machine_id = {ctx.machine_id} "
        f"AND run_id = {ctx.script_run_id} "
        f"AND pid = {ctx.script_pid}"
    )
    ctx.db.execute(sql)


# ---------------------------------------------------------------------------
# run_script — the main entry point
# ---------------------------------------------------------------------------

def run_script(
    script_name: str,
    main_fn: Callable[[ScriptContext], None],
    max_time_hours: float = 1.0,
) -> None:
    """Run a Ventura Python script with full lifecycle management.

    This is the Python equivalent of R's ``source("Init.R"); I.executeScript()``.

    Parameters
    ----------
    script_name:
        Name matching the ``script`` column in the ``static_script`` CSV.
    main_fn:
        The script's business logic, called as ``main_fn(ctx)``.
    max_time_hours:
        Maximum allowed runtime (currently logged, not enforced).

    Example::

        def main(ctx: ScriptContext) -> None:
            data = ctx.db.select("SELECT * FROM prices")
            ...

        if __name__ == "__main__":
            run_script("My_Script", main_fn=main, max_time_hours=0.5)
    """
    start_time = datetime.now()

    print_banner(f"Script: {script_name}")
    print_banner(f"PID: {os.getpid()}", False)

    # Initialize config (loads DB, reference tables, system info)
    db = Database()
    cfg = VenturaConfig.load(db=db)

    ctx = ScriptContext(
        script_name=script_name,
        config=cfg,
        db=db,
        start_time=start_time,
    )

    # Pre-flight checks
    can_execute = _check_can_execute(cfg)
    print_banner(f"Good to start: {can_execute}")

    if not can_execute:
        print_banner("DB not writable — not running the script")
        _goodbye()
        return

    # Register in status_script
    _register_script(ctx)

    # Run the main function
    try:
        main_fn(ctx)
    except Exception as exc:
        print_banner(f"Script error: {exc}")
        raise
    finally:
        _mark_complete(ctx)
        print_banner(f"Script Complete — {script_name}")
        _goodbye()


def _goodbye() -> None:
    print_banner("Goodbye...")
    time.sleep(1)
