#!/usr/bin/env python3
"""Signal List — prepares signals, GitHub file, and order lists for execution."""

import os
import sys
sys.path.insert(0, "/home/fls/Models/Ventura/HD/Code/Python")

from ventura.script_runner import run_script, ScriptContext
from ventura.ib.connection import IBConnection
from ventura.signals.predictions import PrepareSignalsList
from ventura.signals.github import GithubSignalExporter
from ventura.signals.order_list import NewOrdersBuilder
from ventura.signals.exits import ExitOrdersBuilder
from ventura.signals.combined import CombinedOrdersBuilder
from ventura.utils import print_banner


def get_fx_vs_usd(db):
    """Load latest FX rates vs USD."""
    sql = """
        SELECT A.code, H.fx
        FROM (
            SELECT asset_id, MAX(date) AS date
            FROM histo_fx_close_vs_usd GROUP BY asset_id
        ) D
        LEFT JOIN histo_fx_close_vs_usd H ON D.asset_id = H.asset_id AND D.date = H.date
        LEFT JOIN static_asset A ON A.asset_id = H.asset_id
    """
    return db.select(sql)


def find_execution_time(db, cfg):
    """Determine execution_time_id from schedule + current time."""
    from ventura.ib.connection import IBConnection
    # Re-use existing logic from execution_utils
    import execution_utils as vx
    return vx.findExecutionTime()


def send_telegram_signals():
    """Trigger R scripts to send Telegram notifications."""
    os.system("RScriptVentura.sh Communications/Send_Predict.R")
    os.system("RScriptVentura.sh Communications/Send_Signal.R")


def wait_for_predict(db, cfg):
    """Wait for the Predict script to finish (re-uses existing logic)."""
    import execution_utils as vx
    vx.waitTillPreviousJobHasFinished(
        previous_script_name="Predict",
        last_completed_stage=1,
        nb_jobs_total=8,
        max_lag_prices_minutes=7,
        max_wait_time_before_continuing_minutes=4,
    )


def main(ctx: ScriptContext) -> None:
    db = ctx.db
    cfg = ctx.config

    # Wait for Predict to finish
    wait_for_predict(db, cfg)

    # Determine execution time
    execution_time_id = find_execution_time(db, cfg)

    # FX rates
    fx_vs_usd = get_fx_vs_usd(db)

    # 1. Prepare filtered signals
    signals = PrepareSignalsList(
        db, cfg, execution_time_id, ctx.start_time,
    ).run()
    print_banner("SIGNALS")

    # 2. Export to GitHub
    GithubSignalExporter(signals, fx_vs_usd, cfg, execution_time_id).run()
    print_banner("GITHUB DONE")

    # 3. Send Telegram notifications
    send_telegram_signals()

    # 4. Prepare new entry orders
    NewOrdersBuilder(db, cfg, execution_time_id, ctx.start_time, fx_vs_usd).run()
    print_banner("NEW ORDERS DONE")

    # 5. Prepare exit + combined orders per account (needs IB connection)
    port = IBConnection.get_port(2, cfg.this_computer)
    client_id = IBConnection.get_client_id(ctx.script_name, db, cfg.data_dir)

    with IBConnection(port=port, client_id=client_id, readonly=True) as conn:
        for account_id in [1, 2]:
            ExitOrdersBuilder(db, cfg, execution_time_id, account_id).run()
            print_banner(f"EXIT DONE account {account_id}")

            CombinedOrdersBuilder(
                db, cfg, conn, execution_time_id, account_id,
            ).run()
            print_banner(f"COMBINED DONE account {account_id}")


if __name__ == "__main__":
    run_script("Signal_List", main_fn=main, max_time_hours=0.25)
