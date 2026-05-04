#!/usr/bin/env python3
"""Price IB Exec — fetches live prices for instruments being executed.

Runs in a loop while Execute_Orders.py is still running, providing
real-time mark-to-market prices for active orders.
"""

import subprocess
import sys
import time
from datetime import datetime, timedelta

sys.path.insert(0, "/home/fls/Models/Ventura/HD/Code/Python")

from ventura.script_runner import run_script, ScriptContext
from ventura.scheduling import find_execution_time, wait_for_previous_job
from ventura.ib.connection import IBConnection
from ventura.ib.prices import ExecPriceRetriever
from ventura.utils import print_banner

WAIT_TIME_BETWEEN_PASSES_SECONDS = 10


def _execution_script_still_running() -> bool:
    """Check if execute_orders.py is still running."""
    try:
        result = subprocess.check_output(
            "ps -aux | grep '[p]ython.*execute_orders'",
            shell=True,
        )
        return len(result.strip()) > 0
    except subprocess.CalledProcessError:
        return False


def main(ctx: ScriptContext) -> None:
    db = ctx.db
    cfg = ctx.config
    max_time_hours = 7.0 / 60  # ~7 minutes

    execution_time_id = find_execution_time(db, cfg.data_dir, ctx.start_time)
    time_finish = ctx.start_time + timedelta(hours=max_time_hours)

    # Wait for Signal_List to finish first
    wait_for_previous_job(db, cfg.data_dir, "Signal_List", 1, 1, 5, 5)

    port = IBConnection.get_port(1, cfg.this_computer)
    client_id = IBConnection.get_client_id(ctx.script_name, db, cfg.data_dir)

    with IBConnection(port=port, client_id=client_id, readonly=True) as conn:
        retriever = ExecPriceRetriever(
            conn, db, cfg,
            execution_time_id=execution_time_id,
            start_time=ctx.start_time,
        )

        # Clear old prices
        retriever.erase_live_prices()

        # Loop until timeout or execution script finishes
        keep_going = datetime.now() <= time_finish
        while keep_going:
            print_banner("Fetching exec prices...", False)
            retriever.run_once()

            time.sleep(WAIT_TIME_BETWEEN_PASSES_SECONDS)

            still_running = _execution_script_still_running()
            keep_going = (datetime.now() <= time_finish) and still_running

            if not still_running:
                print_banner("Execute_Orders no longer running — stopping")

    print_banner("Price IB Exec complete")


if __name__ == "__main__":
    run_script("Price_IB_Exec", main_fn=main, max_time_hours=7.0 / 60)
