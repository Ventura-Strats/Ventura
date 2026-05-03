#!/usr/bin/env python3
"""Price IB Future — retrieves live + historical prices for active futures."""

import sys
sys.path.insert(0, "/home/fls/Models/Ventura/HD/Code/Python")

from ventura.script_runner import run_script, ScriptContext
from ventura.ib.connection import IBConnection
from ventura.ib.prices import FuturePriceRetriever


def main(ctx: ScriptContext) -> None:
    account_id = 1
    port = IBConnection.get_port(account_id, ctx.config.this_computer)
    client_id = IBConnection.get_client_id(ctx.script_name, ctx.db, ctx.config.data_dir)

    with IBConnection(port=port, client_id=client_id, readonly=True) as conn:
        retriever = FuturePriceRetriever(
            conn, ctx.db, ctx.config, start_time=ctx.start_time,
        )
        retriever.run()


if __name__ == "__main__":
    run_script("Price_IB_Future", main_fn=main, max_time_hours=1 / 60)
