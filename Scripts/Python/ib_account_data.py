#!/usr/bin/env python3
"""IB Account Data — retrieves NAV, positions, FX from Interactive Brokers."""

import sys
sys.path.insert(0, "/home/fls/Models/Ventura/HD/Code/Python")

from ventura.script_runner import run_script, ScriptContext
from ventura.ib.connection import IBConnection
from ventura.ib.account import AccountDataRetriever


ACCOUNT_IDS = [1, 2]


def main(ctx: ScriptContext) -> None:
    timestamp_str = ctx.start_time.strftime("%Y-%m-%d %H:%M:%S")
    port = IBConnection.get_port(1, ctx.config.this_computer)
    client_id = IBConnection.get_client_id(ctx.script_name, ctx.db, ctx.config.data_dir)

    with IBConnection(port=port, client_id=client_id, readonly=True) as conn:
        retriever = AccountDataRetriever(conn, ctx.db, ctx.config, timestamp_str)

        for account_id in ACCOUNT_IDS:
            retriever.get_account_data(account_id)

        retriever.save_fx_to_db()


if __name__ == "__main__":
    run_script("IB_Account_Data", main_fn=main, max_time_hours=1 / 60)
