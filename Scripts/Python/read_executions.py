#!/usr/bin/env python3
"""Read Executions — reads today's fills from IB and saves to CSV + DB."""

import sys
sys.path.insert(0, "/home/fls/Models/Ventura/HD/Code/Python")

from ventura.script_runner import run_script, ScriptContext
from ventura.ib.connection import IBConnection
from ventura.ib.executions import ExecutionReader


ACCOUNT_IDS = [1, 2]


def main(ctx: ScriptContext) -> None:
    port = IBConnection.get_port(1, ctx.config.this_computer)
    client_id = IBConnection.get_client_id(ctx.script_name, ctx.db, ctx.config.data_dir)

    with IBConnection(port=port, client_id=client_id, readonly=True) as conn:
        reader = ExecutionReader(conn, ctx.db, ctx.config, ctx.start_time)

        for account_id in ACCOUNT_IDS:
            reader.read_executions(account_id)

    reader.save_to_files()
    reader.save_to_db()


if __name__ == "__main__":
    run_script("Read_Executions", main_fn=main)
