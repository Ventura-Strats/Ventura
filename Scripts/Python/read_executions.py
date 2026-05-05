#!/usr/bin/env python3
"""Read Executions — reads today's fills from IB and saves to CSV + DB."""

import sys
sys.path.insert(0, "/home/fls/Models/Ventura/HD/Code/Python")

from ventura.script_runner import run_script, ScriptContext
from ventura.ib.connection import IBConnection
from ventura.ib.executions import ExecutionReader


ACCOUNT_IDS = [1, 2]


def main(ctx: ScriptContext) -> None:
    client_id = IBConnection.get_client_id(ctx.script_name, ctx.db, ctx.config.data_dir)

    reader = ExecutionReader(db=ctx.db, cfg=ctx.config, start_time=ctx.start_time)

    for account_id in ACCOUNT_IDS:
        port = IBConnection.get_port(account_id, ctx.config.this_computer)
        with IBConnection(port=port, client_id=client_id, readonly=True) as conn:
            reader.read_executions(account_id, conn)

    reader.save_to_files()
    reader.save_to_db()


if __name__ == "__main__":
    run_script("Read_Executions", main_fn=main)
