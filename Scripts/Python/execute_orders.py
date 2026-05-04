#!/usr/bin/env python3
"""Execute Orders — reads combined order CSV and executes via IB chase algo."""

import sys
sys.path.insert(0, "/home/fls/Models/Ventura/HD/Code/Python")

from ventura.script_runner import run_script, ScriptContext
from ventura.scheduling import find_execution_time
from ventura.ib.connection import IBConnection
from ventura.ib.orders import Order, OrderExecutor, AssetClass, OrderStatus
from ventura.signals.order_list import read_order_list
from ventura.utils import print_banner

ACCOUNT_IDS = [1, 2]


def main(ctx: ScriptContext) -> None:
    db = ctx.db
    cfg = ctx.config

    execution_time_id = find_execution_time(db, cfg.data_dir, ctx.start_time)

    for account_id in ACCOUNT_IDS:
        print_banner(f"Execute orders — account {account_id}")

        # Read combined orders CSV
        try:
            orders_df = read_order_list(
                account_id, "Combined",
                cfg, execution_time_id,
                cfg.data_dir + "Orders/",
            )
        except Exception:
            print_banner(f"No combined orders for account {account_id}")
            continue

        if orders_df.empty:
            print_banner(f"No orders for account {account_id}")
            continue

        # Convert DataFrame rows to Order objects
        orders = []
        for _, row in orders_df.iterrows():
            asset_class_str = str(row.get("asset_class", "")).lower()
            if "fx" in asset_class_str:
                ac = AssetClass.FX
            elif "index" in asset_class_str or "future" in asset_class_str:
                ac = AssetClass.FUTURE
            else:
                ac = AssetClass.STOCK

            order = Order(
                order_id=int(row["order_id"]),
                instrument_id=int(row["instrument_id"]),
                conid=int(row["conid"]),
                direction=int(row["buy_sell"]),
                size=float(row["size_to_do"]),
                price=float(row["px_order"]),
                price_limit=float(row["px_order"]),  # combined orders use px_order as limit
                tick_size=float(row["tick_size"]),
                asset_class=ac,
                symbol=str(row.get("ticker", "")),
            )
            orders.append(order)

        print_banner(f"Executing {len(orders)} orders for account {account_id}")

        # Connect and execute
        port = IBConnection.get_port(account_id, cfg.this_computer)
        client_id = IBConnection.get_client_id(ctx.script_name, db, cfg.data_dir)

        executor = OrderExecutor(
            host="127.0.0.1",
            port=port,
            client_id=client_id,
        )

        if not executor.connect():
            print_banner(f"FAILED to connect for account {account_id}")
            continue

        try:
            results = executor.execute_batch(orders)
        finally:
            executor.disconnect()

        # Summary
        filled = sum(1 for o in results if o.status == OrderStatus.FILLED)
        partial = sum(1 for o in results if o.status == OrderStatus.PARTIAL)
        cancelled = sum(1 for o in results if o.status == OrderStatus.CANCELLED)
        errors = sum(1 for o in results if o.status == OrderStatus.ERROR)

        print_banner(
            f"Account {account_id}: {filled} filled, {partial} partial, "
            f"{cancelled} cancelled, {errors} errors"
        )


if __name__ == "__main__":
    run_script("Execute_Orders", main_fn=main, max_time_hours=0.5)
