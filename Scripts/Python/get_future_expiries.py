#!/usr/bin/env python3
"""Get Future Expiries — fetches contract details for all futures from IB."""

import sys
sys.path.insert(0, "/home/fls/Models/Ventura/HD/Code/Python")

import pandas as pd
from datetime import datetime

from ventura.script_runner import run_script, ScriptContext
from ventura.ib.connection import IBConnection
from ventura.utils import print_banner

from ib_insync import Contract


def main(ctx: ScriptContext) -> None:
    db = ctx.db
    cfg = ctx.config

    # Load reference tables
    futures = db.load_table_local("future_contract", cfg.data_dir)
    currencies = cfg.tables.currencies
    exchanges = db.load_table_local("exchange", cfg.data_dir)

    # Build lookup
    dat = futures.merge(currencies, on="ccy_id", how="left")
    dat = dat.merge(exchanges, on="exchange_id", how="left")

    print_banner(f"Fetching expiries for {len(dat)} future contracts")

    # Connect to IB
    port = IBConnection.get_port(2, cfg.this_computer)
    client_id = IBConnection.get_client_id(ctx.script_name, db, cfg.data_dir)

    with IBConnection(port=port, client_id=client_id, readonly=True) as conn:
        ib = conn.ib

        all_rows = []
        for _, row in dat.iterrows():
            contract = Contract()
            contract.symbol = row["ib_symbol"]
            contract.secType = "FUT"
            contract.exchange = row["ib_exchange"]
            contract.currency = row["ccy"]

            details_list = ib.reqContractDetails(contract)

            if not details_list:
                print(f"  No contracts found for {row['ib_symbol']}")
                continue

            expected_notional = row["notional"]

            for details in details_list:
                c = details.contract
                expiry = datetime.strptime(
                    c.lastTradeDateOrContractMonth, "%Y%m%d"
                )
                notional = float(c.multiplier)

                # Only keep contracts with matching notional
                if notional != expected_notional:
                    continue

                all_rows.append({
                    "future_id": int(row["future_id"]),
                    "conid": c.conId,
                    "expiry": expiry,
                    "notional": notional,
                })

            print(f"  {row['ib_symbol']}: {len([r for r in all_rows if r['future_id'] == row['future_id']])} contracts")

    result = pd.DataFrame(all_rows)

    if result.empty:
        print_banner("No future expiries found")
        return

    # Save CSV
    out_path = cfg.data_dir + "Orders/future_expiries.csv"
    result.to_csv(out_path, index=False)
    print_banner(f"Saved {len(result)} expiries to {out_path}")


if __name__ == "__main__":
    run_script("Future_Expiry", main_fn=main, max_time_hours=0.25)
