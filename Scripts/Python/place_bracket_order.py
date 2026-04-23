#!/usr/bin/env python3
"""
Place a bracket order (entry LMT + target LMT GTC + stop STP GTC) via IB Gateway.

Called from R via B.sendOrder(). All instrument metadata is passed as CLI args.
Outputs JSON result to stdout.

Usage:
    python place_bracket_order.py \
        --pair EUR.USD --action BUY --size 25000 \
        --entry_price 1.08500 --target_price 1.09000 --stop_price 1.08000 \
        --account_id 1 --conid 42 --tick_size 0.00005 --asset_class fx_dm
"""

import argparse
import json
import logging
import sys

from ib_insync import IB, Forex, Future, Stock, Contract

# Log to stderr so stdout is clean for JSON
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(message)s",
    stream=sys.stderr,
)
logger = logging.getLogger(__name__)

CLIENT_ID = 25
HOST = "127.0.0.1"


def parse_args():
    parser = argparse.ArgumentParser(description="Place bracket order on IB Gateway")
    parser.add_argument("--pair", required=True, help="Instrument pair e.g. EUR.USD")
    parser.add_argument("--action", required=True, choices=["BUY", "SELL"])
    parser.add_argument("--size", required=True, type=float)
    parser.add_argument("--entry_price", required=True, type=float)
    parser.add_argument("--target_price", required=True, type=float)
    parser.add_argument("--stop_price", required=True, type=float)
    parser.add_argument("--account_id", required=True, type=int)
    parser.add_argument("--conid", required=True, type=int)
    parser.add_argument("--tick_size", required=True, type=float)
    parser.add_argument("--asset_class", required=True, type=str)
    return parser.parse_args()


def round_to_tick(price: float, tick_size: float) -> float:
    return round(round(price / tick_size) * tick_size, 10)


def build_contract(ib: IB, pair: str, conid: int, asset_class: str):
    if asset_class in ("fx_dm", "fx_em"):
        contract = Forex(pair.replace(".", ""))
    elif asset_class in ("index", "bond", "metal"):
        contract = Future(conId=conid)
    else:
        contract = Stock(conId=conid)

    qualified = ib.qualifyContracts(contract)
    if not qualified:
        raise ValueError(f"Failed to qualify contract for {pair} (conid={conid}, asset_class={asset_class})")

    logger.info(f"Contract qualified: {contract}")
    return contract


def place_bracket_order(ib: IB, contract, args):
    entry_price = round_to_tick(args.entry_price, args.tick_size)
    target_price = round_to_tick(args.target_price, args.tick_size)
    stop_price = round_to_tick(args.stop_price, args.tick_size)

    bracket = ib.bracketOrder(
        action=args.action,
        quantity=args.size,
        limitPrice=entry_price,
        takeProfitPrice=target_price,
        stopLossPrice=stop_price,
    )
    parent, take_profit, stop_loss = bracket

    # Set GTC on exit orders
    take_profit.tif = "GTC"
    stop_loss.tif = "GTC"

    # Place all three orders
    trades = []
    for order in bracket:
        trade = ib.placeOrder(contract, order)
        trades.append(trade)
        logger.info(f"Placed order: {order.orderType} {order.action} {order.totalQuantity} @ {getattr(order, 'lmtPrice', None) or getattr(order, 'auxPrice', None)}")

    # Wait for confirmation
    ib.sleep(3)

    parent_trade, tp_trade, sl_trade = trades

    return {
        "parent_order_id": parent_trade.order.orderId,
        "target_order_id": tp_trade.order.orderId,
        "stop_order_id": sl_trade.order.orderId,
        "parent_status": parent_trade.orderStatus.status,
        "target_status": tp_trade.orderStatus.status,
        "stop_status": sl_trade.orderStatus.status,
        "entry_price": entry_price,
        "target_price": target_price,
        "stop_price": stop_price,
    }


def main():
    args = parse_args()
    port = 7496 + args.account_id - 1

    result = {
        "status": "ERROR",
        "pair": args.pair,
        "action": args.action,
        "size": args.size,
        "account_id": args.account_id,
        "parent_order_id": None,
        "target_order_id": None,
        "stop_order_id": None,
        "parent_status": None,
        "target_status": None,
        "stop_status": None,
        "entry_price": args.entry_price,
        "target_price": args.target_price,
        "stop_price": args.stop_price,
        "message": "",
    }

    ib = IB()
    try:
        ib.connect(HOST, port, clientId=CLIENT_ID)
        logger.info(f"Connected to IB Gateway at {HOST}:{port}")

        contract = build_contract(ib, args.pair, args.conid, args.asset_class)
        order_result = place_bracket_order(ib, contract, args)

        result.update(order_result)
        result["status"] = "OK"
        result["message"] = "Bracket order placed successfully"

    except Exception as e:
        result["message"] = str(e)
        logger.error(f"Error: {e}")

    finally:
        if ib.isConnected():
            ib.disconnect()
            logger.info("Disconnected from IB Gateway")

    print(json.dumps(result))


if __name__ == "__main__":
    main()
