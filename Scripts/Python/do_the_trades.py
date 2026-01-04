#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug  7 15:23:29 2020

@author: fls
"""

####################################################################################################
### Description: Execution strategy
####################################################################################################
### The problem with doing this in R is we don't have feedback on the order status: 
### transmitted/rejected, filled ?
### So we proceed by placing orders, moving the price away, canceling them, 
### and checking the book position.
### Once an order is filled it is automatically cancelled so it doesn't matter what you do with it.
### If we have a partial fill, IB works by keeping the size constant and 
### changing the price until it is filled completely.
###
### Place the orders
### Wait a few seconds
### Move the orders away by 4% to breathe and compute what has happened
### Wait a few seconds to make sure the IB position has been updated
### Check whether they have been filled by looking at our position
### Improve price
### Repeat for something like a minute
####################################################################################################
script_name = "Do_The_Trades"
####################################################################################################
### Import Libraries
####################################################################################################
from datetime import datetime, timedelta
import time
import numpy as np
import pandas as pd

from threading import Timer

from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract
from ibapi.ticktype import TickTypeEnum
from ibapi.order import Order

import ib_routines as vi
import execution_utils as vx
import utils as vu
import init

####################################################################################################
### Initialization
####################################################################################################
init.scriptInit(script_name)
init.initializeApplicationVariables()

####################################################################################################
### Script variables and parameters
####################################################################################################
EXECUTION_TIME_LIMIT_PER_LOOP_MINUTES = 2

END_TIME = init.start_time + timedelta(minutes = EXECUTION_TIME_LIMIT_PER_LOOP_MINUTES)

TIME_SLEEP_SECONDS = 2.5

MOVE_AWAY_ORDER_PCT = pd.DataFrame({
    "asset_class":["fx_dm","fx_em","index","metal"],
    "move_away_pct":[0.01,0.01,0.04,0.04]
    })

all_order_ids = None

IB_CLIENT_ID = vx.getIBClientId(script_name)
PATH_EXECUTION = init.DIRECTORY_DATA + "Execution/"

EXECUTION_TIME_ID = vx.findExecutionTime()

####################################################################################################
### Classes
####################################################################################################
class IBAppOrders(EWrapper, EClient):
    def __init__(self, account_id, order_list):
        EClient.__init__(self, self)
        self.started = False
        self.continue_with_loop = True
        self.iteration = 0
        self.account_id = account_id
        self.oda = order_list
        self.nb_orders = len(order_list)
        self.nextOrderId = None
                
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson):
        print("Error ", reqId, " ", errorCode, " ", errorString)
        if errorCode == 502:
            print("Error - Not connected - exiting")
            self.stop()
    
    def nextValidId(self, orderId):
        self.nextOrderId = orderId
        if not self.started:
            self.started = True
            self.initializeOrderList()
            self.start()
    
    def orderStatus(self, orderId, status, filled, remaining, 
                    avgFillPrice, permId, parentId, lastFillPrice, clientId, 
                    whyHeld, mktCapPrice):
        print("Order Status: Id ", orderId, " Status ", status, " Filled ", 
              filled, "Remaining ", remaining, " last fill price ", lastFillPrice)
        pos_id = (self.oda["order_id"] == orderId)
        
        self.oda["filled"][pos_id] = filled
        self.oda["remaining"][pos_id] = remaining
        self.oda["ib_status"][pos_id] = status
        
        if (remaining > 0) & (self.oda["size"] > remaining):
            self.oda["status"][pos_id] = "Working - some filled"
        if (remaining <= 0) or (status == "Filled"):
            self.oda["status"][pos_id] = "Filled"
        if (status == "Inactive"):
            self.oda["status"][pos_id] = "Rejected"
            
        if self.oda["status"][pos_id] == "Filled":
            self.cancelOrder(orderId)

    def initializeOrderList(self):
        self.oda["order_id"] = list(range(self.nextOrderId, self.nextOrderId + self.nb_orders))
        self.oda["status"] = "Not yet started"
        self.oda["ib_status"] = "Not yet started"
        vu.printBanner("Initialized these these orders...")
        print(self.oda)

    def readThisOrderDetails(self, i):
        vu.printBanner("Order Description:")
        print(self.oda.loc[[i]])
        return self.oda.loc[[i]]

    @vu.trySimpleNone()
    def placeThisOrderAndGetId(self, ib_order_id, ib_contract, ib_order):
        self.placeOrder(ib_order_id, ib_contract, ib_order)

    @vu.trySimpleNone()
    def placeThisOrder(self, i):
        this_order_details = self.readThisOrderDetails(i)
        ib_contract = self.oda["contract"].values[i]
        # is_good_order = testIfOrderIsGood(this_order_details, ib_contract)
        is_good_order = True
        if is_good_order:
            order_id = self.oda["order_id"].values[i]
            ib_order = prepareIBOrder(order_id, this_order_details)
            self.oda["status"].values[i] = "Placing now"
            self.placeOrder(order_id, ib_contract, ib_order)
            self.oda["status"].values[i] = "Placed, waiting"

    @vu.tryDiagnosticDat()
    def placeTheOrders(self):
        for i in range(self.nb_orders):
            if (self.oda["status"].values[i] != "Filled"):
                print("This is where we would actually place this order:")
                print(self.oda.iloc[i])
                #  self.placeThisOrder(i)
    
    def moveAwayOrders(self):
        vu.printBanner("Will now move the orders away from this level:")
        print(self.oda)
        self.oda = self.oda.assign(px_order = self.oda["px_order"] + self.oda["px_move_away"])
        self.placeTheOrders()
        vu.printBanner("The orders have now been moved away")

    def testPriceBeyondLimit(self, i):
        buy_sell = self.oda["buy_sell"].values[i]
        px_order = self.oda["px_order"].values[i]
        px_limit = self.oda["px_limit"].values[i]
        test_limit = ((buy_sell * (px_order - px_limit)) > 0)
        return test_limit
    
    def limitOrderPrices(self):
        for i in range(self.nb_orders):
            if self.testPriceBeyondLimit(i):
                self.oda["px_order"].values[i] = self.oda["px_limit"].values[i]

    def moveBackAndIncrementOrderPrices(self):  
        vu.printBanner("Will now move back the prices and increment:")
        print(self.oda)
        
        self.oda = self.oda.assign(
            px_order = self.oda["px_order"] 
                - self.oda["px_move_away"]
                + self.oda["buy_sell"] * self.oda["tick_size"]
            )
        self.limitOrderPrices()
        vu.printBanner("Prices incremented:")
        print(self.oda)
    
    @vu.tryDiagnosticNone()
    def moveAwayOrdersThenCancelAll(self):
        vu.printBanner("Last for this round: Move away all remaining orders, then cancel")
        self.moveAwayOrders()
        for i in range(self.nb_orders):
            order_id = self.oda["order_id"].values[i]
            self.cancelOrder(order_id)
    
    def shouldWeContinue(self):
        have_orders_left = False
        for i in range(self.nb_orders):
            if self.oda["status"].values[i] != "Filled":
                have_orders_left = True
        time_not_yet_up = (datetime.now() <= END_TIME)
        self.continue_with_loop = (time_not_yet_up and have_orders_left)

    def writeExecutionData(self, file_name):
        file_path_name = "{}{}_{}_{}_{}.csv".format(
                vx.executionDataDirectory(), 
                self.account_id,
                EXECUTION_TIME_ID,
                file_name,
                vu.dateTimeFormatForExport()
            )
        pd.to_csv(self.oda, file_path_name)
        
    def writeExecutionDataSubmission(self):
        vu.printBanner("New iteration - going to do this:")
        self.writeExecutionData("execution_loop_submission")
    
    def writeExecutionDataResult(self):
        self.writeExecutionData("execution_loop_result")

    def doTrades(self):
        self.iteration += 1
        if self.continue_with_loop:
            self.step1()
        else:
            self.moveAwayOrdersThenCancelAll()            

    def step1(self):
        self.writeExecutionDataSubmission()
        self.placeTheOrders()
        Timer(TIME_SLEEP_SECONDS, self.step2).start()
        
    def step2(self):
        self.moveAwayOrders()
        Timer(TIME_SLEEP_SECONDS, self.step3).start()
    
    def step3(self):
        self.writeExecutionDataResult()
        self.moveBackAndIncrementOrderPrices()
        Timer(TIME_SLEEP_SECONDS, self.step4).start()
    
    def step4(self):
        self.shouldWeContinue()
        self.doTrades()          
        
    def start(self):
        self.doTrades()

    def stop(self):
        self.cancelAllOrders()
        self.disconnect()

    
####################################################################################################
### Sub routines - Place Orders
####################################################################################################

def testIfOrderIsGood(this_order_details):
    bad_order_test = [False, False, False]
    bad_order_test[0] = (
        (this_order_details["buy_sell_action"].values[0] is None) or 
            (this_order_details["remaining"].values[0] is np.NaN) or 
            (this_order_details["remaining"].values[0] < 0) or 
            (this_order_details["size"].values[0] is np.NaN) or 
            (this_order_details["contract"].values[0] is None) or 
            (this_order_details["px_order"].values[0] is np.NaN)
        )
    if not bad_order_test[0]:
        #if (ib_contract$sectype == "STK")
        #    bad_order_test[1] <- (this_order_details$remaining[0] <= 0);
        pass
    if (not bad_order_test[0]) and (not bad_order_test[1]):
        #if (ib_contract$sectype == "STK")
        #    bad_order_test[2] <- (this_order_details$size[1] <= 0);
        pass
    good_order = not bad_order_test[0]
    vu.printBanner(f"Is this order good: {good_order}")
    return good_order
       
def buildIBOrder(action, total_quantity, order_type, lmt_price, tif):
    order = Order()
    order.action = action
    order.totalQuantity = total_quantity
    order.orderType = order_type
    order.lmtPrice = lmt_price
    order.tif = tif
    return order
    
def prepareIBOrder(order_id, this_order_details):
    action = this_order_details["buy_sell_action"].values[0]
    total_quantity = this_order_details["size"].values[0]
    order_type = "LMT"
    lmt_price = this_order_details["px_order"].values[0]
    tif = "DAY"
    
    order = buildIBOrder(action, total_quantity, order_type, lmt_price, tif)
    if order_id is not None:
        order.orderId = order_id
    
    return order


####################################################################################################
### Sub routines
####################################################################################################
@vu.trySimpleDat()
def calcMoveAwayPriceIncrement(dat):
    dat = dat.merge(MOVE_AWAY_ORDER_PCT, on = "asset_class", how = "left")
    dat = dat.assign(
        px_move_away = -dat["move_away_pct"] * dat["buy_sell"] * dat["px_order"]
        )
    dat = dat.assign(
        px_move_away = vu.mround(dat["px_move_away"], dat["tick_size"])
        )
    dat = dat.drop("move_away_pct", axis = 1)
    return dat
    
def makeAPause():
    time.sleep(TIME_SLEEP_SECONDS)

    
####################################################################################################
### Main script
####################################################################################################
@vu.tryDiagnosticDat()
def doTheTrades(order_list, account_id):
    order_list = calcMoveAwayPriceIncrement(order_list)
#    order_list.to_csv(PATH_EXECUTION + "order_list_sample.csv")
    ib_app = IBAppOrders(account_id, order_list)
    ib_app.connect("127.0.0.1", vx.getIBPort(account_id), IB_CLIENT_ID)
    ib_app.run() 
    order_list = ib_app.oda
    return order_list

####################################################################################################
### Extra lines for order check in the IB class, putting at the end of the script in case need it later
####################################################################################################
    # def openOrder(self, orderId, contract, order, orderState):
    #     print("Open order. Id ", orderId, contract.symbol, contract.secType, contract.exchange,
    #           contract.currency,":", order.action, order.orderType, " ", orderState.status)

    # def execDetails(self, reqId, contract, execution):
    #     print("ExecDetails. ", reqId, contract.symbol, contract.secType, contract.currency,
    #           execution.execId, execution.orderId, execution.shares, execution.lastLiquidity)

