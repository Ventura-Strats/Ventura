#!/usr/bin/env python3
# -*- coding: utf-8 -*-
####################################################################################################
### Script description
####################################################################################################
### input is a dataframe of orders:
### - instrument_id + ticker for reference
### - conid
### - buy_sell = 1 or -1
### - size_to_do = size to transact (eg 25 contracts) #DO NOT RENAME AS 'size' BECAUSE DF ATTRIBUTE
### - order_px = target price of the order - 
### If this column is missing then we take our best estimate of live price.
### - limit_px = worst acceptable execution price. If this column is missing then we assume X% 
### off the live price (see parameters below).
###
### We proceed in loops because
### - Sometimes the price has moved too much and we need to adjust rather than chasing 
### tick by tick
####################################################################################################
script_name = "Execute_Orders"
max_time_hours = 0.25

####################################################################################################
### Imports
####################################################################################################
import init
import numpy as np
import pandas as pd
import db
from datetime import datetime
from datetime import timedelta
import time
import sys
import os

from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract
from ibapi.order import Order

from threading import Timer

import utils as vu
import execution_utils as vx
# import ib_routines as vi

# import prepare_execution as vp
# import do_the_trades as vd

####################################################################################################
### Script initialization
####################################################################################################
ACCOUNT_ID = int(sys.argv[2])
script_name = script_name + "_" + str(ACCOUNT_ID)
init.scriptInit(script_name, max_time_hours)

FUTURES = db.loadTableLocal("future_contract")
FUTURES_EXPIRY = db.loadTableLocal("future_expiry")

####################################################################################################
### Execution strategy parameters
####################################################################################################
EXECUTION_TIME_LIMIT_WHOLE_CYCLE_MINUTES = 20
WAIT_AT_CYCLE_END_SECONDS = 0

EXECUTION_ENDING_TIME = init.start_time \
    + timedelta(minutes = EXECUTION_TIME_LIMIT_WHOLE_CYCLE_MINUTES)

####################################################################################################
### Execution strategy parameters - per round
####################################################################################################
LIMIT_EXECUTION_PRICE_FROM_INITIAL_MID_PCT = 0.0025
INITIAL_ORDER_TICKS_FROM_MID = 2
TIME_THRESHOLD_DB_PX_VS_NOW = 100
TIME_LIMIT_FOR_PX = datetime.now() - timedelta(minutes = TIME_THRESHOLD_DB_PX_VS_NOW)
TIME_LIMIT_FOR_PX = TIME_LIMIT_FOR_PX.strftime("%Y-%m-%d %H:%M:%S")

####################################################################################################
### Do the trades parameters
####################################################################################################

EXECUTION_TIME_LIMIT_PER_LOOP_MINUTES = 2

END_TIME = init.start_time + timedelta(minutes = EXECUTION_TIME_LIMIT_PER_LOOP_MINUTES)

TIME_SLEEP_SECONDS = 2.5

MOVE_AWAY_ORDER_PCT = pd.DataFrame({
    "asset_class":["fx_dm","fx_em","index","metal"],
    "move_away_pct":[0.01,0.01,0.04,0.04]
    })

all_order_ids = None

IB_CLIENT_ID_EXECUTION = vx.getIBClientId(script_name)
PATH_EXECUTION = init.DIRECTORY_DATA + "Execution/"

PATH_ORDERS_COMBINED = init.DIRECTORY_DATA + "Orders/Combined/"
####################################################################################################
### Script constants
####################################################################################################
# PATH_ORDERS = init.DIRECTORY_DATA + "Orders/"
# PATH_FILE_ORDERS = f"{PATH_ORDERS}{init.TODAY.strftime('%Y-%m')}/{init.TODAY_STR}/"

####################################################################################################
### Script variables
####################################################################################################
EXECUTION_TIME_ID = vx.findExecutionTime()

####################################################################################################
### Classes
####################################################################################################
class IBAppBookPriceAndPositions(EWrapper, EClient):
    def __init__(self, account_id):
        EClient.__init__(self, self)
        self.px_position = self.prepareContractTable()
        self.account_id = account_id
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson):
        print("Error ", reqId, " ", errorCode, " ", errorString)
        if errorCode == 502:
            print("Error - Not connected - exiting")
            self.stop()
    
    def nextValidId(self, orderId):
        self.start()
               
    def updatePortfolio(self, contract, position, marketPrice, marketvalue, 
                        averageCost, unrealizedPNL, realizedPNL, accountName):
        dat = self.prepareContractTable([contract], [marketPrice], [position])
        self.px_position = self.px_position.append(dat)
        
    def accountDownloadEnd(self, account):
        self.finalFormatting()
        self.stop()
        
    def finalFormatting(self):
        self.px_position["account_id"] = self.account_id
        self.px_position["conid"] = self.px_position["contract"].map(lambda x: x.conId)
        self.px_position = self.px_position[["account_id", "conid", "price", "position"]]
        
    def prepareContractTable(self, contract = [], price = [], position = []):
        return pd.DataFrame({"contract":contract, "price":price, "position":position})

    def start(self):
        self.reqAccountUpdates(True, "")

    def stop(self):
        self.disconnect()

####################################################################################################
class PrepareOrderListThisRound():
    ### Prepare initial order list for this round
    ### For the first round it takes for input the result of the previous task:  
    ###    order_id, ib_order_id, account_id, instrument_id, ticker, asset_class, future_id,
    ###    conid, contract, tick_size, buy_sell, buy_sell_action, size_to_do,
    ###    px_order, px_live, px_avg,
    ###    initial_position, position, filled, remaining, 
    ###    status, ib_status
    ### And populates live info:
    ###    px_live, filled, status, initial_position, position, remaining, px_avg
    
    def __init__(self, order_list, account_id, i_round):
        self.account_id = account_id
        self.oda = order_list
        self.i_round = i_round
        
    def __call__(self):
        self.start()
        vu.printBanner(f"prepareOrderListThisRound for round {self.i_round} done. Result:")
        print(self.oda)
        return self.oda

    @vu.tryDiagnosticDat()
    def readLivePricesFromDB(self):
        sql_q = f"""SELECT conid, price as px_db 
            FROM live_px_exec 
            WHERE timestamp >= '{init.char_start_time}'"""
        dat_px = db.select(sql_q)
        self.oda = self.oda.merge(dat_px, on="conid", how="left")
        self.oda["px_live"] = self.oda["px_db"]
        self.oda = self.oda.drop("px_db", axis=1)
        vu.printBanner("getLivePricesSpot done, result:")
        print(self.oda)
    
    @vu.tryDiagnosticDat()
    def keepOnlyInstrumentsWithAPrice(self):
        self.oda.dropna(subset = ["px_live"], inplace=True)
        vu.printBanner("keepOnlyInstrumentsWithAPrice done, result:")
        print(self.oda)
    
    def roundPriceToClosestTick(self, px_col):
        self.oda[px_col] = vu.mround(self.oda[px_col], self.oda["tick_size"])
        vu.printBanner("roundPriceToClosestTick done, result:")
        print(self.oda)
        
    @vu.tryDiagnosticDat()
    def addPositions(self): 
        df_position = getPricePositions() \
            [["account_id", "conid", "position"]] \
            .assign(
                account_id = lambda x: list(map(int, x.account_id)),
                conid = lambda x: list(map(int, x.conid)),
                position = lambda x: list(map(float, x.position))
                )
            
        self.oda = self.oda \
            .drop("position", axis=1) \
            .merge(df_position, on=["account_id", "conid"], how="left")
 
        if self.i_round == 1:
            self.oda["initial_position"] = self.oda["position"]
            self.oda = self.oda.fillna({"initial_position":0})
        
        vu.printBanner("addPositions done, result:")
        print(self.oda)
 
    @vu.tryDiagnosticNone()
    def getLivePrices(self):
        self.oda["px_live"] = np.nan
        self.readLivePricesFromDB()
        self.keepOnlyInstrumentsWithAPrice()
        self.roundPriceToClosestTick("px_live")
        self.roundPriceToClosestTick("px_order")
        self.addPositions()
        vu.printBanner("getLivePrices done, result:")
        print(self.oda)
        
    @vu.tryDiagnosticNone()
    def moveInitialPriceAFewTicks(self, px_order_col):
        self.oda[px_order_col] = self.oda[px_order_col] \
            - INITIAL_ORDER_TICKS_FROM_MID \
            * self.oda.buy_sell * self.oda.tick_size
    
    @vu.tryDiagnosticNone()
    def setOrderPriceToLivePriceIfOrderPriceIsMissing(self):
        pos_na = self.oda.px_order.isnull()
        self.oda["px_order"][pos_na] = self.oda["px_live"][pos_na]
        vu.printBanner("setOrderPriceToLivePriceIfOrderPriceIsMissing done, result:")
        print(self.oda)
    
    @vu.tryDiagnosticNone()
    def setOrderPriceToLivePriceIfTheyAreCloseEnough(self):
        px_diff_pct = abs(self.oda.px_live / self.oda.px_order - 1)
        pos_close_enough = (px_diff_pct <= LIMIT_EXECUTION_PRICE_FROM_INITIAL_MID_PCT)
        self.oda["px_order"][pos_close_enough] = self.oda["px_live"][pos_close_enough]
        vu.printBanner("setOrderPriceToLivePriceIfTheyAreCloseEnough done, result:")
        print(self.oda)
    
    @vu.tryDiagnosticNone()
    def prepareInitialOrderPrice(self):
        self.setOrderPriceToLivePriceIfOrderPriceIsMissing()
        self.setOrderPriceToLivePriceIfTheyAreCloseEnough()
        self.moveInitialPriceAFewTicks("px_order")
        vu.printBanner("prepareInitialOrderPrice done, result:")
        print(self.oda)
    
    
    @vu.tryDiagnosticNone()    
    def prepareOrderPriceLimit(self):
        self.oda["px_limit"] = self.oda.px_order \
                * (1 + self.oda.buy_sell * LIMIT_EXECUTION_PRICE_FROM_INITIAL_MID_PCT)
        
        pos_beyond_limit = (
            (self.oda.buy_sell * self.oda.px_order) \
                > (self.oda.buy_sell * self.oda.px_limit)
            )
            
        self.oda["px_order"][pos_beyond_limit] = self.oda["px_limit"][pos_beyond_limit]
        vu.printBanner("prepareOrderPriceLimit done, result:")
        print(self.oda)
    
    @vu.trySimpleNone()   
    def roundOrderSize(self):
        # this is already done in Signal_List but doing it just to be sure
        # should remove later
        self.oda = self.oda.assign(size_to_do = round(self.oda["size_to_do"]-0.25, 0))
        vu.printBanner("roundOrderSize done, result:")
        print(self.oda)
        
    @vu.trySimpleNone()  
    def keepOnlyPositiveSizeOrders(self):
        self.oda = self.oda[self.oda.size_to_do > 0]
        vu.printBanner("keepOnlyPositiveSizeOrders done, result:")
        print(self.oda)
        
    @vu.trySimpleNone()   
    def removeNegativeRemainingSizeToDo(self):
        self.oda["remaining"][self.oda["remaining"] < 0] = 0
        vu.printBanner("removeNegativeRemainingSizeToDo done, result:")
        print(self.oda)
    
    @vu.trySimpleNone()    
    def removeDuplicatedInstrumentRows(self):
        self.oda.drop_duplicates(subset =["instrument_id"], keep=False, inplace=True)
        vu.printBanner("removeDuplicatedInstrumentRows done, result:")
        print(self.oda)
    
    @vu.tryDiagnosticNone()
    def finalFormatting(self):
        if "filled" not in self.oda.columns:
            self.oda["filled"] = 0
        if "status" not in self.oda.columns:
            self.oda["status"] = "not_yet_started"
    
        self.roundPriceToClosestTick("px_order")
        self.roundPriceToClosestTick("px_limit")
        self.oda["remaining"] = self.oda.size_to_do - self.oda.filled
        self.oda["px_avg"] = 0
        
        self.removeNegativeRemainingSizeToDo()   
        vu.printBanner("finalFormatting done, result:")
        print(self.oda)

    @vu.trySimpleNone()
    def start(self):
        self.getLivePrices()
        self.prepareInitialOrderPrice()
        self.prepareOrderPriceLimit()
        self.roundOrderSize()
        self.keepOnlyPositiveSizeOrders()
        self.finalFormatting()

####################################################################################################

class IBExecuteOrders(EWrapper, EClient):
    ### Actual execution of the trades
    ### Order list looks like this:
    ###    order_id, ib_order_id, account_id, instrument_id, ticker, asset_class, future_id,
    ###    conid, contract, tick_size, buy_sell, buy_sell_action, size_to_do,
    ###    px_order, px_live, px_avg,
    ###    initial_position, position, filled, remaining, 
    ###    status, ib_status
    
    def __init__(self, order_list, account_id):
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
        pos_id = (self.oda["ib_order_id"] == orderId)
        
        self.oda["filled"][pos_id] = filled
        self.oda["remaining"][pos_id] = remaining
        self.oda["ib_status"][pos_id] = status
        
        if (remaining > 0) & (self.oda["size_to_do"] > remaining):
            self.oda["status"][pos_id] = "Working - some filled"
        if (remaining <= 0) or (status == "Filled"):
            self.oda["status"][pos_id] = "Filled"
        if (status == "Inactive"):
            self.oda["status"][pos_id] = "Rejected"
            
        if self.oda["status"][pos_id] == "Filled":
            self.cancelOrder(orderId)
            
    @vu.tryDiagnosticNone()
    def initializeOrderList(self):
        self.oda["ib_order_id"] = list(range(self.nextOrderId, self.nextOrderId + self.nb_orders))
        self.oda["status"] = "Not yet started"
        self.oda["ib_status"] = "Not yet started"
        vu.printBanner("initializeOrderList done, result:")
        print(self.oda)

    @vu.trySimpleNone()
    def readThisOrderDetails(self, i):
        vu.printBanner(f"Order {i} Description:", False)
        print(self.oda.loc[[i]])
        return self.oda.loc[[i]]

    @vu.tryDiagnosticNone()
    def prepareIBOrder(self, ib_order_id, this_order_details):
        order = Order()
        order.action = this_order_details["buy_sell_action"].values[0]
        order.totalQuantity = this_order_details["size_to_do"].values[0]
        order.orderType = "LMT"
        order.lmtPrice = this_order_details["px_order"].values[0]
        order.tif = "DAY"

        if not np.isnan(ib_order_id):
            order.orderId = ib_order_id
        
        vu.printBanner(f"prepareIBOrder {ib_order_id} done, result:")
        print(order)

        return order

    @vu.trySimpleNone()
    def placeThisOrder(self, i):
        this_order_details = self.readThisOrderDetails(i)
        ib_contract = self.oda["contract"].values[i]
        # is_good_order = testIfOrderIsGood(this_order_details, ib_contract)
        is_good_order = True
        if is_good_order:
            ib_order_id = self.oda["ib_order_id"].values[i]
            ib_order = self.prepareIBOrder(ib_order_id, this_order_details)
            self.oda["status"].values[i] = "Placing this order now:"
            print(ib_order)
            self.placeOrder(ib_order_id, ib_contract, ib_order)
            self.oda["status"].values[i] = "Placed, waiting"
        vu.printBanner(f"placeThisOrder {i} done, result:")
        print(self.oda)

    @vu.tryDiagnosticDat()
    def placeTheOrders(self):
        for i in range(self.nb_orders):
            if (self.oda["status"].values[i] != "Filled"):
                print("This is where we would actually place this order:")
                print(self.oda.iloc[[i]])
                #  self.placeThisOrder(i)
                
    @vu.tryDiagnosticNone()
    def moveAwayOrders(self):
        vu.printBanner("Will now move the orders away from this level:")
        print(self.oda)
        self.oda = self.oda.assign(px_order = self.oda["px_order"] + self.oda["px_move_away"])
        self.placeTheOrders()
        vu.printBanner("The orders have now been moved away")

    @vu.tryDiagnosticNone()
    def testPriceBeyondLimit(self, i):
        buy_sell = self.oda["buy_sell"].values[i]
        px_order = self.oda["px_order"].values[i]
        px_limit = self.oda["px_limit"].values[i]
        test_limit = ((buy_sell * (px_order - px_limit)) > 0)
        return test_limit
    
    @vu.tryDiagnosticNone()
    def limitOrderPrices(self):
        for i in range(self.nb_orders):
            if self.testPriceBeyondLimit(i):
                self.oda["px_order"].values[i] = self.oda["px_limit"].values[i]

    @vu.tryDiagnosticNone()
    def moveBackAndIncrementOrderPrices(self):  
        vu.printBanner("Will now move back the prices and increment:")
        print(self.oda)
        
        self.oda["px_order"] = self.oda.px_order \
                - self.oda.px_move_away \
                + self.oda.buy_sell * self.oda.tick_size

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
    
    @vu.tryDiagnosticNone()    
    def shouldWeContinue(self):
        have_orders_left = False
        for i in range(self.nb_orders):
            if self.oda["status"].values[i] != "Filled":
                have_orders_left = True
        time_not_yet_up = (datetime.now() <= END_TIME)
        self.continue_with_loop = (time_not_yet_up and have_orders_left)

    @vu.tryDiagnosticNone()
    def writeExecutionData(self, file_name):
        file_path_name = "{}{}_{}_{}_{}.csv".format(
                vx.executionDataDirectory(), 
                self.account_id,
                EXECUTION_TIME_ID,
                file_name,
                vu.dateTimeFormatForExport()
            )
        self.oda.to_csv(file_path_name)
        
    @vu.tryDiagnosticNone()
    def writeExecutionDataSubmission(self):
        vu.printBanner("New iteration - going to do this:")
        self.writeExecutionData("execution_loop_submission")
    
    @vu.tryDiagnosticNone()
    def writeExecutionDataResult(self):
        self.writeExecutionData("execution_loop_result")
        
    @vu.tryDiagnosticNone()
    # From old R routine, should be changed or removed altogether
    def testIfOrderIsGood(self, this_order_details):
        bad_order_test = [False, False, False]
        bad_order_test[0] = (
            (this_order_details["buy_sell_action"].values[0] is None) or 
                (np.isnan(this_order_details["remaining"].values[0])) or 
                (this_order_details["remaining"].values[0] < 0) or 
                (np.isnan(this_order_details["size_to_do"].values[0])) or 
                (this_order_details["contract"].values[0] is None) or 
                (np.isnan(this_order_details["px_order"].values[0]))
            )
        if not bad_order_test[0]:
            #if (ib_contract$sectype == "STK")
            #    bad_order_test[1] <- (this_order_details$remaining[0] <= 0);
            pass
        if (not bad_order_test[0]) and (not bad_order_test[1]):
            #if (ib_contract$sectype == "STK")
            #    bad_order_test[2] <- (this_order_details$size_to_do[1] <= 0);
            pass
        good_order = not bad_order_test[0]
        vu.printBanner(f"Is this order good: {good_order}")
        return good_order

    @vu.trySimpleDat()
    def calcMoveAwayPriceIncrement(self):
        self.oda = self.oda.merge(MOVE_AWAY_ORDER_PCT, on = "asset_class", how = "left")
        self.oda["px_move_away"] = -self.oda.move_away_pct * self.oda.buy_sell * self.oda.px_order
        self.oda["px_move_away"] = vu.mround(self.oda.px_move_away, self.oda.tick_size)
        self.oda.drop("move_away_pct", axis=1, inplace=True)
        vu.printBanner("calcMoveAwayPriceIncrement done, result:")
        print(self.oda)
        
    def makeAPause(self):
        time.sleep(TIME_SLEEP_SECONDS)

    @vu.tryDiagnosticNone()
    def doTrades(self):
        self.calcMoveAwayPriceIncrement()
        #    order_list.to_csv(PATH_EXECUTION + "order_list_sample.csv")
        self.iteration += 1
        if self.continue_with_loop:
            self.step1()
        else:
            self.moveAwayOrdersThenCancelAll()            

    @vu.tryDiagnosticNone()
    def step1(self):
        self.writeExecutionDataSubmission()
        self.placeTheOrders()
        Timer(TIME_SLEEP_SECONDS, self.step2).start()
        
    @vu.tryDiagnosticNone()
    def step2(self):
        self.moveAwayOrders()
        Timer(TIME_SLEEP_SECONDS, self.step3).start()
    
    @vu.tryDiagnosticNone()    
    def step3(self):
        self.writeExecutionDataResult()
        self.moveBackAndIncrementOrderPrices()
        Timer(TIME_SLEEP_SECONDS, self.step4).start()
    
    @vu.tryDiagnosticNone()
    def step4(self):
        self.shouldWeContinue()
        self.doTrades()          
    
    def start(self):
        self.doTrades()

    def stop(self):
        self.cancelAllOrders()
        self.disconnect()

####################################################################################################
### Sub Routines
####################################################################################################
def csvFileWrite(data_table, file_name):
    file_path_name = vx.executionDirectory() + file_name + ".csv"
    data_table.to_csv(file_path_name)
            
def writeLoopResults(i, account_id, orders_this_loop, orders_filled, orders_to_do, orders_result):
    def nameFile(file_desc):
        return f"E{account_id}_{EXECUTION_TIME_ID}_{file_desc}_round_{i}"
        
    csvFileWrite(orders_this_loop, nameFile("executions_summary"))
    csvFileWrite(orders_filled, nameFile("orders_filled"))
    csvFileWrite(orders_to_do, nameFile("orders_remaining"))
    csvFileWrite(orders_result, nameFile("orders_res"))
    
def remarkPriceAsAverageOfTwoBooks(dat):
    px = dat.groupby("conid", as_index=False)["price"].mean()
    return dat \
        .drop("price", axis = 1) \
        .merge(px, on = "conid", how = "left")

def combineInformationFromBothBooks(px_position_1, px_position_2):
    dat = pd.concat([px_position_1, px_position_2])
    return remarkPriceAsAverageOfTwoBooks(dat)
   
def getBookLivePriceAndPositions(account_id):
    ib_app = IBAppBookPriceAndPositions(account_id)
    ib_app.connect("127.0.0.1", vx.getIBPort(account_id), 157)
    ib_app.run()    
    return ib_app.px_position

def getPricePositions():
    px_position_1 = getBookLivePriceAndPositions(1)
    px_position_2 = getBookLivePriceAndPositions(2)
    px_position = combineInformationFromBothBooks(px_position_1, px_position_2)
    return px_position
            
####################################################################################################
### Sub Routines remaining
####################################################################################################  
@vu.tryDiagnosticDat()    
def formatFilledResults(order_list):
    return order_list \
        [["account_id", "pair", "size_to_do", "filled"]] \
        .rename({"filled":"new_filled"}, axis=1)

@vu.tryDiagnosticDat()    
def formatOrdersResults(order_list, filled_orders):
    order_list = order_list.merge(
        filled_orders, 
        on = ["account_id","pair","size_to_do"], 
        how = "left"
        )
    if "new_filled" not in order_list.columns:
        order_list["new_filled"] = 0
        
    return order_list \
        .assign(
            filled = order_list["filled"] + order_list["new_filled"]
            ) \
        .drop("new_filled", axis=1)

@vu.tryDiagnosticDat()    
def prepareOrdersToDo(orders_result, i):
    df = orders_result
    df = df[df["status"] != "Filled"]
    return df \
        .assign(size_to_do = df["size_to_do"] - df["filled"]) \
        [df["size_to_do"] > 0] \
        [["account_id", "ticker", "buy_sell", "buy_sell_action", "size_to_do",
               "px_order", "px_limit", "conid", "tick_size", "filled"]]

def testKeepGoing(all_done):
    return ((not all_done) and (datetime.now() < EXECUTION_ENDING_TIME))
    
def printDoingRound(i, orders_to_do):
    vu.printBanner(f"Doing Round {i} - Orders before preparation:")
    print(orders_to_do)
    
####################################################################################################
### Sub routines
####################################################################################################
@vu.tryDiagnosticNone()
def readCombinedOrders(account_id):
    file_path = "{}{}/{}/".format(
        PATH_ORDERS_COMBINED, 
        init.TODAY.strftime("%Y-%m"),
        init.TODAY.strftime("%Y-%m-%d")
        )
    
    file_name = "combined_orders_{}_{}-{}.csv".format(
        str(account_id),
        init.TODAY.strftime("%Y%m%d"), 
        ("0" + str(EXECUTION_TIME_ID))[-2:]
        )

    try:
        order_list = pd.read_csv(file_path + file_name)
        #order_list = pd.read_csv("/home/fls/Mount/Glenorchy/FX/Orders/Combined/test_order.csv")
    except:
        order_list = pd.DataFrame()
    finally:
        vu.printBanner("INITIAL ORDER LIST:")
        print(order_list)
        return order_list

def prepareOrderListThisRound(orders_to_do, round_i):
    vu.printBanner("PrepareOrderListThisRound - Beginning:")
    orders_this_loop = PrepareOrderListThisRound(orders_to_do, ACCOUNT_ID, round_i)()
    vu.printBanner("PrepareOrderListThisRound - DONE: Result:")
    print(orders_this_loop)
    return orders_this_loop

def doTheTrades(orders_to_do, account_id):
    vu.printBanner("doTheTrades - Beginning:")
    ib_app = IBExecuteOrders(orders_to_do, account_id)
    ib_app.connect("127.0.0.1", vx.getIBPort(account_id), vx.getIBClientId("Do_The_Trades"))
    ib_app.run()
    vu.printBanner("doTheTrades - DONE: Result:")
    print(ib_app.oda)
    return ib_app.oda

def doWeHaveOrders(order_list):
    keep_going = (len(order_list) > 0)    
    if not keep_going:
        vu.printBanner("Nothing to do")
    return keep_going

def wrapUpLoop(orders_this_loop, orders_filled, orders_to_do, orders_result, round_i):
    writeLoopResults(
        round_i, ACCOUNT_ID, orders_this_loop, orders_filled, orders_to_do, orders_result
        )

    all_done = (len(orders_to_do) == 0)
    vu.printBanner(f"Are we all done ? {all_done}")
    keep_going = testKeepGoing(all_done)
    vu.printBanner(f"Do we keep going ? {keep_going}")
    vu.printBanner(f"Round {round_i} done. Trades left to do:")
    print(orders_to_do)

    time.sleep(WAIT_AT_CYCLE_END_SECONDS)
    keep_going = False
    return keep_going

def waitAFewSecondsToHaveTheLivePricesComeIn():
    time.sleep(10)

####################################################################################################
### Script
#################################################################################################### 
if __name__ == "__main__":
    vx.waitTillPreviousJobHasFinished("Signal_List", 1, 1, 5, 5)
    waitAFewSecondsToHaveTheLivePricesComeIn()
    round_i = 1  
    orders_result = orders_to_do = order_list = readCombinedOrders(ACCOUNT_ID)
        
    keep_going = doWeHaveOrders(order_list)
        
    while keep_going:        
        printDoingRound(round_i, orders_to_do)
        orders_this_loop = prepareOrderListThisRound(orders_to_do, round_i)
        orders_this_loop = doTheTrades(orders_this_loop, ACCOUNT_ID)()

        orders_filled = formatFilledResults(orders_this_loop)
        orders_result = formatOrdersResults(orders_result, orders_filled)
        orders_to_do = prepareOrdersToDo(orders_result, round_i)

        keep_going = wrapUpLoop(
            orders_this_loop, orders_filled, orders_to_do, orders_result, round_i
            )
        round_i += 1
    
    vu.printBanner("Execution routine finished:")
    print(orders_result)
    init.scriptFinish(script_name)