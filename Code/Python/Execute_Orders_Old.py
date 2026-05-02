#!/usr/bin/env python3
# -*- coding: utf-8 -*-
####################################################################################################
### Script description
####################################################################################################
### input is a dataframe of orders:
### - instrument_id + ticker for reference
### - conid
### - buy_sell = 1 or -1
### - size = size to transact (eg 25 contracts)
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

####################################################################################################
### Execution strategy parameters
####################################################################################################
EXECUTION_TIME_LIMIT_WHOLE_CYCLE_MINUTES = 20
WAIT_AT_CYCLE_END_SECONDS = 0

EXECUTION_ENDING_TIME = init.start_time + timedelta(
        minutes = EXECUTION_TIME_LIMIT_WHOLE_CYCLE_MINUTES
        )

FUTURES = db.loadTableLocal("future_contract")
FUTURES_EXPIRY = db.loadTableLocal("future_expiry")


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
PATH_ORDERS = init.DIRECTORY_DATA + "Orders/"
PATH_FILE_ORDERS = f"{PATH_ORDERS}{init.TODAY.strftime('%Y-%m')}/{init.TODAY_STR}/"

####################################################################################################
### Script variables
####################################################################################################
EXECUTION_TIME_ID = vx.findExecutionTime()

####################################################################################################
### Classes
####################################################################################################

class IBContracts(EWrapper, EClient):
    # Get conid for a simple FX contract
    def __init__(self, ticker_list):
        EClient.__init__(self, self)
        self.ticker_list = ticker_list
        self.nb_items = len(self.ticker_list)
        self.conid_list = [[] for i in range(self.nb_items)]
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson):
        print("Error ", reqId, " ", errorCode, " ", errorString)
        if errorCode == 502:
            print("Error - Not connected - exiting")
            self.stop()
    
    def nextValidId(self, orderId):
        self.start()
    
    def contractDetails(self, reqId, contractDetails):
        self.conid_list[reqId].append(contractDetails)
        
    def contractDetailsEnd(self, reqId):
        self.conid_list[reqId] = self.conid_list[reqId][0]
        if self.conid_list[reqId] == []:
            self.conid_list[reqId] = "Not found"
        else:        
            try:
                self.conid_list[reqId] = self.conid_list[reqId].contract.conId
            except:
                pass
        self.wrapUpIfAllComplete(reqId)
        
    def checkIfAllDone(self):
        is_completed = True
        for conid in self.conid_list:
            is_completed = is_completed and (conid != []) 
        return is_completed
    
    def wrapUpIfAllComplete(self, reqId):
        if self.checkIfAllDone():
            self.stop()
            
    def buildQuickFXContract(self, fx_pair):
        contract = Contract()
        contract.symbol = fx_pair[:3]
        contract.secType = "CASH"
        contract.exchange = "IDEALPRO"
        contract.currency = fx_pair[-3:]
        return contract
            
    def start(self):
        for i in range(self.nb_items):
            if self.ticker_list[i] is None:
                self.conid_list[i] = [None]
            else:
                contract = self.buildQuickFXContract(self.ticker_list[i])
                self.reqContractDetails(i, contract)

    def stop(self):
        self.disconnect()

class IBAppRetrieveContracts(EWrapper, EClient):
    ### Get contract details given a list of conid
    def __init__(self, conid_list):
        EClient.__init__(self, self)
        self.conid_list = conid_list
        self.nb_items = len(self.conid_list)
        self.contract_details = [[] for i in range(self.nb_items)]
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson):
        print("Error ", reqId, " ", errorCode, " ", errorString)
        if errorCode == 502:
            print("Error - Not connected - exiting")
            self.stop()
    
    def nextValidId(self, orderId):
        self.start()
    
    def contractDetails(self, reqId, contractDetails):
        self.contract_details[reqId].append(contractDetails)
        
    def contractDetailsEnd(self, reqId):
        self.contract_details[reqId] = self.contract_details[reqId][0]
        if self.contract_details[reqId] == []:
            self.contract_details[reqId] = "Not found"
        else:        
            try:
                self.contract_details[reqId] = self.contract_details[reqId].contract
            except:
                pass
        self.wrapUpIfAllComplete(reqId)
        
    def checkIfAllDone(self):
        is_completed = True
        for contract_detail in self.contract_details:
            is_completed = is_completed and (contract_detail != []) 
        return is_completed
    
    def wrapUpIfAllComplete(self, reqId):
        if self.checkIfAllDone():
            self.stop()
        
    def start(self):
        for i in range(self.nb_items):
            if self.conid_list[i] is None:
                self.contract_details[i] = [None]
            else:
                contract = Contract()
                contract.conId = int(self.conid_list[i])
                self.reqContractDetails(i, contract)

    def stop(self):
        self.disconnect()
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

class PrepareInitialOrderList():
    ### Prepare initial order list by gather new and exit trades
    ### We take data that looks like this:
    ### - New: From file: 
    ###     account_id, strategy_id, instrument_id, ticker, future_id, 
    ###     conid, buy_sell, size, px_order
    ### - Exit: from DB
    ### We output a combined table that looks like this:
    ### account_id, pair, asset_class, 
    ### buy_sell, size, px_order, buy_sell_action, 
    ### future_id, instrument_id, tick_size, conid, contract, 
    ### initial_position, position, filled, remaining, px_live, px_avg, 
    ### order_id, ib_order_id, status, ib_status

    def __init__(self, account_id):
        self.execution_time_id = EXECUTION_TIME_ID
        self.account_id = account_id
        self.orders_new = pd.DataFrame()
        self.orders_exit = pd.DataFrame()
        self.orders = pd.DataFrame()
    
    def __call__(self):
        self.prepareDetailedOrderList()

    @vu.tryDiagnosticNone()
    def formatOrdersNew(self):
        self.orders_new = self.orders_new \
            .assign(
                conid = self.orders_new.conid.map(int),
                new_exit = "N"
                ) \
            .merge(init.INSTRUMENTS[["instrument_id", "pair"]], on="instrument_id", how="left") \
            [["new_exit", "account_id", "strategy_id", "instrument_id", "pair", 
              "future_id", "conid", "buy_sell", "size", "px_order"]]

    @vu.tryDiagnosticNone()
    def readOrdersNewFromFile(self):
        file_name = "orders_{}_{}-{}.csv".format(
            str(self.account_id),
            init.TODAY.strftime("%Y%m%d"), 
            ("0" + str(self.execution_time_id))[-2:]
            )
        file_path_name = PATH_FILE_ORDERS + file_name
        self.orders_new = pd.read_csv(file_path_name)
    
    @vu.tryDiagnosticNone()
    def prepareOrdersNew(self):
        # input format:
        # account_id, strategy_id, instrument_id, ticker, future_id, conid, buy_sell, size, px_order
        # output format:
        # new_exit, account_id, strategy_id, instrument_id, pair, future_id, 
        #   conid, buy_sell, size, px_order
        self.readOrdersNewFromFile()
        self.formatOrdersNew()

    @vu.tryDiagnosticNone()
    def loadExitTradesForTodayFromDB(self):
        #date_entry: TO DO PROPERLY WITH CALENDARS
        date_entry = (init.TODAY - timedelta(days = 7)).strftime("%Y-%m-%d") 
        
        sql_q = f"""SELECT T.trade_id, L.account_id, T.strategy_id, L.identifier, T.date_entry, 
            AVG(L.buy_sell) AS buy_sell, SUM(L.price * L.size) / SUM(L.size) AS price, 
            SUM(L.size) as size
            FROM book_trade T
            LEFT JOIN book_trade_map M ON M.trade_id = T.trade_id
            LEFT JOIN book_trade_leg L ON L.leg_id = M.leg_id
            WHERE T.strategy_id != 0
            AND T.trade_outcome_id = 0
            AND T.date_exit IS NULL
            AND T.date_entry = '{date_entry}'
            AND M.trade_category_id = 1
            AND L.account_id = {self.account_id}
            GROUP BY T.trade_id, T.strategy_id, L.identifier"""
        self.orders_exit = db.select(sql_q)

    @vu.tryDiagnosticNone() 
    def retrieveConIdForFXInstruments(self, df):
        missing_instruments = df[df["asset_class"].isin(["fx_dm","fx_em"])]
        missing_instruments["conid"] = np.nan
        pair_list = list(missing_instruments["ticker"])
        if len(pair_list) > 0:
            ib_app = IBContracts(pair_list)
            ib_app.connect("127.0.0.1", vx.getIBPort(2), 111)
            ib_app.run()
            for i in range(len(df)):
                if df["ticker"][i] == pair_list[i]:
                    df["conid"][i] = ib_app.conid_list[i]
        return df[["instrument_id", "conid"]]

    @vu.tryDiagnosticNone()
    def instrumentListForIdentifier(self, df_identifier):
        df_identifier = df_identifier \
            .assign(instrument_id = df_identifier["identifier"]) \
            .merge(init.INSTRUMENTS[["instrument_id", "pair", "asset_class"]]) \
            .dropna(subset = ["pair"]) \
            .assign(future_id = np.nan) 
        
        df_conid = self.retrieveConIdForFXInstruments(df_identifier)
        
        return df_identifier \
            .merge(df_conid, on="instrument_id", how="left") \
            [["identifier","instrument_id", "pair", "conid", "future_id"]]
            
    @vu.tryDiagnosticNone()
    def futureListForIdentifier(self, df_identifier):
        return df_identifier \
            .assign(conid = df_identifier["identifier"]) \
            .merge(FUTURES_EXPIRY[["conid", "future_id"]], on="conid", how="left") \
            .dropna(subset = ["future_id"]) \
            .merge(FUTURES[["future_id", "instrument_id"]], on="future_id", how="left") \
            .merge(init.INSTRUMENTS[["instrument_id", "pair"]]) \
            [["identifier", "instrument_id", "pair", "conid", "future_id"]]
    
    @vu.tryDiagnosticNone()
    def remapIdentifierToEitherInstrumentOrFutureForExitTrades(self):
        df_identifier = self.orders_exit[["identifier"]].drop_duplicates()
        df_instrument = self.instrumentListForIdentifier(df_identifier)
        df_fut = self.futureListForIdentifier(df_identifier)
        df_identifier = df_instrument.append(df_fut)
        
        self.orders_exit = self.orders_exit \
            .merge(df_identifier, on="identifier", how="left") \
            .drop("identifier", axis=1)

    @vu.tryDiagnosticNone()
    def keepOnlyTradesForThisExecutionTime(self):
        instruments_to_keep = init.INSTRUMENTS["instrument_id"] \
            [(init.INSTRUMENTS["execution_time_id"] == self.execution_time_id)]
        self.orders_exit = self.orders_exit \
            [self.orders_exit["instrument_id"].isin(list(instruments_to_keep))]

    def invertSideForExitTrades(self):
        self.orders_exit = self.orders_exit \
            .assign(buy_sell = -self.orders_exit["buy_sell"])

    @vu.tryDiagnosticNone()
    def formatExitOrders(self):
        self.orders_exit = self.orders_exit \
            .assign(
                new_exit = "E",
                px_order = np.nan
                ) \
            [["new_exit", "account_id", "strategy_id", "instrument_id", "pair", 
              "future_id", "conid", "buy_sell", "size", "px_order"]]
    
    @vu.tryDiagnosticNone()
    def prepareOrdersExit(self):
        # output format:
        # new_exit, account_id, strategy_id, instrument_id, pair, future_id, 
        #   conid, buy_sell, size, px_order
        self.loadExitTradesForTodayFromDB()
        self.remapIdentifierToEitherInstrumentOrFutureForExitTrades()
        self.keepOnlyTradesForThisExecutionTime()
        self.invertSideForExitTrades()
        self.formatExitOrders()
        
    @vu.tryDiagnosticNone()
    def combineNewAndExitOrders(self):
        self.orders = self.orders_new \
            .append(self.orders_exit) \
            .merge(
                init.INSTRUMENTS[["instrument_id", "asset_class"]], 
                on="instrument_id", how="left"
                )
    
    def forceColumnType(self, col_list, type_function):
        for col_name in col_list:
            self.orders[col_name] = self.orders[col_name].map(type_function)
    
    @vu.tryDiagnosticNone() 
    def formatNumericalColumnTypes(self):
        int_col = ["account_id", "strategy_id", "instrument_id", "conid", "buy_sell"]
        float_col = ["size", "px_order"]
        self.forceColumnType(int_col, int)
        self.forceColumnType(float_col, float)

    @vu.tryDiagnosticNone()
    def summarizePositions(self):
        df_pos = self.orders
        df_pos = df_pos \
            .assign(size = df_pos["size"] * df_pos["buy_sell"]) \
            .groupby(
                ["account_id", "instrument_id", "pair", "asset_class", "conid"], 
                as_index=False) \
            .agg({"size":"sum"})
    
        df_pos = df_pos \
            .assign(
                buy_sell = df_pos["size"].map(vu.sign),
                size = df_pos["size"].map(abs)
                ) \
            .merge(
                FUTURES[["instrument_id", "future_id"]], 
                on="instrument_id", how="left"
                )
        return df_pos
    
    @vu.tryDiagnosticNone()
    def summarizePrices(self):
        if len(self.orders) == 0:
            return pd.DataFrame({"instrument_id":[], "px_order":[]})
        else:
            return self.orders \
                [["instrument_id","px_order"]] \
                .dropna() \
                .groupby("instrument_id", as_index=False) \
                .agg({"px_order":"mean"})            
        
    @vu.tryDiagnosticNone()
    def groupAndNetOrdersByInstrument(self):
        df_pos = self.summarizePositions()
        df_px = self.summarizePrices() 
        self.orders = df_pos \
            [df_pos["size"] > 0] \
            .merge(df_px, on="instrument_id", how="left") \
            [["account_id", "instrument_id", "future_id", "pair",
              "asset_class", "conid", "buy_sell", 
              "size", "px_order"]]
            
    @vu.tryDiagnosticNone()
    def addBuySellAction(self):
        self.orders = self.orders.assign(
            buy_sell_action = np.where(
                self.orders.buy_sell == 1, "BUY", 
                np.where(self.orders.buy_sell == -1, "SELL", "NOTHING")
                )
            )
        self.orders = self.orders[self.orders.buy_sell_action.isin({"BUY", "SELL"})]
        
    @vu.tryDiagnosticNone() 
    def addFutureTickSize(self):
        dat_tick = FUTURES[["future_id", "tick_size"]] \
            .rename({"tick_size":"tick_future"}, axis=1)
        self.orders = self.orders.merge(dat_tick, on="future_id", how="left")
    
    @vu.tryDiagnosticNone() 
    def addFXTickSize(self):
        TICK_SIZE_FX = db.loadTableLocal("instrument_attribute_dbl")
        dat_attribute = db.loadTableLocal("instrument_attribute_type")
        attribute_id = dat_attribute["attribute_id"] \
            [dat_attribute["attribute"] == "tick_size"]
        attribute_id = attribute_id.values[0]
        dat_tick = TICK_SIZE_FX \
            [TICK_SIZE_FX["attribute_id"] == attribute_id] \
            .rename({"value":"tick_fx"}, axis=1) \
            .drop("attribute_id", axis=1)
        self.orders = self.orders \
            .merge(dat_tick, on="instrument_id", how="left")
    
    @vu.tryDiagnosticNone() 
    def wrapUpTickSize(self):
        self.orders["tick_size"] = self.orders["tick_fx"]
        pos_fut = self.orders["tick_future"].notnull()
        self.orders["tick_size"][pos_fut] = self.orders["tick_future"][pos_fut]
        self.orders = self.orders \
            .drop("tick_future", axis=1) \
            .drop("tick_fx", axis=1) \
            .dropna(subset = ["tick_size"])
                
    @vu.tryDiagnosticNone() 
    def addTickSize(self):
        self.addFutureTickSize()
        self.addFXTickSize()
        self.wrapUpTickSize()
        
    @vu.tryDiagnosticNone()
    def getContractDetails(self):
        self.orders["conid"] = list(map(int, self.orders["conid"]))
        contract_details = fetchIBContractDetails(list(self.orders["conid"]))
        self.orders = self.orders \
            .merge(contract_details, on="conid", how="left")
    
    @vu.tryDiagnosticNone()
    def prepareExecutionColumns(self):
        self.orders["initial_position"] = 0
        self.orders["position"] = 0
        self.orders["filled"] = 0
        self.orders["remaining"] = self.orders["size"]
        self.orders["px_live"] = np.nan
        self.orders["px_avg"] = np.nan
        self.orders["order_id"] = list(range(1, 1+len(self.orders)))
        self.orders["ib_order_id"] = np.nan
        self.orders["status"] = "not yet started"
        self.orders["ib_status"] = ""
        
    @vu.tryDiagnosticNone()
    def prepareDetailedOrderList(self):
        print(1)
        self.prepareOrdersNew()
        print(2)
        print(self.orders_new)
        self.prepareOrdersExit()
        print(3)
        print(self.orders_exit)
        self.combineNewAndExitOrders()
        print(4)
        print(self.orders)
        self.formatNumericalColumnTypes()
        print(5)
        print(self.orders)
        self.groupAndNetOrdersByInstrument()
        print(6)
        print(self.orders)
        self.addBuySellAction()
        print(7)
        print(self.orders)
        self.addTickSize()
        print(8)
        print(self.orders)
        self.getContractDetails()
        print(9)
        print(self.orders)
        self.prepareExecutionColumns()
        print(10)
        print(self.orders)
    
####################################################################################################

class PrepareOrderListThisRound():
    ### Prepare initial order list for this round
    ### For the first round it takes for input the result of the previous task:
    ###     account_id  instrument_id  future_id    pair asset_class      conid
    ###     buy_sell, size, px_order, buy_sell_action, tick_size
    ###     contract, initial_position,
    ###     position, filled, remaining, px_live, px_avg, order_id, ib_order_id
    ###     status, ib_status
    ### And populates live info:
    ###     px_live, filled, status, initial_position, position, remaining, px_avg
    
    def __init__(self, order_list, account_id, i_round):
        self.account_id = account_id
        self.oda = order_list
        self.i_round = i_round
        self.start()

    def runLivePriceRoutine(self):
        os.system("RScriptVentura.sh Technicals/Price_Live.R")
    
    @vu.tryDiagnosticDat()
    def matchLivePriceSpotOrFuture(self, spot_or_fut):
        if spot_or_fut == "spot":
            sql_tbl = "live_px"
            id_col = "instrument_id"
            px_field = "close"
        elif spot_or_fut == "fut":
            sql_tbl = "live_px_future"
            id_col = "conid"
            px_field = "price"
        
        sql_q = f"""SELECT A.{id_col}, P.{px_field} AS price_db
            FROM (
            	SELECT {id_col}, MAX(timestamp) AS timestamp
                FROM {sql_tbl}
                GROUP BY {id_col}
                ) A
            LEFT JOIN {sql_tbl} P ON P.{id_col} = A.{id_col} AND P.timestamp = A.timestamp
            WHERE A.timestamp >= '{TIME_LIMIT_FOR_PX}'"""
        
        df_px = db.select(sql_q)
        
        self.oda = self.oda \
            .merge(df_px, on=id_col, how="left")
        pos_not_null = self.oda["price_db"].notnull()
        self.oda["px_live"][pos_not_null] = self.oda["price_db"][pos_not_null]
        self.oda.drop("price_db", axis=1, inplace=True)

    @vu.tryDiagnosticDat()
    def getLivePricesSpot(self):
        self.matchLivePriceSpotOrFuture("spot")
    
    @vu.tryDiagnosticDat()
    def getLivePricesFuture(self):
        self.matchLivePriceSpotOrFuture("fut")
    
    @vu.tryDiagnosticDat()
    def keepOnlyInstrumentsWithAPrice(self):
        self.oda.dropna(subset = ["px_live"], inplace=True)
    
    def roundPriceToClosestTick(self, px_col):
        self.oda[px_col] = vu.mround(self.oda[px_col], self.oda["tick_size"])
        
    @vu.tryDiagnosticDat()
    def addPositions(self): 
        print("ppp-0")
        df_position = getPricePositions()
        print("ppp-1")
        print(df_position)
        df_position = df_position \
            [["account_id", "conid", "position"]]
        print("ppp-2")
        print(df_position)    
        df_position = df_position \
            .assign(
                account_id = lambda x: list(map(int, x.account_id)),
                conid = lambda x: list(map(int, x.conid)),
                position = lambda x: list(map(float, x.position))
                )
        print("ppp-3")
        print(df_position)
            
        self.oda = self.oda \
            .drop("position", axis=1) \
            .merge(df_position, on=["account_id", "conid"], how="left")
        print("ppp-4")
        print(df_position)
        
        print("ppp-5")
        print(self.i_round)

        if self.i_round == 1:
            print("ppp-6")
            print(self.oda)
            self.oda["initial_position"] = self.oda["position"]
            self.oda = self.oda.fillna({"initial_position":0})
            print("ppp-7")
            print(self.oda)

    @vu.tryDiagnosticNone()
    def getLivePrices(self):
      #  self.runLivePriceRoutine()
        self.oda["px_live"] = np.nan
        self.getLivePricesSpot()
        self.getLivePricesFuture()
        self.keepOnlyInstrumentsWithAPrice()
        self.roundPriceToClosestTick("px_live")
        self.roundPriceToClosestTick("px_order")
        self.addPositions()
    
    def moveInitialPriceAFewTicks(self, px_order_col):
        self.oda["px_order_adj"] = self.oda[px_order_col]
        self.oda = self.oda \
            .assign(
                px_order_adj = self.oda[px_order_col] 
                - INITIAL_ORDER_TICKS_FROM_MID 
                * self.oda["buy_sell"] * self.oda["tick_size"]
                )
        self.oda[px_order_col] = self.oda["px_order_adj"]
        self.oda.drop("px_order_adj", axis=1, inplace=True)
    
    def setOrderPriceToLivePriceIfOrderPriceIsMissing(self):
        pos_na = self.oda["px_order"].isnull()
        self.oda["px_order"][pos_na] = self.oda["px_live"][pos_na]
    
    def setOrderPriceToLivePriceIfTheyAreCloseEnough(self):
        self.oda = self.oda \
            .assign(
                px_diff_pct = abs(self.oda["px_live"] / self.oda["px_order"] - 1)
                )
        pos_close_enough = (
            self.oda["px_diff_pct"] <= LIMIT_EXECUTION_PRICE_FROM_INITIAL_MID_PCT
            )
        self.oda["px_order"][pos_close_enough] = self.oda["px_live"][pos_close_enough]
        self.oda.drop("px_diff_pct", axis=1, inplace=True)
    
    @vu.tryDiagnosticNone()
    def prepareInitialOrderPrice(self):
        self.setOrderPriceToLivePriceIfOrderPriceIsMissing()
        self.setOrderPriceToLivePriceIfTheyAreCloseEnough()
        self.moveInitialPriceAFewTicks("px_order")
    
    @vu.tryDiagnosticNone()    
    def prepareOrderPriceLimit(self):
        self.oda = self.oda \
            .assign(
                px_limit = self.oda["px_order"] 
                * (1 + self.oda["buy_sell"] 
                * LIMIT_EXECUTION_PRICE_FROM_INITIAL_MID_PCT)
            )
        
        pos_beyond_limit = (
            (self.oda["buy_sell"] * self.oda["px_order"]) 
                > (self.oda["buy_sell"] * self.oda["px_limit"])
            )
            
        self.oda["px_order"][pos_beyond_limit] = self.oda["px_limit"][pos_beyond_limit]
    
    @vu.trySimpleNone()   
    def roundOrderSize(self):
        # Need to be much more comprehensive
        self.oda = self.oda.assign(size = round(self.oda["size"]-0.25, 0))
        
    @vu.trySimpleNone()  
    def keepOnlyPositiveSizeOrders(self):
        self.oda = self.oda[self.oda["size"] > 0]
        
    @vu.trySimpleNone()   
    def removeNegativeRemainingSizeToDo(self):
        self.oda["remaining"][self.oda["remaining"] < 0] = 0
    
    @vu.trySimpleNone()    
    def removeDuplicatedInstrumentRows(self):
        self.oda.drop_duplicates(subset =["instrument_id"], keep=False, inplace=True) 
    
    @vu.tryDiagnosticNone()
    def finalFormatting(self):
        if "filled" not in self.oda.columns:
            self.oda["filled"] = 0
        if "status" not in self.oda.columns:
            self.oda["status"] = "not_yet_started"
    
        self.roundPriceToClosestTick("px_order")
        self.roundPriceToClosestTick("px_limit")
        self.oda = self.oda \
            .assign(
                remaining = self.oda ["size"] - self.oda ["filled"],
                px_avg = 0
            )
        
        self.removeNegativeRemainingSizeToDo()   
        #dat.drop("account_id", axis = 1, inplace=True)   

    @vu.trySimpleNone()
    def start(self):
        self.getLivePrices()
        print(1)
        print(self.oda)
        self.prepareInitialOrderPrice()
        print(2)
        print(self.oda)
        self.prepareOrderPriceLimit()
        print(3)
        print(self.oda)
        self.roundOrderSize()
        print(4)
        print(self.oda)
        self.keepOnlyPositiveSizeOrders()
        print(5)
        print(self.oda)
        self.finalFormatting()
        print(6)
        print(self.oda)
####################################################################################################


class IBExecuteOrders(EWrapper, EClient):
    ### Actual execution of the trades
    ### Order list looks like this:
    ###     account_id, instrument_id, pair, future_id, 
    ###     conid, buy_sell, size, px_order, 
    ###     buy_sell_action, tick_size, contract_details,
    ###     px_live, filled, status, initial_position, position, remaining, px_avg
    
    def __init__(self, order_list, account_id):
        EClient.__init__(self, self)
        self.started = False
        self.continue_with_loop = True
        self.iteration = 0
        self.account_id = account_id
        self.oda = order_list
        self.nb_orders = len(order_list)
        self.nextOrderId = None
        self.doTrades()
                
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
        
        if (remaining > 0) & (self.oda["size"] > remaining):
            self.oda["status"][pos_id] = "Working - some filled"
        if (remaining <= 0) or (status == "Filled"):
            self.oda["status"][pos_id] = "Filled"
        if (status == "Inactive"):
            self.oda["status"][pos_id] = "Rejected"
            
        if self.oda["status"][pos_id] == "Filled":
            self.cancelOrder(orderId)

    def initializeOrderList(self):
        self.oda["ib_order_id"] = list(range(self.nextOrderId, self.nextOrderId + self.nb_orders))
        self.oda["status"] = "Not yet started"
        self.oda["ib_status"] = "Not yet started"
        vu.printBanner("Initialized these these orders...")
        print(self.oda)

    def readThisOrderDetails(self, i):
        vu.printBanner("Order Description:")
        print(self.oda.loc[[i]])
        return self.oda.loc[[i]]
       
    def prepareIBOrder(self, ib_order_id, this_order_details):
        order = Order()
        order.action = this_order_details["buy_sell_action"].values[0]
        order.totalQuantity = this_order_details["size"].values[0]
        order.orderType = "LMT"
        order.lmtPrice = this_order_details["px_order"].values[0]
        order.tif = "DAY"

        if not np.isnan(ib_order_id):
            order.orderId = ib_order_id
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
        self.oda.to_csv(file_path_name)
        
    def writeExecutionDataSubmission(self):
        vu.printBanner("New iteration - going to do this:")
        self.writeExecutionData("execution_loop_submission")
    
    def writeExecutionDataResult(self):
        self.writeExecutionData("execution_loop_result")
        
    def testIfOrderIsGood(self, this_order_details):
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

    @vu.trySimpleDat()
    def calcMoveAwayPriceIncrement(self):
        self.oda = self.oda \
            .merge(MOVE_AWAY_ORDER_PCT, on = "asset_class", how = "left")
        self.oda = self.oda.assign(
            px_move_away = -self.oda["move_away_pct"] * self.oda["buy_sell"] * self.oda["px_order"]
            )
        self.oda["px_move_away"] = vu.mround(self.oda["px_move_away"], self.oda["tick_size"])
    
        self.oda.drop("move_away_pct", axis=1, inplace=True)
        
    def makeAPause(self):
        time.sleep(TIME_SLEEP_SECONDS)

    @vu.tryDiagnosticDat()
    def doTrades(self):
        self.calcMoveAwayPriceIncrement()
        #    order_list.to_csv(PATH_EXECUTION + "order_list_sample.csv")
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
### Sub Routines for prepareOrderList
####################################################################################################    
@vu.tryDiagnosticDat()
def fetchIBContractDetails(conid_list):
    account_id_contracts = 2
    res = pd.DataFrame({
            "conid":[], 
            "contract":[]
            })
    if len(conid_list) > 0:
        conid_list = list(map(int, conid_list))
        ib_app = IBAppRetrieveContracts(conid_list)
        ib_app.connect(
            "127.0.0.1", 
            vx.getIBPort(account_id_contracts), 
            vx.getIBClientId(script_name)
            )
        ib_app.run()    
        res = pd.DataFrame({
            "conid":conid_list, 
            "contract":ib_app.contract_details
            })
    return res
        
####################################################################################################
### Sub Routines remaining
####################################################################################################  
@vu.tryDiagnosticDat()    
def formatFilledResults(order_list):
    return order_list \
        [["account_id", "pair", "size", "filled"]] \
        .rename({"filled":"new_filled"}, axis=1)

@vu.tryDiagnosticDat()    
def formatOrdersResults(order_list, filled_orders):
    order_list = order_list.merge(
        filled_orders, 
        on = ["account_id","pair","size"], 
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
        .assign(size = df["size"] - df["filled"]) \
        [df["size"] > 0] \
        [["account_id", "pair", "buy_sell", "buy_sell_action", "size",
               "px_order", "px_limit", "conid", "tick_size", "filled"]]


def testKeepGoing(all_done):
    return ((not all_done) and (datetime.now() < EXECUTION_ENDING_TIME))
    
def printDoingRound(i, orders_to_do):
    vu.printBanner(f"Doing Round {i} - Orders before preparation:")
    print(orders_to_do)
    
    
####################################################################################################
### Sub routines
####################################################################################################
def readCombinedOrders(account_id):
    file_path = PATH_ORDERS_COMBINED
    file_name = "combined_orders_{}_{}-{}.csv".format(
        str(account_id),
        init.TODAY.strftime("%Y%m%d"), 
        ("0" + str(EXECUTION_TIME_ID))[-2:]
        )
    file_path_name = file_path + file_name
    return pd.read_csv(file_path_name)

def doTheTrades(orders_to_do, account_id):
    ib_app = IBExecuteOrders(account_id, orders_to_do)
    ib_app.connect("127.0.0.1", vx.getIBPort(account_id), vx.getIBClientId("Do_The_Trades"))
    ib_app.run() 

####################################################################################################
### Script
#################################################################################################### 
if __name__ == "__main__":
    vx.waitTillPreviousJobHasFinished("Signal_List", 1, 1, 5, 5)
    order_list = PrepareInitialOrderList(ACCOUNT_ID)()
    vu.printBanner("INITIAL ORDER LIST:")
    print(order_list)
    orders_result = order_list
    orders_to_do = order_list
    i = 1
    all_done = False
    keep_going = True
    
    vu.printBanner("Is order list empty:", False)
    print(len(order_list) == 0)
    
    if len(order_list) == 0:
        keep_going = False
        print("AAZ-1")
        
    while keep_going:
        orders_this_loop = None
        
        printDoingRound(i, orders_to_do)
        vu.printBanner("PrepareOrderListThisRound - Beginning:")
        orders_this_loop = PrepareOrderListThisRound(orders_to_do, ACCOUNT_ID, i)
        orders_this_loop = orders_this_loop.oda
        vu.printBanner("PrepareOrderListThisRound - DONE: Result:")
        print(orders_this_loop)
        vu.printBanner("IBExecuteOrders - Beginning:")
        orders_this_loop = IBExecuteOrders(orders_this_loop, ACCOUNT_ID)
        orders_this_loop = orders_this_loop.oda
        vu.printBanner("IBExecuteOrders - DONE: Result:")
        print(orders_this_loop)
        orders_filled = formatFilledResults(orders_this_loop)
        orders_result = formatOrdersResults(orders_result, orders_filled)
        orders_to_do = prepareOrdersToDo(orders_result, i)

        if len(orders_to_do) == 0: 
            all_done = True
        vu.printBanner(f"Are we all done ? {all_done}", all_done)
        writeLoopResults(
            i, ACCOUNT_ID, orders_this_loop, orders_filled, orders_to_do, orders_result
            )

        keep_going = testKeepGoing(all_done)
        vu.printBanner(f"Do we keep going ? {keep_going}")

        vu.printBanner(f"Round {i} done. Trades left to do:")

        time.sleep(WAIT_AT_CYCLE_END_SECONDS)
        i += 1
        keep_going = False
    
    vu.printBanner("Execution routine finished:")
    print(orders_result)
    init.scriptFinish(script_name)