#!/usr/bin/env python3
# -*- coding: utf-8 -*-

####################################################################################################
### Retrieves daily historical OHLC from IB where available (defined in DB)
### That includes today's prices if market has open
### Then saves into csv files that are picked up by the broader R Script
####################################################################################################
script_name = "Price_IB_Exec"
max_time_hours = 7 / 60

####################################################################################################
### Imports
####################################################################################################
from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract
from ibapi.ticktype import TickTypeEnum
import pandas as pd
import numpy as np
import init
import utils as vu
import execution_utils as xu
import execution_utils as vx
import db
from threading import Timer
import time
from datetime import datetime
from datetime import timedelta
import subprocess
import os

####################################################################################################
### Script initialization
####################################################################################################
init.scriptInit(script_name, max_time_hours)

####################################################################################################
### Script variables
####################################################################################################
BAR_TYPE = "1 day"          # "1 hour"
HISTO_LENGTH = "1 D"        # "1 W" "1 M" etc
FILE_PATH = init.DIRECTORY_DATA + "Spot/Live_Exec/"
DATE_FORMAT = "%Y%m%d"
TIME_WAIT_FOR_PRICES_SECONDS = 5
KEEP_RECENT_PX_THRESHOLD_MINUTES = 5
EXECUTION_TIME_ID = vx.findExecutionTime()
PATH_ORDERS_COMBINED = init.DIRECTORY_DATA + "Orders/Combined/"
ACCOUNT_LIST = [1,2]
TIME_FINISH = init.start_time + timedelta(hours=max_time_hours)
WAIT_TIME_BEFORE_RERUNNING_SECONDS = 10

####################################################################################################
### Classes
####################################################################################################
class ibAppLive(EWrapper, EClient):
    def __init__(self, contract_list):
        EClient.__init__(self, self)
        self.dat = contract_list
        
    def error(self, reqId , errorCode, errorString, advancedOrderRejectJson):
        print("Error ", reqId, " ", errorCode, " ", errorString)
        if errorCode == 502:
            print("Error - Not connected - exiting")
            self.stop()

    def nextValidId(self, orderId):
        self.start()  
    
    def tickPrice(self, reqId, tickType, price, attrib):
        tick_type = TickTypeEnum.to_str(tickType).lower()
        if (price != -1) and (price != 0) and (tick_type in ["bid", "ask", "last"]):
            self.dat[tick_type][reqId] = price
    
    def historicalData(self, reqId, bar):
        is_today = (pd.to_datetime(bar.date, format=DATE_FORMAT) >= init.TODAY)
        if is_today:
            self.dat["histo"][reqId] = float(bar.close)
            
    def updatePortfolio(self, contract, position, marketPrice, marketvalue, 
                        averageCost, unrealizedPNL, realizedPNL, accountName):
        self.dat["book"][self.dat["conid"] == contract.conId] = marketPrice

    def getPricesFromBook(self):
        self.reqAccountUpdates(True, "")
        
    def getPricesFromStream(self, contract, i):
        market_data_type = 1
        #if self.live_or_delayed == "delayed":
        #    market_data_type == 3
        self.reqMarketDataType(market_data_type)
        self.reqMktData(i, contract, "", False, False, [])
        
    def getPricesFromHisto(self, contract, i):
        self.reqHistoricalData(
            i, 
            contract, 
            "", 
            HISTO_LENGTH, 
            BAR_TYPE, 
            self.dat["data_type"].values[i], 
            0, 1, False, []
            )
    
    def start(self):
        self.getPricesFromBook()
        for i in range(len(self.dat)):
            contract = self.dat["contract"][i]
            if contract is not None:
                self.getPricesFromStream(contract, i)
                self.getPricesFromHisto(contract, i)

        Timer(TIME_WAIT_FOR_PRICES_SECONDS, self.stop).start()

    def stop(self):
        self.dat = self.dat[["conid", "bid", "ask", "last", "histo", "book"]]
        self.disconnect()

####################################################################################################
### Sub routines
####################################################################################################
@vu.tryDiagnosticNone()
def readInstrumentsListAccount(account_id):
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
    
    vu.printBanner("Opening this file:", False)
    print(file_path + file_name)
    
    try:
        #order_list = pd.read_csv("/home/fls/Mount/Glenorchy/FX/Orders/Combined/test_order.csv")
        print("reading file")
        order_list = pd.read_csv(file_path + file_name)
        print("keeping selected columns")
        order_list = order_list \
            [["instrument_id", "ticker", "asset_class", "future_id", "conid", "contract"]]
        print("making columns int")
        for col_name in ["instrument_id", "future_id", "conid"]:
            order_list[col_name] = order_list.col_name.map(int)
        print("done")
    except:
        order_list = pd.DataFrame({
            "instrument_id":[], 
            "ticker":[], 
            "asset_class":[], 
            "future_id":[], 
            "conid":[], 
            "contract":[]
            })
    finally:
        return order_list
    
@vu.tryDiagnosticNone()
def formatContracts(df):
    df["timestamp"] = init.char_start_time
    df["bid"] = df["ask"] = df["mid"] = df["last"] = df["histo"] = df["book"] = np.nan
    
    pos_index = (df["asset_class"] == "index")
    df["data_type"] = "MIDPOINT"
    df["data_type"][pos_index] = "TRADES"
    df["timestamp"] = init.char_start_time
    return df
    
@vu.tryDiagnosticNone()
def readInstrumentsList():
    df = pd.DataFrame()
    for account_id in ACCOUNT_LIST:
        df_i = readInstrumentsListAccount(account_id)
        df = df.append(df_i)
    df = df.drop_duplicates()
    df = formatContracts(df)
    return df

@vu.tryDiagnosticNone()
def getIBPrices(contract_list):
    if len(contract_list) > 0:
        df_px = pd.DataFrame()
        
        for account_id in ACCOUNT_LIST:
            ib_app = ibAppLive(contract_list)
            ib_app.connect("127.0.0.1", xu.getIBPort(account_id), xu.getIBClientId(script_name))      
            ib_app.run()
            df_i = ib_app.dat
            ib_app.disconnect()
            df_px = df_px.append(df_i)
        vu.printBanner("PPP-1", False)
        print(df_px)
            
        df_px = df_px \
            .groupby("conid", as_index=False) \
            .agg({
                "bid": "mean",
                "ask": "ask",
                "last": "last",
                "book": "book",
                "histo": "histo"
                })
        vu.printBanner("PPP-2", False)
        print(df_px)
        contract_list = contract_list. \
            drop(subset=["bid","ask","last","histo","book"], errors="ignore")
        vu.printBanner("PPP-3", False)
        print(contract_list)
        contract_list = contract_list.merge(df_px, on="conid", how="left")
    vu.printBanner("PPP-4", False)
    print(contract_list)
    return contract_list

@vu.tryDiagnosticDat()
def readInvestingPricesFromDB(dat):
    time_limit = init.start_time - timedelta(minutes = KEEP_RECENT_PX_THRESHOLD_MINUTES)
    time_limit_chr = time_limit.strftime("%Y-%m-%d %H:%M:%S")
    dat_px = xu.getLatestFuturePricesFromDB(time_limit_chr) \
        .rename({"price":"investing"}, axis=1)
    return dat.merge(dat_px, on="conid", how="left")

@vu.tryDiagnosticNone()
def determineBestUsablePrice(dat, i):
    bid = dat["bid"].values[i]
    ask = dat["ask"].values[i]
    last = dat["last"].values[i]
    histo = dat["histo"].values[i]
    book = dat["book"].values[i]
    investing = dat["investing"].values[i]
    
    mid = np.nan
    
    if np.isnan(bid) or np.isnan(ask):
        bid = ask = np.nan
    elif ask <= bid:
        bid = ask = np.nan
    else:
        mid = 0.5 * (bid + ask)
        bid_ask_pct = 100 * (ask - bid) / mid
        if bid_ask_pct > 0.1:
            bid = ask = mid = np.nan
        elif bid_ask_pct <= 0.002:
            last = histo = book = np.nan
    
    if not np.isnan(mid):
        if not np.isnan(last):
            if not (bid <= last <= ask):
                last = np.nan
        if not np.isnan(histo):
            if not (bid <= histo <= ask):
                histo = np.nan
        if not np.isnan(book):
            if not (bid <= book <= ask):
                book = np.nan
    
    dat_px = pd.DataFrame({
            "weight":[400,200,200,100,1],
            "price":[mid,last,histo,book,investing]
        }) \
        .dropna(subset=["price"])

    price = np.nan
    if len(dat_px) > 0:
        price = sum(dat_px.weight * dat_px.price) / sum(dat_px.weight)
    
    dat["mid"][i] = mid
    dat["price"][i] = price

    return dat

@vu.tryDiagnosticNone()    
def calcPrice(df):
    df["price"] = np.nan
    for i in range(len(df)):
        df = determineBestUsablePrice(df, i)
    return df

@vu.tryDiagnosticDat()
def eraseMissingPrices(df):
    return df.dropna(subset=["price"])

@vu.tryDiagnosticDat()
def saveToFileAndDB(df):
    df.to_csv(FILE_PATH + "px_live_exec.csv", index=False)
    dat_db = df[["conid", "timestamp", 
        "bid", "ask", "mid", "last", "histo", "book", "investing", "price"]]
    db.replaceToTable("live_px_exec", dat_db)
    
def testIfExecutionScriptStillRunning():
    try:
        shell_command = "ps -aux | grep [p]ython"
        python_processes = subprocess.check_output(shell_command, shell=True)
        python_processes = str(python_processes)
        still_running = ("Execute_Orders.py" in python_processes)
    except:
        still_running = False
    finally:
        return still_running

@vu.tryDiagnosticDat()
def prepareLiveIBPrices(keep_going):
    contract_list  = readInstrumentsList()
    dat = getIBPrices(contract_list)
    dat = readInvestingPricesFromDB(dat)
    dat = calcPrice(dat)
    dat = eraseMissingPrices(dat)
    saveToFileAndDB(dat)
    still_running = testIfExecutionScriptStillRunning()
    keep_going = (datetime.now() <= TIME_FINISH) and still_running
    return keep_going

@vu.tryDiagnosticNone()
def eraseLivePrices():
    file_name = FILE_PATH + "px_live_exec.csv"
    try:
        os.remove(file_name)
    except:
        pass
    sql_q = "TRUNCATE TABLE live_px_exec"
    db.executeSQL(sql_q)

####################################################################################################
### Script
####################################################################################################
if __name__ == "__main__":
    vx.waitTillPreviousJobHasFinished("Signal_List", 1, 1, 5, 5)
    eraseLivePrices()
    keep_going = (datetime.now() <= TIME_FINISH)
    while keep_going:
        keep_going = prepareLiveIBPrices(keep_going)
        time.sleep(WAIT_TIME_BEFORE_RERUNNING_SECONDS)
    init.scriptFinish(script_name)
