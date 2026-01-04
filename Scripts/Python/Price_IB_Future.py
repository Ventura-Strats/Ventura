#!/usr/bin/env python3
# -*- coding: utf-8 -*-

####################################################################################################
### Retrieves daily historical OHLC from IB where available (defined in DB)
### That includes today's prices if market has open
### Then saves into csv files that are picked up by the broader R Script
####################################################################################################
script_name = "Price_IB_Future"
max_time_hours = 1 / 60

####################################################################################################
### Imports
####################################################################################################
from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract
from ibapi.ticktype import TickTypeEnum
import numpy as np
import pandas as pd
import init
import utils as vu
import execution_utils as xu
import db
from threading import Timer
from datetime import datetime
from datetime import timedelta

####################################################################################################
### Script initialization
####################################################################################################
init.scriptInit(script_name, max_time_hours)

####################################################################################################
### Script variables
####################################################################################################
BAR_TYPE = "1 day"          # "1 hour"
HISTO_LENGTH = "1 D"        # "1 W" "1 M" etc
FILE_PATH = init.DIRECTORY_DATA + "Spot/IB_Future/"
ACCOUNT_ID = 1
DATE_FORMAT = "%Y%m%d"
TIME_WAIT_FOR_PRICES_SECONDS = 10
KEEP_RECENT_PX_THRESHOLD_MINUTES = 2

####################################################################################################
### Classes
####################################################################################################
class ibAppLive(EWrapper, EClient):
    def __init__(self, contract_list, live_or_delayed):
        EClient.__init__(self, self)
        self.dat = contract_list
        self.live_or_delayed = live_or_delayed
        
    def error(self, reqId , errorCode, errorString, advancedOrderRejectJson):
        print("Error ", reqId, " ", errorCode, " ", errorString)
        if errorCode == 502:
            print("Error - Not connected - exiting")
            self.stop()

    def nextValidId(self, orderId):
        self.start()  
    
    def tickPrice(self, reqId, tickType, price, attrib):
        tick_type = TickTypeEnum.to_str(tickType)
        col_bid = "bid"
        col_ask = "ask"
        col_last = "last"
        if self.live_or_delayed == "delayed":
            col_bid = col_bid + "_delayed"
            col_ask = col_ask + "_delayed"
            col_last = col_last + "_delayed"
        if (price != -1) and (price != 0):    
            if tick_type == "BID":
                self.dat[col_bid][reqId] = price
            elif tick_type == "ASK":
                self.dat[col_ask][reqId] = price
            elif tick_type == "LAST":  
                self.dat[col_last][reqId] = price

    
    def historicalData(self, reqId, bar):
        is_today = (pd.to_datetime(bar.date, format=DATE_FORMAT) >= init.TODAY)
        if is_today:
            self.dat["histo"][reqId] = float(bar.close)
            
    def updatePortfolio(self, contract, position, marketPrice, marketvalue, 
                        averageCost, unrealizedPNL, realizedPNL, accountName):
        self.dat["book"][self.dat["conid"] == contract.conId] = marketPrice


    def getPricesFromBook(self):
        if self.live_or_delayed == "live":
            self.reqAccountUpdates(True, "")
        
    def getPricesFromStream(self, contract, i):
        market_data_type = 1
        if self.live_or_delayed == "delayed":
            market_data_type == 3
        self.reqMarketDataType(market_data_type)
        self.reqMktData(i, contract, "", False, False, [])
        
    def getPricesFromHisto(self, contract, i):
        if self.live_or_delayed == "live":
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
        self.disconnect()

####################################################################################################
### Sub routines
####################################################################################################
def buildSimpleContract(symbol, ccy, sectype, exchange, conid):
    contract = Contract()
    contract.symbol = symbol
    contract.currency = ccy
    contract.secType = sectype
    contract.exchange = exchange
    contract.conId = conid
    return contract

def buildContractList(dat):
    dat["contract"] = None
    for i in range(len(dat)):
        dat["contract"].values[i] = buildSimpleContract(
            dat["ib_symbol"].values[i], 
            dat["ccy"].values[i], 
            dat["sectype"].values[i], 
            dat["ib_exchange"].values[i], 
            dat["conid"].values[i]
            )
    return dat

@vu.tryDiagnosticNone()
def getContractList():
    FUTURES = db.loadTableLocal("future_contract")
    FUTURES_EXPIRIES = db.loadTableLocal("future_expiry")
    FUTURES_ACTIVE = db.loadTableLocal("future_active")
    EXCHANGES = db.loadTableLocal("exchange")
    CURRENCIES = db.loadTableLocal("currency")
    dat = FUTURES_ACTIVE.merge(FUTURES_EXPIRIES, on=["conid","future_id"], how="left")
    dat = dat.merge(FUTURES, on="future_id", how="left")
    dat = dat.merge(CURRENCIES, on="ccy_id", how="left")
    dat = dat.merge(EXCHANGES, on="exchange_id", how="left")
    dat = dat.merge(init.INSTRUMENTS[["instrument_id","asset_class"]], on="instrument_id", how="left")
    dat["sectype"] = "FUT"

    pos_index = (dat["asset_class"] == "index")
    dat["data_type"] = "MIDPOINT"
    dat["data_type"][pos_index] = "TRADES"
    
    dat = dat[["instrument_id", "future_id", "ib_symbol", 
               "conid", "sectype", "expiry", "ccy", "ib_exchange", "data_type", "tick_size"]]
    dat = buildContractList(dat)
    dat["timestamp"] = init.char_start_time
    dat["bid"] = np.nan
    dat["ask"] = np.nan
    dat["mid"] = np.nan
    dat["last"] = np.nan
    dat["histo"] = np.nan
    dat["book"] = np.nan
    dat["bid_delayed"] = np.nan
    dat["ask_delayed"] = np.nan
    dat["mid_delayed"] = np.nan
    dat["last_delayed"] = np.nan
    
    return dat

@vu.tryDiagnosticDat()
def readInvestingPricesFromDB(dat):
    time_limit = init.start_time - timedelta(minutes = KEEP_RECENT_PX_THRESHOLD_MINUTES)
    time_limit_chr = time_limit.strftime("%Y-%m-%d %H:%M:%S")
    dat_px = xu.getLatestFuturePricesFromDB(time_limit_chr)
    dat_px.rename({"price":"investing"}, axis=1, inplace=True)
    dat = dat.merge(dat_px, on="conid", how="left")
    dat["price"] = np.nan
    return dat

@vu.trySimpleNone()
def determineBestUsablePrice(dat, i):
    bid = dat["bid"].values[i]
    ask = dat["ask"].values[i]
    last = dat["last"].values[i]
    histo = dat["histo"].values[i]
    book = dat["book"].values[i]
    tick_size = dat["tick_size"].values[i]
    investing = dat["investing"].values[i]
    
    mid = np.nan
    
    if np.isnan(bid) or np.isnan(ask):
        bid = ask = np.nan
    elif ask <= bid:
        bid = ask = np.nan
    else:
        mid = 0.5 * (bid + ask)
        bid_ask_vs_tick_size = (ask - bid) / tick_size
        if bid_ask_vs_tick_size > 10:
            bid = ask = np.nan
        elif bid_ask_vs_tick_size <= 1.5:
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
        "weight":[400, 200, 200, 100, 1],
        "price":[mid, last, histo, book, investing]
        }) \
        .dropna(subset=["price"])

    price = np.nan
    if len(dat_px) > 0:
        price = sum(dat_px.weight * dat_px.price) / sum(dat_px.weight)
    
    dat["mid"][i] = mid
    dat["price"][i] = price

    return dat

@vu.tryDiagnosticNone()    
def calcPrice(dat):
    for i in range(len(dat)):
        dat = determineBestUsablePrice(dat, i)
    return dat

def getDelayedData():
    ib_app = ibAppLive(contract_list, "delayed")
    ib_app.connect("127.0.0.1", xu.getIBPort(ACCOUNT_ID), xu.getIBClientId(script_name))      
    ib_app.run()
    vu.printBanner("Second run with delayed:")
    dat_delayed = ib_app.dat
    print(dat_delayed[["ib_symbol", "bid", "ask", "last", "bid_delayed", "ask_delayed", 
                       "last_delayed"]])
    return dat_delayed

@vu.tryDiagnosticDat()
def eraseMissingPrices(dat):
    dat.dropna(subset=["price"], inplace=True)
    return dat

@vu.tryDiagnosticDat()
def saveToDB(dat):
    dat_db = dat[["conid", "timestamp", "price"]]
    print("Saving this:")
    print(dat_db)
    db.appendToTable("live_px_future", dat_db)
    return dat

####################################################################################################
### Script
####################################################################################################
if __name__ == "__main__":
    contract_list = getContractList()
    ib_app = ibAppLive(contract_list, "live")
    ib_app.connect("127.0.0.1", xu.getIBPort(ACCOUNT_ID), xu.getIBClientId(script_name))      
    ib_app.run()
    dat = ib_app.dat
    dat = readInvestingPricesFromDB(dat)
    dat = calcPrice(dat)
    dat = eraseMissingPrices(dat)
    dat.to_csv(FILE_PATH + "px_future.csv", index=False)
    saveToDB(dat)
    init.scriptFinish(script_name)
