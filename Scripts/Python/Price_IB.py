#!/usr/bin/env python3
# -*- coding: utf-8 -*-

####################################################################################################
### Retrieves daily historical OHLC from IB where available (defined in DB)
### That includes today's prices if market has open
### Then saves into csv files that are picked up by the broader R Script
####################################################################################################
script_name = "Price_IB"
max_time_hours = 0.25

####################################################################################################
### Imports
####################################################################################################
from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract
import pandas as pd
from datetime import datetime
import time 
import os
import sys
import init
import utils as ut
import execution_utils as xu
import db
from threading import Timer

####################################################################################################
### Script initialization
####################################################################################################
init.scriptInit(script_name, max_time_hours)

####################################################################################################
### Script variables
####################################################################################################
bar_type = "1 day"          # "1 hour"
histo_length = "2 W"        # "1 W" "1 M" etc
file_path = init.DIRECTORY_DATA + "Spot/IB/"
ACCOUNT_ID = int(sys.argv[2])

####################################################################################################
### Classes
####################################################################################################
class ibApp(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
        self.started = False
        self.date_format = determineDateFormat()
        self.asset_list = buildAssetList()
        self.nb_assets = len(self.asset_list)
        self.histo_data_list = [[] for _ in range(self.nb_assets)]
        self.ticker_already_covered = [False for _ in range(self.nb_assets)]
        
    def error(self, reqId , errorCode, errorString, advancedOrderRejectJson=""):
        try:
            pair_name = str(self.asset_list["pair"][reqId])
        except:
            pair_name = ""
        print("Error: ", pair_name, " ", reqId, " ", errorCode, " ", errorString)
        if errorCode in [162, 200, 354]:
            if not self.ticker_already_covered[reqId]:
                ut.printBanner("Error - Moving on to next asset")
        elif errorCode == 502:
            ut.printBanner("Error - Not connected - exiting")
            self.stop()

    def nextValidId(self, orderId):
        if not self.started:
            self.start()
           
    def historicalData(self, reqId, bar):
        self.histo_data_list[reqId].append(
            [self.asset_list["pair"][reqId],
             bar.date, 
             bar.open, 
             bar.high, 
             bar.low, 
             bar.close]
            )
                
    def historicalDataEnd(self, reqId: int, start: str, end: str):
        dat = convertIBMessageToDF(self.histo_data_list[reqId])
        if len(dat.index) != 0:
            dat['date'] = pd.to_datetime(dat['date'], format = self.date_format)
            file_path_name = file_path + self.asset_list["pair"][reqId] + "_histo.csv"
            print(file_path_name)
            print(dat)
            dat.to_csv(file_path_name, index = False) 
            time_now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
            ut.printBanner(f"file saved - {file_path_name} - {time_now}", False)
            self.ticker_already_covered[reqId] = True
        finished = all(self.ticker_already_covered)
        if finished:
            ut.printBanner("All done here")
            self.stop()
        else:
            ut.printBanner("Pairs still missing:", False)
            for i in range(self.nb_assets):
                if not self.ticker_already_covered[i]:
                    print(self.asset_list["pair"].values[i])
   
    def start(self):
        for i in range(self.nb_assets):  
            pair = self.asset_list["pair"].values[i]
            ut.printBanner(f"Doing {pair} ...")
            try:
                contract = prepareContract(pair, self.asset_list)
                data_type = self.asset_list["data_type"].values[i]
                self.reqHistoricalData(
                    i, 
                    contract, 
                    "", 
                    histo_length, 
                    bar_type, 
                    data_type, 
                    0, 1, False, []
                    )
            except:
                ut.printBanner("Fail, moving on to next asset")
        Timer(5, self.stop).start()

    def stop(self):
        time.sleep(1)
        self.done = True
        self.disconnect()

####################################################################################################
### Sub routines
####################################################################################################

@ut.trySimpleNone()
def getDBAssetData():
    
    account_specific = """WHERE V.attribute = 'have_ib_histo' AND U.value = 1
        AND T.asset_class NOT IN ('fx_dm', 'fx_em')"""
    if ACCOUNT_ID == 2:
        account_specific = """WHERE T.asset_class = 'fx_dm'
            OR (T.asset_class = 'fx_em' AND V.attribute = 'have_ib_histo' AND U.value = 1)"""
    
    sql_q = f"""SELECT CONCAT(A.code, C.ccy) AS pair, T.asset_class, M.market, 
                X.ib_symbol, Y.ib_exchange
            FROM static_instrument I
            LEFT JOIN static_currency C ON C.ccy_id = I.ccy_id
            LEFT JOIN static_asset A ON A.asset_id = I.asset_id
            LEFT JOIN static_asset_class T ON T.asset_class_id = I.asset_class_id
            LEFT JOIN static_market M ON M.market_id = I.market_id
            LEFT JOIN static_instrument_attribute_int U on U.instrument_id = I.instrument_id
            LEFT JOIN static_instrument_attribute_type V ON V.attribute_id = U.attribute_id
            LEFT JOIN (
                SELECT instrument_id, value AS ib_symbol 
                FROM static_instrument_attribute_chr 
                WHERE attribute_id = 4
            ) X ON X.instrument_id = I.instrument_id
            LEFT JOIN (
                SELECT M.instrument_id, N.ib_exchange
                FROM
                	(SELECT instrument_id, value as exchange_id
                	FROM static_instrument_attribute_int
                	WHERE attribute_id = 3
                	) M
                LEFT JOIN static_exchange N ON N.exchange_id = M.exchange_id
            ) Y ON Y.instrument_id = I.instrument_id
            {account_specific}
            ORDER BY pair"""
            
    df = db.select(sql_q)
    df = df.merge(init.INSTRUMENTS[["pair", "is_etf"]], on="pair", how="left")
   # df = df[df.asset_class.isin(["fx_dm","fx_em","metal"])]
    return df

def formatAssetDat(dat):
    pos_index = dat["asset_class"] == "index"    
    pos_metal = dat["asset_class"] == "metal"
    pos_etf = dat["is_etf"] == 1

    dat["symbol"] = dat["pair"].apply(lambda x: x[:3])
    dat["symbol"][pos_index] = dat["ib_symbol"][pos_index]
    dat["symbol"][pos_metal] = dat["pair"][pos_metal]

    dat["secType"] = "CASH"
    dat["secType"][pos_index] = "IND"
    dat["secType"][pos_metal] = "CMDTY"
    dat["secType"][pos_etf] = "STK"

    dat["exchange"] = "IDEALPRO"
    dat["exchange"][pos_index] = dat["ib_exchange"][pos_index]
    dat["exchange"][pos_metal] = dat["ib_exchange"][pos_metal]
    dat["exchange"][pos_etf] = "SMART"

    pair_nyse = ["AGGUSD", "BNDUSD", "BSVUSD", "EMBUSD", "HYDUSD", "HYGUSD", "IEFUSD", "JNKUSD",
                 "LQDUSD", "MBBUSD", "TIPUSD", "TLTUSD", "VCIUSD", "VTIUSD"]
    dat["exchange"][dat.pair.isin(pair_nyse)] = "NYSE"
    
    dat["currency"] = dat["pair"].apply(lambda x: x[-3:])
    
    dat["data_type"] = "MIDPOINT"
    dat["data_type"][pos_index] = "TRADES"

    dat = dat[["pair","symbol", "secType", "exchange", "currency", "data_type"]]
    return dat

@ut.tryDiagnosticNone()
def buildAssetList():
    dat = getDBAssetData()
    dat = formatAssetDat(dat)
    return dat

def determineDateFormat():
    date_format = "%Y%m%d"
    if "hour" in bar_type:
        date_format = "%Y%m%d  %H:%M:%S"
    return date_format

def prepareContract(this_pair, asset_list):
    this_asset = asset_list[asset_list["pair"] == this_pair]
    contract = Contract()
    contract.symbol = this_asset["symbol"].values[0]
    contract.secType = this_asset["secType"].values[0]
    contract.exchange = this_asset["exchange"].values[0]
    contract.currency = this_asset["currency"].values[0]
    return contract

def convertIBMessageToDF(data):
    try:
        return pd.DataFrame(data, columns=["pair", "date", "open", "high", "low", "close"])
    except:
        return []

def eraseAllPreviousFiles():
    file_list = os.listdir(file_path)
    for this_file in file_list:
        os.remove(file_path + this_file)
    
####################################################################################################
### Script
####################################################################################################
if __name__ == "__main__":
    # eraseAllPreviousFiles()
    ib_app = ibApp()
    ib_app.connect("127.0.0.1", xu.getIBPort(ACCOUNT_ID), xu.getIBClientId(script_name))      
    ib_app.run()    
    
    init.scriptFinish(script_name)