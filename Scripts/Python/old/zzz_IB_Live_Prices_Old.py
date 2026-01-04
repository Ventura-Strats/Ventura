#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul 29 15:57:29 2020

@author: fls
"""
####################################################################################################
### Imports
####################################################################################################
from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract
import MySQLdb as mysql
import pandas as pd
from datetime import datetime
import time 
from threading import Timer
import os

####################################################################################################
### Script variables
####################################################################################################
bar_type = "1 day"          # "1 hour"
histo_length = "2 W"        # "1 W" "1 M" etc

file_path = "/home/fls/Mount/Glenorchy/FX/Spot/IB/"

script_name = "IB_Live_Prices"

ib_client_id = 102

#process_timeout = 10 #seconds

####################################################################################################
### Script initialization
####################################################################################################
file_computer = open("/home/fls/Data/System/this_computer.txt", "r")
this_computer = file_computer.read(1)
file_computer.close()
      


print("####################################################################################################")
print("### Starting Python script... " + datetime.now().strftime("%Y-%m-%d %H:%M:%S"))
print("### Script Name: " + script_name) 
print("### This Computer: " + this_computer)
print("####################################################################################################")


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
        
    def error(self, reqId , errorCode, errorString):
        try:
            pair_name = str(self.asset_list["pair"][reqId])
        except:
            pair_name = ""
        print("Error: ", pair_name, " ", reqId, " ", errorCode, " ", errorString)
        if errorCode in [162, 200, 354]:
            if not self.ticker_already_covered[reqId]:
                print("Error - Moving on to next asset")
                #self.moveToNextAsset(reqId)
        elif errorCode == 502:
            print("Error - Not connected - exiting")
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
            print(f"file saved - {file_path_name} - {time_now}")
            self.ticker_already_covered[reqId] = True
        finished = all(self.ticker_already_covered)
        if finished:
            print("All done here")
            self.stop()
        else:
            print("Pairs still missing:")
            for i in range(self.nb_assets):
                if not self.ticker_already_covered[i]:
                    print(self.asset_list["pair"].values[i])
   
    def start(self):
        for i in range(self.nb_assets):  
            pair = self.asset_list["pair"].values[i]
            print(f"Doing {pair} ...")
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
                print("Fail, moving on to next asset")
    #    Timer(60, self.stop).start()
    
    def stop(self):
        time.sleep(2)
        self.done = True
        self.disconnect()

####################################################################################################
### Sub routines
####################################################################################################
def determineIBPort():
    file_computer = open("/home/fls/Data/System/this_computer.txt", "r")
    this_computer = file_computer.read(1)
    file_computer.close()
    ib_port = 7496
    if this_computer == "M":
        ib_port = 7498
    print(ib_port)
    return ib_port    

def buildAssetList():
    db_connection = mysql.connect(
        host = "192.168.0.103",
        user = "ventura", 
        passwd = "psuY2oF4qq7B$Lw8U!If", 
        db="Ventura"
    )
    
    sql_q = """SELECT CONCAT(A.code, C.ccy) AS pair, T.asset_class, M.market, 
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
            WHERE (V.attribute = 'have_ib_histo' AND U.value = 1) 
            OR T.asset_class = 'fx_dm'
            ORDER BY pair"""
    dat = pd.read_sql(sql_q, db_connection)
    
    db_connection.close()
    
    pos_index = (dat["asset_class"] == "index")
    pos_metal = (dat["asset_class"] == "metal")

    dat["symbol"] = dat["pair"].apply(lambda x: x[:3])
    dat["symbol"][pos_index] = dat["ib_symbol"][pos_index]
    dat["symbol"][pos_metal] = dat["pair"][pos_metal]

    dat["secType"] = "CASH"
    dat["secType"][pos_index] = "IND"
    dat["secType"][pos_metal] = "CMDTY"

    dat["exchange"] = "IDEALPRO"
    dat["exchange"][pos_index] = dat["ib_exchange"][pos_index]
    dat["exchange"][pos_metal] = dat["ib_exchange"][pos_metal]
    
    dat["currency"] = dat["pair"].apply(lambda x: x[-3:])
    
    dat["data_type"] = "MIDPOINT"
    dat["data_type"][pos_index] = "TRADES"

    dat = dat[["pair","symbol", "secType", "exchange", "currency", "data_type"]]
    
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

def runIBProcess(ib_conn):
    ib_conn.connect("127.0.0.1", determineIBPort(), ib_client_id)        
    ib_conn.run()
    ib_conn.disconnect()
    
####################################################################################################
### Script
####################################################################################################
if __name__ == "__main__":
    # eraseAllPreviousFiles()
    ib_app = ibApp()
    ib_app.connect("127.0.0.1", determineIBPort(), ib_client_id)       
  #  Timer(process_timeout, ib_app.stop).start()
    ib_app.run()    
    ib_app.disconnect()
    
    print("####################################################################################################")
    print("### Script finished... " + datetime.now().strftime("%Y-%m-%d %H:%M:%S"))
    print("####################################################################################################")
