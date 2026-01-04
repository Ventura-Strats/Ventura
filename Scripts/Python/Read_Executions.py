#!/usr/bin/env python3
# -*- coding: utf-8 -*-
####################################################################################################
### Description: Read trades done today
####################################################################################################
script_name = "Read_Executions"

####################################################################################################
### Import Libraries
####################################################################################################
from datetime import datetime
import pandas as pd
from threading import Timer
from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.execution import ExecutionFilter
import utils as vu
import db
import init
import execution_utils as vx

TZ_IB = "Asia/Hong_Kong"
TZ_LOCAL = "Europe/London"

####################################################################################################
### Initialization
####################################################################################################
init.scriptInit(script_name)

####################################################################################################
### Script variables and parameters
####################################################################################################
ACCOUNT_ID_LIST = [1,2]
FUTURE_EXPIRY = db.loadTableLocal("future_expiry")
ETF_PROXY = db.loadTableLocal("etf_proxy")[["instrument_id", "conid"]]
ETF_DIRECT = db.select("""SELECT instrument_id, value AS conid 
                       FROM static_instrument_attribute_int 
                       WHERE attribute_id = 7""")
ETF = pd.concat([ETF_PROXY, ETF_DIRECT])
PATH_EXECUTIONS = init.DIRECTORY_DATA + "Executions_IBAPI/"
FILE_NAME_LAST = "executions_last.csv"
FILE_NAME_DATED = "executions_{}.csv".format(init.start_time.strftime("%Y%m%d_%H%M%S"))

####################################################################################################
### Classes
####################################################################################################
class IBAppExec(EWrapper, EClient):
    def __init__(self, account_id):
        EClient.__init__(self, self)
        self.account_id = account_id
        self.executions = pd.DataFrame()

    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        print("Error ", reqId, " ", errorCode, " ", errorString)
        if errorCode == 502:
            print("Error - Not connected - exiting")
            self.stop()
    
    def nextValidId(self, orderId):
        self.start()
    
    def execDetails(self, reqId, contract, execution):
        print(execution)
        this_exec = prepareExecutionSummary(contract, execution, self.account_id)
        self.executions = pd.concat([self.executions, this_exec])

    def start(self):
        self.reqExecutions(1, ExecutionFilter())
        Timer(2, self.stop).start()
        
    def stop(self):
        self.executions = keepOnlyLastRowPerExecution(self.executions)
        self.disconnect()

####################################################################################################
### Sub routines
####################################################################################################
@vu.tryDiagnosticNone()
def matchInstrumentIdToPair(contract):
    fx_pair = contract.symbol + contract.currency
    try:
        instrument_id = init.INSTRUMENTS[
            init.INSTRUMENTS["pair"] == fx_pair]["instrument_id"].values[0]
    except:
        instrument_id = None
    return instrument_id

@vu.tryDiagnosticNone()
def matchFutureConIdToConId(contract):
    if contract.conId in list(FUTURE_EXPIRY["conid"]):
        conid = contract.conId
    else:
        conid = None
    return conid

@vu.tryDiagnosticNone()
def matchStockConidToInstrument(contract):
    if contract.conId in list(ETF["conid"]):
        conid = contract.conId
    else:
        conid = None
    return conid

@vu.tryDiagnosticNone()
def findIdentifierFromContract(contract):
    if contract.secType == "CASH":
        identifier = matchInstrumentIdToPair(contract)
    elif contract.secType == "FUT":
        identifier = matchFutureConIdToConId(contract)
    elif contract.secType == "STK":
        identifier = matchStockConidToInstrument(contract)
    return identifier

@vu.tryDiagnosticNone()
def findBuySell(ib_buy_sell):
    if ib_buy_sell == "BOT":
        return int(1)
    elif ib_buy_sell == "SLD":
        return int(-1)
    else:
        return None
    
@vu.tryDiagnosticNone()
def formatExecutionTime(execution):
    print("Formatting")
    print(execution.time)
    print(1)
    dt = datetime.strptime(execution.time, "%Y%m%d  %H:%M:%S Asia/Hong_Kong")
    print(2)
    print(dt)
    dt_local = vu.convertDateTimeFromTZToAnotherTZ(dt, TZ_IB, TZ_LOCAL)
    print(3)
    print(dt_local)
    return dt_local

@vu.tryDiagnosticNone()
def prepareExecutionSummary(contract, execution, account_id):
    df = pd.DataFrame()
    if contract.secType in ["FUT","CASH","STK"]:
        df = pd.DataFrame({
            "account_id": [account_id],
            "ib_trade_id": [execution.permId],
            "ib_exec_id": [execution.execId],
            "ib_order_id": [execution.orderId],
            "identifier": [findIdentifierFromContract(contract)],
            "timestamp": [formatExecutionTime(execution)],
            "conid": [contract.conId],
            "symbol": [contract.symbol],
            "sec_type": [contract.secType],
            "ccy": [contract.currency],
            "buy_sell": [findBuySell(execution.side)],
            "size": [float(execution.shares)],
            "price": [float(execution.price)]
            })
    return df

@vu.tryDiagnosticDat()
def keepOnlyLastRowPerExecution(df):
    vu.printBanner("Keeping only last row. Before: ")
    print(df)
    if (len(df) > 0):
        df = df \
            .sort_values(["ib_exec_id", "ib_trade_id", "timestamp"]) \
            .groupby("ib_exec_id") \
            .tail(1) \
            .drop_duplicates() \
            .sort_values("ib_exec_id") \
        .reset_index(drop=True)
    vu.printBanner("Keeping only last row. after: ")
    print(df)
    return df

@vu.tryDiagnosticNone()        
def rearrangeRows(df):
    return df.sort_values(["identifier", "account_id"])

@vu.tryDiagnosticNone()
def saveToFiles(df):
    df.to_csv(PATH_EXECUTIONS + FILE_NAME_DATED, index=False)
    df.to_csv(PATH_EXECUTIONS + FILE_NAME_LAST, index=False)
    
@vu.tryDiagnosticNone()    
def saveToDB(df):
    vu.printBanner("AAA-1")
    print(df)
    sql_q = """SELECT DISTINCT ib_exec_id 
        FROM book_trade_leg 
        WHERE timestamp >= '{}'""" \
            .format(init.YESTERDAY.strftime("%Y-%m-%d %H:%M:%S"))
    df_already_in_db = db.select(sql_q)
    vu.printBanner("AAA-2")
    print(df_already_in_db)
    pos_keep = set(df.ib_exec_id).difference(df_already_in_db.ib_exec_id)
    vu.printBanner("AAA-3")
    print(pos_keep)
    max_leg_id = int(db.select("SELECT MAX(leg_id) AS leg_id FROM book_trade_leg").leg_id[0])
    vu.printBanner("AAA-4")
    print(max_leg_id)
    df = df[df.ib_exec_id.isin(pos_keep)]
    vu.printBanner("AAA-5")
    print(df)
    if (len(df) > 0):
        vu.printBanner("AAA-6")
        df = df \
            .assign(
                leg_id = range(max_leg_id+1, max_leg_id+1+len(df)),
                fees = 0,
                fees_ccy_id = 1,
                tax = 0
                ) \
            [["leg_id", "ib_trade_id", "ib_exec_id", "account_id", 
                 "identifier", "timestamp", "buy_sell", "size", "price",
                 "fees", "fees_ccy_id", "tax"
                 ]] 
        vu.printBanner("AAA-7")
        print(df)
        df = df \
            .dropna(subset = ["identifier"])
        vu.printBanner("AAA-8")
        print(df)
        db.appendToTable("book_trade_leg", df)
        vu.printBanner("AAA-8")

####################################################################################################
### Main script
####################################################################################################
@vu.tryDiagnosticDat()
def readExecutions(account_id):
    vu.printBanner(f"Doing this account: {account_id}")
    ib_app = IBAppExec(account_id)
    ib_app.connect("127.0.0.1", vx.getIBPort(account_id), vx.getIBClientId(script_name))
    ib_app.run() 
    return ib_app.executions

if __name__ == "__main__":
    dat = pd.DataFrame()
    for i in ACCOUNT_ID_LIST:
        dat = pd.concat([dat, readExecutions(i)])
    dat = rearrangeRows(dat)    
    saveToFiles(dat)
    saveToDB(dat)
    
    init.scriptFinish(script_name)