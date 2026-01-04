#!/usr/bin/env python3
# -*- coding: utf-8 -*-

####################################################################################################
### Retrieves account information (NAV, positions, etc) from IB
### And saves it into csv files because we don't know how to export to DB for now
####################################################################################################
script_name = "IB_Account_Data"
max_time_hours = 1 / 60

####################################################################################################
### Imports
####################################################################################################
from ibapi.wrapper import EWrapper
from ibapi.client import EClient
import init
import utils as ut
import execution_utils as xu
import db
import pandas as pd

####################################################################################################
### Script initialization
####################################################################################################
init.scriptInit(script_name, max_time_hours)

####################################################################################################
### Script variables
####################################################################################################
account_id_list = [1,2]
file_path = init.DIRECTORY_DATA + "Account_Data/"
file_path_account = file_path + "Account/"
file_path_px_position = file_path + "Px_Position/"
columns_px_position = ["account_id", "conid", "symbol", "secType", "primaryExchange", 
    "currency", "price", "position"]

####################################################################################################
### Classes
####################################################################################################
class IBAppBook(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
        self.account_data = pd.DataFrame()
        self.px_position = pd.DataFrame()

    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        print("IB Error ", reqId, " ", errorCode, " ", errorString)
        if errorCode == 502:
            ut.printBanner("Error - Not connected - exiting", False)
            self.stop()
    
    def nextValidId(self, orderId):
        self.start()
        
    def addPxPositionRow(self, contract, price, position):
        dat = pd.DataFrame({
            "contract":[contract],
            "price":[price],
            "position":[position]
            })
        self.px_position = pd.concat([self.px_position, dat])
               
    def addAccountDataRow(self, key, value, ccy):
        dat = pd.DataFrame({
            "key":[key],
            "value":[value],
            "ccy":[ccy]
            })
        self.account_data = pd.concat([self.account_data, dat])
        
    def updatePortfolio(self, contract, position, marketPrice, marketValue, 
            averageCost, unrealizedPNL, realizedPNL, accountName):
        self.addPxPositionRow(contract, marketPrice, position) 
        
    def updateAccountValue(self, key, val, currency, accountName):
        self.addAccountDataRow(key, val, currency)
    
    def accountDownloadEnd(self, account):
        self.finalFormatting()
        self.stop()
        
    def finalFormatting(self):
        self.px_position = formatPxPositionTable(self.px_position, self.account_id)

    def start(self):
        self.reqAccountUpdates(True, "")
        
    def stop(self):
        self.disconnect()

####################################################################################################
### Sub routines
####################################################################################################
def correctFTSEPrice(dat):
    # TO BE CORRECTED WITH A 'MULTIPLIER' PARAMETER IN static_future_contract
    pos_ftse = (dat["symbol"] == "Z")
    dat.price[pos_ftse] = 100 * dat.price[pos_ftse]
    return dat

def formatPxPositionTable(dat, account_id):
    if len(dat) > 1:
        dat = dat \
            .assign(
                account_id = account_id,
                conid = dat.contract.map(lambda x: x.conId),
                symbol = dat.contract.map(lambda x: x.symbol),
                secType = dat.contract.map(lambda x: x.secType),
                primaryExchange = dat.contract.map(lambda x: x.primaryExchange),
                currency = dat.contract.map(lambda x: x.currency)
                ) \
            [columns_px_position]
        dat = correctFTSEPrice(dat)
    return dat

@ut.tryDiagnosticDat()
def saveAccountFile(dat, account_id):
    file_name = f"account_{account_id}_{init.char_start_time}.csv"
    dat.to_csv(file_path_account + file_name, index=False)
    file_name = f"account_{account_id}_last.csv"
    dat.to_csv(file_path_account + file_name, index=False)
    return dat

@ut.tryDiagnosticDat()    
def savePxPositionFile(dat, account_id):
    file_name = f"px_position_{account_id}_{init.char_start_time}.csv"
    dat.to_csv(file_path_px_position + file_name, index=False)
    file_name = f"px_position_{account_id}_last.csv"
    dat.to_csv(file_path_px_position + file_name, index=False)
    return dat

def tryToFindQuoteInUSD(dat_px_position, ccy_account):
    return dat_px_position[
        (dat_px_position["secType"] == "CASH")
        & (dat_px_position["symbol"] == ccy_account)
        & (dat_px_position["currency"] == "USD")
    ]

def tryToConvertFXRateToUSDUsingAccount(dat_account, ccy_account):
    fx_account = float(dat_account[
        (dat_account["key"] == "ExchangeRate")
        & (dat_account["ccy"] == ccy_account)
    ]["value"].values[0])
    fx_usd = float(dat_account[
        (dat_account["key"] == "ExchangeRate")
        & (dat_account["ccy"] == "USD")
    ]["value"].values[0])
    return fx_account / fx_usd
    

def findUSDQuotedInCCY(dat_px_position, ccy_account):
    dat_fx = dat_px_position[
        (dat_px_position["secType"] == "CASH")
        & (dat_px_position["symbol"] == "USD")
        & (dat_px_position["currency"] == ccy_account)
    ]
    fx = 1 / float(dat_fx["price"].values[0])
    return fx

def findFXinPxPosition(dat_px_position, account_id):
    ACCOUNTS = db.loadTableLocal("account")
    ccy_id_account = ACCOUNTS["ccy_id"][ACCOUNTS["account_id"] == account_id].values[0]

    ccy_account = init.CURRENCIES["ccy"][init.CURRENCIES["ccy_id"] == ccy_id_account].values[0]

    dat_fx = tryToFindQuoteInUSD(dat_px_position, ccy_account)
    if len(dat_fx) > 0:
        fx = float(dat_fx["price"].values[0])
    else:
        fx = findUSDQuotedInCCY(dat_px_position, ccy_account)
    return fx

def findFXinAccount(dat_account, account_id):
    ACCOUNTS = db.loadTableLocal("account")
    ccy_id_account = ACCOUNTS["ccy_id"][ACCOUNTS["account_id"] == account_id].values[0]
    ccy_account = init.CURRENCIES["ccy"][init.CURRENCIES["ccy_id"] == ccy_id_account].values[0]
    fx = tryToConvertFXRateToUSDUsingAccount(dat_account, ccy_account)
    return fx

@ut.tryDiagnosticDat()
def saveNAVToDB(dat_account, dat_px_position, account_id):
    nav = float(dat_account["value"][dat_account["key"] == "NetLiquidation"].values[0])
    fx = findFXinAccount(dat_account, account_id)
    nav_usd = nav * fx

    sql_q = """INSERT INTO book_nav
        (account_id, date, timestamp, nav_ccy, nav_usd)
        VALUES ({}, '{}', '{}', {}, {})""".format(
            account_id,
            init.TODAY_STR,
            init.char_start_time,
            nav,
            nav_usd
        )
    print(sql_q)
    db.executeSQL(sql_q)

    sql_q = """SELECT * FROM book_nav 
        WHERE account_id = {} AND date = '{}' AND timestamp = '{}'""".format(
            account_id,
            init.TODAY_STR,
            init.char_start_time
        )
    return db.select(sql_q)

def extractAccountFXData(dat_i, dat_fx):
    dat_fx_i = dat_i[
        (dat_i["key"] == "ExchangeRate") 
            & (dat_i["ccy"] != "BASE")
         ] \
        .assign(value = lambda x: float(x.value))
    fx_usd = dat_fx_i[dat_fx_i["ccy"] == "USD"]["value"].values[0]
    dat_fx_i["value"] = dat_fx_i["value"].apply(lambda x: x / fx_usd)
    return pd.concat([dat_fx, dat_fx_i])

def extractAccountPositionData(dat_i, dat_pos, account_id):
    dat_pos_i = dat_i[
        (dat_i["key"] == "TotalCashBalance")
            & (dat_i["ccy"] != "BASE")
        ] \
        .assign(
            account_id = account_id,
            value = lambda x: float(x.value)
            )
    
    return pd.concat([dat_pos, dat_pos_i])

def formatFXPrices(dat_fx):
    return dat_fx \
        .drop("key", axis=1) \
        .rename({"ccy":"code", "value":"price"}, axis=1) \
        .groupby("code", as_index=False).agg({"price":"mean"}) \
        .merge(init.ASSETS, on="code", how="left") \
        .assign(timestamp = init.char_start_time) \
        [["asset_id", "timestamp", "price"]]

def formatPositions(dat_pos, dat_fx): 
    return dat_pos \
       .drop("key", axis=1) \
       .rename({"ccy":"code", "value":"position"}, axis=1) \
       .merge(init.ASSETS, on="code", how="left") \
       .assign(
           timestamp = init.char_start_time,
           position = lambda x: float(x.position)
           ) \
       [["account_id", "asset_id", "timestamp", "position"]]
    
@ut.tryDiagnosticNone()
def saveFXPxPositionIntoDB():
    dat_fx = pd.DataFrame()
    dat_pos = pd.DataFrame()
    for account_id in account_id_list:
        file_name = "account_{}_{}.csv".format(account_id, init.char_start_time)
        dat_i = pd.read_csv(file_path_account + file_name)        
        dat_fx = extractAccountFXData(dat_i, dat_fx)
        dat_pos = extractAccountPositionData(dat_i, dat_pos, account_id)

    dat_fx = formatFXPrices(dat_fx)
    db.appendToTable("live_px_fx_book", dat_fx)
        
    dat_pos = formatPositions(dat_pos, dat_fx)
    db.appendToTable("book_live_position_fx", dat_pos)
    return [dat_fx, dat_pos]

def extractAccountFutureData(dat_i, dat_fut):
    # dat_fut_i = dat_i[dat_i["secType"] == "FUT"]
    # dat_fut_i["account_id"] = dat_fut_i["account_id"].map(int)
    # dat_fut_i["conid"] = dat_fut_i["conid"].map(int)
    # dat_fut_i["price"] = dat_fut_i["price"].map(float)
    # dat_fut_i["position"] = dat_fut_i["position"].map(float)
    # dat_fut_i = dat_fut_i[["account_id", "conid", "price", "position"]]
    # return dat_fut_i
    
    return dat_i \
        [dat_i["secType"] == "FUT"] \
        .assign(
            account_id = dat_i.account_id.map(int),
            conid = dat_i.conid.map(int),
            price = dat_i.price.map(float),
            position = dat_i.position.map(float)
            ) \
        [["account_id", "conid", "price", "position"]]

def formatFuturePrices(dat):
    dat_px = dat.groupby("conid", as_index=False).agg({"price":"mean"})
    dat_px["timestamp"] = init.char_start_time
    dat_px = dat_px[["conid", "timestamp", "price"]]
    return dat_px

def formatFuturePositions(dat):
    dat["timestamp"] = init.char_start_time
    dat = dat[["account_id", "conid", "timestamp", "position"]]
    return dat

@ut.tryDiagnosticNone()
def saveFuturePxPositionIntoDB():
    dat_fut = pd.DataFrame()
    for account_id in account_id_list:
        file_name = "px_position_{}_{}.csv".format(account_id, init.char_start_time)
        dat_i = pd.read_csv(file_path_px_position + file_name)        
        dat_fut = extractAccountFXData(dat_i, dat_fut)
    
    dat_px = formatFuturePrices(dat_fut)
    db.appendToTable("live_px_future", dat_px)
        
    dat_pos = formatFuturePositions(dat_fut)
    db.appendToTable("book_live_position_future", dat_pos)
    return [dat_px, dat_pos]

@ut.tryDiagnosticNone()
def getBookLivePriceAndPositions(account_id):
    ut.printBanner("Retrieving data for account " + str(account_id))
    ib_app = IBAppBook()
    ib_port = xu.getIBPort(account_id)
    ib_client_id = xu.getIBClientId(script_name)
    ib_app.connect("127.0.0.1", ib_port, ib_client_id)
    ib_app.account_id = account_id
    ib_app.run()    
    global AAA
    AAA = ib_app.px_position
    saveAccountFile(ib_app.account_data, account_id)
    savePxPositionFile(ib_app.px_position, account_id)
    saveNAVToDB(ib_app.account_data, ib_app.px_position, account_id)

####################################################################################################
### Script
####################################################################################################
if __name__ == "__main__":
    for account_id in account_id_list:
        getBookLivePriceAndPositions(account_id)
    saveFXPxPositionIntoDB()
#    saveFuturePxPositionIntoDB()
    init.scriptFinish(script_name)