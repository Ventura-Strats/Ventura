#!/usr/bin/env python3
# -*- coding: utf-8 -*-

####################################################################################################
### Script description
####################################################################################################
# Gets live prices from instruments we have in book, mark-to-market valuation price
####################################################################################################
script_name = "Price_Book"

####################################################################################################
### Imports
####################################################################################################
from ibapi.client import EClient
from ibapi.wrapper import EWrapper
import pandas as pd
import execution_utils as xu

####################################################################################################
### Script constants
####################################################################################################
CLIENT_ID = xu.getIBClientId(script_name)

####################################################################################################
### Classes
####################################################################################################
class IBAppBookPriceAndPositions(EWrapper, EClient):
    def __init__(self, account_id):
        EClient.__init__(self, self)
        self.px_position = prepareContractTable()
        self.account_id = account_id
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        print("Error ", reqId, " ", errorCode, " ", errorString)
        if errorCode == 502:
            print("Error - Not connected - exiting")
            self.stop()

    def nextValidId(self, orderId):
        self.start()
               
    def updatePortfolio(self, contract, position, marketPrice, marketvalue, 
                        averageCost, unrealizedPNL, realizedPNL, accountName):
        dat = prepareContractTable([contract], [marketPrice], [position])
        self.px_position = self.px_position.append(dat)
        
    def accountDownloadEnd(self, account):
        self.finalFormatting()
        self.stop()
        
    def finalFormatting(self):
        self.px_position["account_id"] = self.account_id
        self.px_position["conid"] = self.px_position["contract"].map(lambda x: x.conId)
        self.px_position = self.px_position[["account_id", "conid", "price", "position"]]

    def start(self):
        self.reqAccountUpdates(True, "")

    def stop(self):
        self.disconnect()
        
####################################################################################################
### Sub routines
####################################################################################################
def prepareContractTable(contract = [], price = [], position = []):
    return pd.DataFrame({"contract":contract, "price":price, "position":position})

def getBookLivePriceAndPositions(account_id):
    ib_app = IBAppBookPriceAndPositions(account_id)
    ib_app.connect("127.0.0.1", xu.getIBPort(account_id), CLIENT_ID)
    ib_app.run()    
    return ib_app.px_position

def remarkPriceAsAverageOfTwoBooks(dat):
    px = dat.groupby("conid", as_index=False)["price"].mean()
    dat = dat.drop("price", axis = 1).merge(px, on = "conid", how = "left")
    return dat

def combineInformationFromBothBooks(px_position_1, px_position_2):
    px_position = pd.concat([px_position_1, px_position_2])
    px_position = remarkPriceAsAverageOfTwoBooks(px_position)
    return px_position

def getPricePositions():
    px_position_1 = getBookLivePriceAndPositions(1)
    px_position_2 = getBookLivePriceAndPositions(2)
    px_position = combineInformationFromBothBooks(px_position_1, px_position_2)
    return px_position

def getLivePriceFromBook():
    px_position = getPricePositions()
    return px_position[["conid", "price"]].drop_duplicates()

