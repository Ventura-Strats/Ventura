#!/usr/bin/env python3
# -*- coding: utf-8 -*-

####################################################################################################
### Script description
####################################################################################################
# Gets live prices from instruments we have in book, mark-to-market valuation price
####################################################################################################
SCRIPT_NAME = "Future_Expiry"

####################################################################################################
### Imports
####################################################################################################
from datetime import datetime
from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract
#from ibapi.contract import ContractDetails
import pandas as pd
import execution_utils as xu
import init
import db
import utils as vu

####################################################################################################
### Script initialization
####################################################################################################
init.scriptInit(SCRIPT_NAME)

####################################################################################################
### Script constants
####################################################################################################
ACCOUNT_ID = 2
CLIENT_ID = 117# xu.getIBClientId(script_name)
FUTURES = db.loadTableLocal("future_contract")

####################################################################################################
### Classes
####################################################################################################
class IBAppFutureExpiries(EWrapper, EClient):
    def __init__(self, contract_list):
        EClient.__init__(self, self)
        self.contract_list = contract_list
        nb_contracts = len(self.contract_list)
        self.conid = [[] for i in range(nb_contracts)]
        self.expiry = [[] for i in range(nb_contracts)]
        self.notional = [[] for i in range(nb_contracts)]
        self.contract_done = [False for i in range(nb_contracts)]

    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        print("Error ", reqId, " ", errorCode, " ", errorString)
        if errorCode == 502:
            print("Error - Not connected - exiting")
            self.stop()
        if errorCode == 200:
            print(f"Error - No securitiess found for {self.contract_list[reqId].symbol}")
            self.contract_done[reqId] = True
    
    def nextValidId(self, orderId):
        self.start()
                       
    def contractDetails(self, reqId, contractDetails):
        contract = contractDetails.contract
        expiry = datetime.strptime(contract.lastTradeDateOrContractMonth, "%Y%m%d")
        self.expiry[reqId].append(expiry)
        self.conid[reqId].append(contract.conId)
        self.notional[reqId].append(float(contract.multiplier))

    def contractDetailsEnd(self, reqId):
        self.contract_done[reqId] = True
        print("Remaining:")
        print([self.contract_list[i].symbol for i, x in enumerate(self.contract_done) if not x])
        self.wrapUpIfAllComplete(reqId)
            
    def wrapUpIfAllComplete(self, reqId):
        if all(self.contract_done):
            self.stop()

    def start(self):
        i = 0
        for contract in self.contract_list:
            self.reqContractDetails(i, contract)
            i += 1

    def stop(self):
        self.disconnect()
        
####################################################################################################
### Sub routines
####################################################################################################
def prepareContractList():
    dat = FUTURES.merge(init.CURRENCIES, on="ccy_id", how="left")
    EXCHANGES = db.loadTableLocal("exchange")
    dat = dat.merge(EXCHANGES, on="exchange_id", how="left")
    
    contract_list = []
    for i in range(len(dat)):
        contract = Contract()
        contract.symbol = dat["ib_symbol"].values[i]
        contract.secType = "FUT"
        contract.exchange = dat["ib_exchange"].values[i]
        contract.currency = dat["ccy"].values[i]
        contract_list.append(contract)
    return contract_list

def keepOnlyTheContractsWithTheRightNotional(i, dat_i):
    this_notional = FUTURES["notional"].values[i]
    dat_i = dat_i[dat_i["notional"] == this_notional]
    return dat_i

def finalFormatting(contract_list, conid, expiry, notional):
    dat = pd.DataFrame(columns = ["future_id", "conid", "expiry", "notional"])
    for i in range(len(contract_list)):
        dat_i = pd.DataFrame({
            "future_id":FUTURES["future_id"].values[i],
            "conid":conid[i],
            "expiry":expiry[i],
            "notional":notional[i]
            })
        dat_i = keepOnlyTheContractsWithTheRightNotional(i, dat_i)
        dat = dat.append(dat_i)
    return dat
    
def getFutureExpiries():
    contract_list = prepareContractList()
    ib = IBAppFutureExpiries(contract_list)
    ib.connect("127.0.0.1", xu.getIBPort(ACCOUNT_ID), xu.getIBClientId(SCRIPT_NAME))
    ib.run()
    print("run done")
    dat = finalFormatting(contract_list, ib.conid, ib.expiry, ib.notional)
    dat.to_csv("future_expiries.csv",index=False)
    return dat

if __name__ == "__main__":
    getFutureExpiries()   
    init.scriptFinish(SCRIPT_NAME)