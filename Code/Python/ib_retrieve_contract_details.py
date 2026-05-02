#!/usr/bin/env python3
# -*- coding: utf-8 -*-

####################################################################################################
### Script description
####################################################################################################
### Given a list of simple IB contracts, retrieves the full contracts with all information
####################################################################################################
script_name = "Contract_Details"

####################################################################################################
### Imports
####################################################################################################
#from threading import Timer
import pandas as pd
#import time 
from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract
#from ibapi.contract import ContractDetails
import utils as vu
import execution_utils as xu

####################################################################################################
### Script Parameters
####################################################################################################

####################################################################################################
### Script constants
####################################################################################################
ACCOUNT_ID = 2
ib_port = xu.getIBPort(ACCOUNT_ID)
client_id = xu.getIBClientId(script_name)

####################################################################################################
### Script initialization
####################################################################################################

####################################################################################################
### Classes
####################################################################################################
class IBAppRetrieveContracts(EWrapper, EClient):
    def __init__(self, conid_list):
        EClient.__init__(self, self)
        self.conid_list = conid_list
        self.nb_items = len(self.conid_list)
        self.contract_details = [[] for i in range(self.nb_items)]
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
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
### Sub routines
####################################################################################################

@vu.trySimpleNone()
def fetchContractDetails(conid_list):
    ib_app = IBAppRetrieveContracts(conid_list)
    ib_app.connect("127.0.0.1", ib_port, client_id)
    ib_app.run()    
    contract_details = pd.DataFrame({
        "conid":conid_list, 
        "contract_details":ib_app.contract_details
        })
    return contract_details