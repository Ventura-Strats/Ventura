#!/usr/bin/env python3
# -*- coding: utf-8 -*-
####################################################################################################
### Script Description
####################################################################################################
# Retrieves live mid, bid, offer from IB when we have access (FX and some indices/futures)
####################################################################################################
script_name = "Price_Live_IB_Exec"

####################################################################################################
### Imports
####################################################################################################
import pandas as pd
from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.ticktype import TickTypeEnum
import execution_utils as xu

####################################################################################################
### Script constants
####################################################################################################
ACCOUNT_ID = 1
IB_PORT = xu.getIBPort(ACCOUNT_ID)
IB_CLIENT_ID = xu.getIBClientId(script_name)

####################################################################################################
### Classes
####################################################################################################
class IBAppRetrieveLivePrice(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        print("Error ", reqId, " ", errorCode, " ", errorString)
        if errorCode == 502:
            print("Error - Not connected - exiting")
            self.stop()

    def nextValidId(self, orderId):
        self.start()
        
    def tickPrice(self, reqId, tickType, price, attrib):
        tick_type = TickTypeEnum.to_str(tickType)
        if tick_type == "BID":
            self.px_bid[reqId] = price
        elif tick_type == "ASK":
            self.px_ask[reqId] = price
        elif tick_type == "LAST":
            self.px_last[reqId] = price
            self.px[reqId] = price        
        self.determineBestUsablePrice(reqId)
        
    def haveBidOffer(self, i):
        return (self.px_bid[i] is not None) and (self.px_ask[i] is not None)
    
    def haveMidAndLast(self, i):
        return (self.px_mid[i] is not None) and (self.px_last[i] is not None)
    
    def determineBestUsablePrice(self, i):
        if self.haveBidOffer(i):
            self.px_mid[i] = 0.5 * (self.px_bid[i] + self.px_ask[i])
            self.px[i] = self.px_mid[i]
            
        if self.haveMidAndLast(i):
            self.px[i] = 0.5 * (self.px_mid[i] + self.px_last[i])

    def start(self):
        for i in range(self.nb_items):
            contract = self.contract_list[i]
            if contract is not None:
                self.reqMktData(i, contract, "", False, False, [])

    def stop(self):     
        self.disconnect()
        
####################################################################################################
### Sub routines
####################################################################################################    
def getIBLivePrice(contract_list):
    ib_app = IBAppRetrieveLivePrice()
    if type(contract_list) is not list:
        contract_list = [contract_list]
    ib_app.contract_list = contract_list
    ib_app.nb_items = len(contract_list)
    ib_app.conid_list = [contract.conId for contract in contract_list]
    none_list = [None for i in range(ib_app.nb_items)]
    ib_app.px = none_list
    ib_app.px_bid = none_list
    ib_app.px_ask = none_list
    ib_app.px_mid = none_list
    ib_app.px_last = none_list
    ib_app.connect("127.0.0.1", IB_PORT, IB_CLIENT_ID)
    ib_app.run()    
    dat_px = pd.DataFrame({
        "conid":ib_app.conid_list, 
        "px_ib":ib_app.px,
        "px_bid":ib_app.px_bid,
        "px_ask":ib_app.px_ask,
        "px_mid":ib_app.px_mid,
        "px_last":ib_app.px_last
        })
    return dat_px
    