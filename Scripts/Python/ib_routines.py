#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug  6 15:30:07 2020

@author: fls
"""
import ib_retrieve_contract_details 
import ib_retrieve_live_price_from_both_books
import ib_retrieve_live_price


def fetchContractDetails(contract_list):
    return ib_retrieve_contract_details.fetchContractDetails(contract_list)

def getLivePriceFromBook():
    return ib_retrieve_live_price_from_both_books.getLivePriceFromBook()

def getLivePriceAndPositionsFromBook():
    return ib_retrieve_live_price_from_both_books.getPricePositions()

def getIBLivePrice(contract_list):
    return ib_retrieve_live_price.getIBLivePrice(contract_list)

