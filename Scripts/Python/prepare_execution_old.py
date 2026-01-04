#!/usr/bin/env python3
# -*- coding: utf-8 -*-

####################################################################################################
### Script description
####################################################################################################
# This function takes the order list ("I want to buy 123 IBM"), works out the live price, 
# and prepares the order price we're going to send. 
# - Gets the best live price estimate we can
# - Check if bid-offer is the tick-size in which case we just hit the bid-offer
# - Computes the maximum price limit we want to use for this order. 
####################################################################################################
### Import Libraries
####################################################################################################
import ib_routines as vi
import utils as vu
import init
import db
from datetime import timedelta

####################################################################################################
### Script variables and parameters
####################################################################################################
LIMIT_EXECUTION_PRICE_FROM_INITIAL_MID_PCT = 0.03
INITIAL_ORDER_TICKS_FROM_MID = 2
TIME_THRESHOLD_DB_PX_VS_NOW = 10

####################################################################################################
### Script constants
####################################################################################################


####################################################################################################
### Sub routines
####################################################################################################

@vu.tryDiagnosticDat()
def addLivePriceAndPositionForStocksInBook(dat): 
    dat_book = vi.getPricePositions()
    dat_px = dat_book[["conid", "price"]].drop_duplicates()    
    dat_position = dat_book[["account_id", "conid", "position"]]
    
    dat = dat.merge(dat_position, on = ["account_id", "conid"], how = "left")
    dat = dat.fillna({"position":0})
    
    dat = dat.merge(dat_px, on = "conid", how = "left")

    dat = dat.rename({"position":"initial_position", "price":"px_book"}, axis = 1)   
    dat = dat.dropna(axis = 1)

    return dat

@vu.tryDiagnosticDat()  
def addIBLivePrice(dat):
    dat_px = vi.getIBLivePrice(list(dat["contract"]))
    dat_px = dat_px.drop(["px_bid", "px_ask", "px_mid", "px_last"], axis = 1)
    dat = dat.merge(dat_px, on = "conid", how = "left")
    return dat

@vu.tryDiagnosticDat()
def addLastPriceFromDB(dat):
    timestamp_limit = init.start_time() - timedelta(minutes = TIME_THRESHOLD_DB_PX_VS_NOW)
    sql_q = f"""SELECT instrument_id, close AS px_db
        FROM live_px
        WHERE timestamp >= '{timestamp_limit}'"""
    dat_px = db.select(sql_q)
    dat = dat.merge(dat_px, on = "instrument_id", how = "left")
    return dat

def setRemainingEmptyPricesToValue(dat, px_name):
    pos_none = dat["px_live"].isnull()
    if px_name in dat.columns:
        dat["px_live"][pos_none] = dat[px_name][pos_none]
    return dat

def determineWhichPriceToUseAsLive(dat):
    dat["px_live"] = None
    dat = setRemainingEmptyPricesToValue(dat, "px_ib")
    dat = setRemainingEmptyPricesToValue(dat, "px_book")
    dat = setRemainingEmptyPricesToValue(dat, "px_db")   
    return dat

@vu.tryDiagnosticNone()
def getLivePrices(dat):
    dat = addLivePriceAndPositionForStocksInBook(dat)
    dat = addIBLivePrice(dat)
    dat = addLastPriceFromDB(dat)
    dat = determineWhichPriceToUseAsLive(dat)
    return dat

@vu.tryDiagnosticDat()
def removeOrdersWithoutPrice(dat):
    # Temporary for now
    dat = dat[dat["px_live"].notnull()]
    return dat

def roundPriceToClosestTick(dat, px_col):
    dat = dat.assign(px_tick = vu.mround(dat[px_col], dat["tick_size"]))
    dat[px_col] = dat["px_tick"]
    dat.drop("px_tick", axis=1, inplace=True)
    return dat    

def moveInitialPriceAFewTicks(dat, px_order_col):
    dat["px_order_adj"] = dat["px_order_name"]
    dat = dat.assign(
        px_order_adj = dat[px_order_col] 
        - INITIAL_ORDER_TICKS_FROM_MID * dat["buy_sell"] * dat["tick_size"]
    )
    dat[px_order_col] = dat["px_order_adj"]
    dat.drop("px_order_adj", axis=1, inplace=True)
    return dat

@vu.tryDiagnosticNone()
def prepareInitialOrderPrice(dat):
    pos_na = dat["px_order"].isnull()
    dat["px_order"][pos_na] = dat["px_live"][pos_na]
    dat = moveInitialPriceAFewTicks(dat, "px_order")
    return dat

@vu.tryDiagnosticNone()    
def prepareOrderPriceLimit(dat):
    dat = dat.assign(
        px_limit = dat["px_order"] 
            * (1 + dat["buy_sell"] * LIMIT_EXECUTION_PRICE_FROM_INITIAL_MID_PCT)
       )
    
    pos_beyond_limit = (
        (dat["buy_sell"] * dat["px_order"]) 
            > (dat["buy_sell"] * dat["px_limit"])
        )
        
    dat["px_order_initial"][pos_beyond_limit] = dat["px_limit"][pos_beyond_limit]
    return dat

@vu.trySimpleNone()   
def roundOrderSize(dat):
    # Need to be much more comprehensive
    dat = dat.assign(size = round(dat["size"]-0.25, 0))
    dat = dat[dat["size"] > 0]
    return dat
    
@vu.trySimpleNone()   
def removeNegativeRemainingSizeToDo(dat):
    dat["remaining"][dat["remaining"] < 0] = 0
    return dat

@vu.trySimpleNone()    
def removeDuplicatedStockIdRows(dat):
    dat.drop_duplicates(subset ="pair", keep=False, inplace=True) 
    return dat

@vu.tryDiagnosticNone()
def finalFormatting(dat):
    if "filled" not in dat.columns:
        dat["filled"] = 0

    dat = roundPriceToClosestTick(dat, "px_order")
    dat = roundPriceToClosestTick(dat, "px_limit")
    dat = dat.assign(
        remaining = dat["size"] - dat["filled"],
        px_avg = 0
        )
    
    dat = removeNegativeRemainingSizeToDo(dat)   
    dat = dat.drop("account_id", axis = 1)   
    return dat
    
####################################################################################################
### Main Script
####################################################################################################

@vu.trySimpleNone()
def prepareExecution(dat, account_id):
    global ACCOUNT_ID
    ACCOUNT_ID = account_id
    dat = getLivePrices(dat)
    dat = removeOrdersWithoutPrice(dat)
    dat = prepareInitialOrderPrice(dat)
    dat = prepareOrderPriceLimit(dat)
    dat = roundOrderSize(dat)
    dat = finalFormatting(dat)
    return dat