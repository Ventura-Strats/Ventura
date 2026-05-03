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

script_name = "Prepare_Execution"

####################################################################################################
### Import Libraries
####################################################################################################
import ib_routines as vi
import utils as vu
import db
from datetime import datetime
from datetime import timedelta
import os

####################################################################################################
### Script variables and parameters
####################################################################################################
LIMIT_EXECUTION_PRICE_FROM_INITIAL_MID_PCT = 0.0025
INITIAL_ORDER_TICKS_FROM_MID = 2
TIME_THRESHOLD_DB_PX_VS_NOW = 100
TIME_LIMIT_FOR_PX = datetime.now() - timedelta(minutes = TIME_THRESHOLD_DB_PX_VS_NOW)
TIME_LIMIT_FOR_PX = TIME_LIMIT_FOR_PX.strftime("%Y-%m-%d %H:%M:%S")

####################################################################################################
### Script constants
####################################################################################################


####################################################################################################
### Sub routines
####################################################################################################

@vu.tryDiagnosticDat()
def addPositions(dat): 
    dat_book = vi.getLivePriceAndPositionsFromBook()
    dat_position = dat_book[["account_id", "conid", "position"]]
    dat_position["account_id"] = dat_position["account_id"].map(int)
    dat_position["conid"] = dat_position["conid"].map(int)
    dat = dat.merge(dat_position, on = ["account_id", "conid"], how = "left")
    dat = dat.fillna({"position":0})
    dat.rename({"position":"initial_position"}, axis = 1, inplace=True)   
    return dat

def runLivePriceRoutine():
    os.system("RScriptVentura.sh Technicals/Price_Live.R")

@vu.tryDiagnosticDat()
def getLivePricesSpot(dat):
    sql_q = f"""SELECT instrument_id, close AS price_spot 
        FROM live_px 
        WHERE timestamp >= '{TIME_LIMIT_FOR_PX}'"""
    dat_px = db.select(sql_q)
    dat = dat.merge(dat_px, on="instrument_id", how="left")
    pos_not_null = dat["price_spot"].isnull().map(lambda x: not x)
    dat["px_live"][pos_not_null] = dat["price_spot"][pos_not_null]
    dat.drop("price_spot", axis=1, inplace=True)
    return dat

@vu.tryDiagnosticDat()
def getLivePricesFuture(dat):
    sql_q = f"""SELECT conid, price AS price_fut 
        FROM live_px_future 
        WHERE timestamp >= '{TIME_LIMIT_FOR_PX}'"""
    dat_px = db.select(sql_q)
    dat = dat.merge(dat_px, on="conid", how="left")
    pos_not_null = dat["price_fut"].isnull().map(lambda x: not x)
    dat["px_live"][pos_not_null] = dat["price_fut"][pos_not_null]
    dat.drop("price_fut", axis=1, inplace=True)
    return dat

@vu.tryDiagnosticDat()
def keepOnlyInstrumentsWithAPrice(dat):
    dat.dropna(subset = ["px_live"], inplace=True)
    return dat

def roundPriceToClosestTick(dat, px_col):
    for i in range(len(dat)):
        dat[px_col][i] = vu.mround(dat[px_col].values[i], dat["tick_size"].values[i])
    return dat    

@vu.tryDiagnosticNone()
def getLivePrices(dat):
  #  runLivePriceRoutine()
    dat["px_live"] = None
    dat = getLivePricesSpot(dat)
    dat = getLivePricesFuture(dat)
    dat = keepOnlyInstrumentsWithAPrice(dat)
    dat = roundPriceToClosestTick(dat, "px_live")
    dat = roundPriceToClosestTick(dat, "px_order")
    dat = addPositions(dat)
    return dat

@vu.tryDiagnosticDat()
def removeOrdersWithoutPrice(dat):
    # Temporary for now
    dat = dat[dat["px_live"].notnull()]
    return dat

def moveInitialPriceAFewTicks(dat, px_order_col):
    dat["px_order_adj"] = dat[px_order_col]
    dat = dat.assign(
        px_order_adj = dat[px_order_col] 
        - INITIAL_ORDER_TICKS_FROM_MID * dat["buy_sell"] * dat["tick_size"]
    )
    dat[px_order_col] = dat["px_order_adj"]
    dat.drop("px_order_adj", axis=1, inplace=True)
    return dat


def setOrderPriceToLivePriceIfOrderPriceIsMissing(dat):
    pos_na = dat["px_order"].isnull()
    dat["px_order"][pos_na] = dat["px_live"][pos_na]
    return dat

def setOrderPriceToLivePriceIfTheyAreCloseEnough(dat):
    dat = dat.assign(px_diff_pct = abs(dat["px_live"] / dat["px_order"] - 1))
    pos_close_enough = dat["px_diff_pct"] <= LIMIT_EXECUTION_PRICE_FROM_INITIAL_MID_PCT
    dat["px_order"][pos_close_enough] = dat["px_live"][pos_close_enough]
    dat.drop("px_diff_pct", axis=1, inplace=True)
    return dat

@vu.tryDiagnosticNone()
def prepareInitialOrderPrice(dat):
    dat = setOrderPriceToLivePriceIfOrderPriceIsMissing(dat)
    dat = setOrderPriceToLivePriceIfTheyAreCloseEnough(dat)
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
        
    dat["px_order"][pos_beyond_limit] = dat["px_limit"][pos_beyond_limit]
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
    if "status" not in dat.columns:
        dat["status"] = "not_yet_started"

    dat = roundPriceToClosestTick(dat, "px_order")
    dat = roundPriceToClosestTick(dat, "px_limit")
    dat = dat.assign(
        remaining = dat["size"] - dat["filled"],
        px_avg = 0
        )
    
    dat = removeNegativeRemainingSizeToDo(dat)   
    #dat.drop("account_id", axis = 1, inplace=True)   
    return dat
    
####################################################################################################
### Main Script
####################################################################################################

@vu.trySimpleNone()
def prepareExecution(dat, account_id):
    global ACCOUNT_ID
    ACCOUNT_ID = account_id
    dat = getLivePrices(dat)
    dat = prepareInitialOrderPrice(dat)
    dat = prepareOrderPriceLimit(dat)
    dat = roundOrderSize(dat)
    dat = finalFormatting(dat)
    return dat