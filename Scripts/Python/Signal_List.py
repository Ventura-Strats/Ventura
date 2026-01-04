#!/usr/bin/env python3
# -*- coding: utf-8 -*-

####################################################################################################
### Prepares signals list after predictions have been saved in DB
### Then exports file to github
####################################################################################################
script_name = "Signal_List"
max_time_hours = 0.25

####################################################################################################
### Imports
####################################################################################################
import init
import utils as vu
import execution_utils as vx
#import time
import db
import numpy as np
import pandas as pd
from datetime import datetime
from datetime import timedelta
import os
from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract

####################################################################################################
### Script initialization
####################################################################################################
init.scriptInit(script_name, max_time_hours)

####################################################################################################
### Script variables
####################################################################################################
TIME_THRESHOLD_MINUTES_PREDICT_VS_NOW = 5
EXECUTION_TIME_ID = vx.findExecutionTime()
hour_new_day = 6

cols_github_file = [
    "trade_id", "date_trade", "strategy", "ticker", "price_entry", "predict", "buy_sell",
    "target", "stop_loss", "date_exit_latest","range_dn", "range_up", 
    "tp_pct", "notional_for_1k_pnl", "timestamp_px", "timestamp_signal"
    ]

EMPTY_GITHUB_TABLE = pd.DataFrame(columns = cols_github_file)

tzhk = "Asia/Hong_Kong"

PATH_GITHUB = init.DIRECTORY_DATA + "Git/Ventura/"
PATH_ORDERS = init.DIRECTORY_DATA + "Orders/"
PATH_ORDERS_NEW = PATH_ORDERS + "New/"
PATH_ORDERS_COMBINED = PATH_ORDERS + "Combined/"
PATH_ORDERS_EXIT = PATH_ORDERS + "Exit/"

ACCOUNTS = db.loadTableLocal("account")

MAX_NB_TRADES = 10
MAX_LEVERAGE_PER_TRADE = 4

FUTURES = db.loadTableLocal("future_contract")
FUTURES_EXPIRY = db.loadTableLocal("future_expiry")
FUTURES_ACTIVE = db.loadTableLocal("future_active")
sql_q = """SELECT C.conid, L.price 
        FROM (
        	SELECT conid, MAX(timestamp) AS timestamp
        	FROM live_px_future
        	GROUP BY conid
        ) C
        LEFT JOIN live_px_future L
        ON L.conid = C.conid AND L.timestamp = C.timestamp         
        WHERE L.timestamp >= '{}'""".format(
    init.start_time - timedelta(minutes = TIME_THRESHOLD_MINUTES_PREDICT_VS_NOW))
FUTURES_PRICE = db.select(sql_q)
vu.printBanner("FUTURES_PRICE:")
print(FUTURES_PRICE)

IB_ACCOUNT_ID = 2

STRATEGY_LIST = list(range(1,15))

EXECUTION_TIMES = db.loadTableLocal("schedule_execution")
TICK_SIZE_FX = db.loadTableLocal("instrument_attribute_dbl")
ATTRIBUTES = db.loadTableLocal("instrument_attribute_type")
ETF = db.loadTableLocal("ETF")



# THRESHOLD_PROBA_DIFF = 0.06 #0.05 #0.06
# THRESHOLD_PROBA_SIGNAL = 0.425# 0.4 #0.425
# THRESHOLD_PROBA_SIGNAL_PLUS_FLAT = 0.75

PROBAS = db.loadTableLocal("probability_threshold")

COLNAMES_ODA_NEW = ["trade_id", "asset_class", "date_trade", "strategy_id", "ticker", 
                "instrument_id", "future_id", "expiry", "conid",
                "predict", "buy_sell", "price_entry", 
                "notional_for_1k_pnl", "timestamp_px", "timestamp_signal"
                ]

EMPTY_SIGNAL_NEW = pd.DataFrame({
            "trade_id":[],
            "asset_class":[],
            "date_trade":[],
            "strategy_id":[],
            "ticker":[],
            "instrument_id":[],
            "future_id":[],
            "expiry":[],
            "conid":[],
            "predict":[],
            "buy_sell":[],
            "price_entry":[],
            "notional_for_1k_pnl":[],
            "timestamp_px":[],
            "timestamp_signal":[]
            })

COL_EXIT_ODA = ["trade_id", "account_id", "strategy_id", "instrument_id", "ticker",
                    "future_id", "conid", "buy_sell", "size_to_do", "px_order"]

COL_COMBINED_ODA = [
            "order_id", "ib_order_id", "account_id", "instrument_id", "ticker",
            "future_id", "conid", "contract", "tick_size",
            "buy_sell", "buy_sell_action", "size_to_do", "px_order", "px_live", "px_avg",
            "initial_position", "position", "filled", "remaining", "status", "ib_status"
            ]

####################################################################################################
### Classes
####################################################################################################
class IBContracts(EWrapper, EClient):
    # Gets the conid for a list of fx pairs
    def __init__(self, ticker_list):
        EClient.__init__(self, self)
        self.ticker_list = ticker_list
        self.nb_items = len(self.ticker_list)
        self.conid_list = [[] for i in range(self.nb_items)]
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson):
        print("Error ", reqId, " ", errorCode, " ", errorString)
        if errorCode == 502:
            print("Error - Not connected - exiting")
            self.stop()
    
    def nextValidId(self, orderId):
        self.start()
    
    def contractDetails(self, reqId, contractDetails):
        self.conid_list[reqId].append(contractDetails)
        
    def contractDetailsEnd(self, reqId):
        self.conid_list[reqId] = self.conid_list[reqId][0]
        if self.conid_list[reqId] == []:
            self.conid_list[reqId] = "Not found"
        else:        
            try:
                self.conid_list[reqId] = self.conid_list[reqId].contract.conId
            except:
                pass
        self.wrapUpIfAllComplete(reqId)
        
    def checkIfAllDone(self):
        is_completed = True
        for conid in self.conid_list:
            is_completed = is_completed and (conid != []) 
        return is_completed
    
    def wrapUpIfAllComplete(self, reqId):
        if self.checkIfAllDone():
            self.stop()
            
    def buildQuickFXContract(self, fx_pair):
        contract = Contract()
        contract.symbol = fx_pair[:3]
        contract.secType = "CASH"
        contract.exchange = "IDEALPRO"
        contract.currency = fx_pair[-3:]
        return contract
        
    def start(self):
        for i in range(self.nb_items):
            if self.ticker_list[i] is None:
                self.conid_list[i] = [None]
            else:
                contract = self.buildQuickFXContract(self.ticker_list[i])
                self.reqContractDetails(i, contract)

    def stop(self):
        self.disconnect()

####################################################################################################        
class IBAppRetrieveContracts(EWrapper, EClient):
    ### Get contract details given a list of conid
    def __init__(self, conid_list):
        EClient.__init__(self, self)
        self.conid_list = conid_list
        self.nb_items = len(self.conid_list)
        self.contract_details = [[] for i in range(self.nb_items)]
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson):
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
class PrepareSignalsList():
    # Reads model predictions for this execution time, and makes a usable list out of it
    def __init__(self):
        self.signals = None
    
    def __call__(self):
        self.prepareSignalsList()
        return self.signals

    @vu.tryDiagnosticNone()
    def compareTimesNowVsPriceAndPredict(self):
        self.signals["diff_vs_now_px"] = calcDiffBetweenNowAndDFTime(self.signals, "timestamp_px")
        self.signals["diff_vs_now_predict"] = calcDiffBetweenNowAndDFTime(self.signals, "timestamp")
        return self.signals
    
    @vu.tryDiagnosticNone()
    def keepOnlyVeryRecentPredictions(self):
        df = self.signals.copy()
        df = df[(
            (df.diff_vs_now_px > 0) 
            & (df.diff_vs_now_px < TIME_THRESHOLD_MINUTES_PREDICT_VS_NOW) 
            & (df.diff_vs_now_predict > 0) 
            & (df.diff_vs_now_predict < TIME_THRESHOLD_MINUTES_PREDICT_VS_NOW) 
            )]
        self.signals = df.copy()
        return self.signals
    
    @vu.tryDiagnosticNone()
    def workProbabiliies(self):
        df = self.signals.copy()
        for col_add in ["", "_w"]:
            df["proba_signal"+col_add] = df["proba_antisignal"+col_add] = np.nan
            pos_up = df["predict"+col_add] == "up"
            pos_down = df["predict"+col_add] == "down"
            df["proba_signal"+col_add][pos_up] = df["proba_up"+col_add][pos_up]
            df["proba_signal"+col_add][pos_down] = df["proba_down"+col_add][pos_down]
            df["proba_antisignal"+col_add][pos_up] = df["proba_down"+col_add][pos_up]
            df["proba_antisignal"+col_add][pos_down] = df["proba_up"+col_add][pos_down] 
            df["proba_diff"+col_add] = df["proba_signal"+col_add] - df["proba_antisignal"+col_add]
        df["max_proba_diff"] = df[["proba_diff", "proba_diff_w"]].max(axis=1)
       # df["max_proba_diff"][df.max_proba_diff.isnull()] = 0
        self.signals = df.copy()
        return self.signals
    
    @vu.tryDiagnosticNone()
    def keepOnlySignalsWhereBothModelsAgree(self):
        df = self.signals.copy()
        df = df[df.predict == df.predict_w]
        self.signals = df.copy()
        return self.signals
    
    @vu.tryDiagnosticNone()
    def keepOnlySignalsFlatOrGoodEnoughProba(self):
        df = self.signals.copy()
        df = df[(df.predict == "flat") | (df.max_proba_diff >= df.threshold)]
        self.signals = df.copy()
        return self.signals
            
    @vu.tryDiagnosticNone()
    def keepOnlySignalsThatFitCriteria(self):
        self.workProbabiliies()
        self.keepOnlySignalsWhereBothModelsAgree()
        self.keepOnlySignalsFlatOrGoodEnoughProba()
        return self.signals

    @vu.tryDiagnosticNone()
    def formatPredictions(self):
        self.signals["execution_time_id"] = EXECUTION_TIME_ID
        col_names = ["execution_time_id", "strategy_id", "instrument_id", "date", 
                     "timestamp", "timestamp_px", "close", "t_up", "t_dn", "predict"]
        self.signals = self.signals[col_names]
        return self.signals
    
    @vu.tryDiagnosticNone()
    def removeBondsForNowUntilFixed(self):
        ### REMOVE THIS ONCE BONDS HISTORY IS CLEAN
        self.signals = self.signals \
            .merge(
                init.INSTRUMENTS[["instrument_id", "asset_class"]],
                on="instrument_id",
                how="left"
                )
        self.signals = self.signals \
            [~self.signals.asset_class.isin(["bond"])] \
            .drop("asset_class", axis=1)
        return self.signals
    
    @vu.tryDiagnosticNone()
    def prepareSignalsList(self):
        self.signals = getModelPredictions(EXECUTION_TIME_ID)
        self.compareTimesNowVsPriceAndPredict()
        self.keepOnlyVeryRecentPredictions()
        self.keepOnlySignalsThatFitCriteria()
        self.formatPredictions()
        self.removeBondsForNowUntilFixed()
        return self.signals
            
####################################################################################################

class PrepareGithubSignals():
    # Reads model predictions, prepares a file with all the signals, and sends it to github
    def __init__(self, dat_signals, fx_spot_vs_usd):
        self.signals = self.github = dat_signals
        self.fx_spot_vs_usd = fx_spot_vs_usd
        self.file_path = self.file_name = self.file_path_export = None
        
    def __call__(self):
        self.prepareGithubTableAndSaveItAndSendItToGithub()
        
    @vu.tryDiagnosticNone()
    def prepareGitTableBasicColumns(self):
        df =  self.github \
            .merge(init.INSTRUMENTS, on=["execution_time_id", "instrument_id"], how="left") \
            .merge(init.MARKETS, on=["market", "market_id"], how="left") \
            .merge(init.REGIONS, on="region_id", how="left")
    
        df["code"] = df.pair.map(lambda x: x[:3])
        df["buy_sell"] = 0
        df["buy_sell"][df.predict == "up"] = 1
        df["buy_sell"][df.predict == "down"] = -1
        
        df = df.merge(self.fx_spot_vs_usd, on="code", how="left")
        self.github = df.copy()
        return self.github
    
    @vu.tryDiagnosticNone()
    def calcTargetsAndNotionals(self):
        df = self.github.copy()
        pos_flat = df.predict == "flat"
        df["target"] = df.close + df.buy_sell * (df.t_up - df.close)
        df["target"][pos_flat] = df.t_up[pos_flat]
        df["stop_loss"] = 2 * df.close - df.target
        df["pnl_tgt_ccy_1"] = 1000 / df.fx
        df["tp_pct"] = df.t_up / df.close - 1
        df["notional_for_1k_pnl"] = df.pnl_tgt_ccy_1 / df.tp_pct
        self.github = df.copy()
        return self.github
    
    @vu.tryDiagnosticNone()
    def addTradeId(self):
        base_trade_id = "V" \
            + init.TODAY.strftime("%Y%m%d") \
            + ("00" + str(EXECUTION_TIME_ID))[-2:]
        row_list = list(map(lambda i: ("000" + str(i+1))[-3:], range(len(self.github))))
        self.github["trade_id"] = list(map(lambda x: base_trade_id + x, row_list))
        return self.github
    
    @vu.tryDiagnosticNone()
    def addLatestExitDate(self):
        # To do properly once we have the calendars
        self.github["date_exit_latest"] = self.github.date.map(lambda d: d + timedelta(days = 7))
        return self.github
    
    @vu.tryDiagnosticNone()
    def finalGitFileFormatting(self):
        self.github = self.github \
            .rename({
                "date":"date_trade",
                "strategy_id":"strategy",
                "close":"price_entry",
                "t_up":"range_up",
                "t_dn":"range_dn",
                "timestamp":"timestamp_signal"
            }, axis=1) \
            [cols_github_file]
        return self.github
    
    @vu.tryDiagnosticNone()
    def prepareGithubTableWhenWeHaveData(self):
        self.prepareGitTableBasicColumns()
        self.calcTargetsAndNotionals()
        self.addTradeId()
        self.addLatestExitDate()
        self.finalGitFileFormatting()
    
    @vu.tryDiagnosticNone()
    def saveGithubFile(self):   
        self.file_path = prepareOrderDirectories(PATH_GITHUB + "trades_new/")
        self.file_name = "new_{}-{}.csv".format(
            init.TODAY.strftime("%Y%m%d"), 
            ("0" + str(EXECUTION_TIME_ID))[-2:]
            )
        self.file_path_export = self.file_path[self.file_path.find("trades_new"):]
        print("Saving git file:")
        print(self.github)
        self.github.to_csv(self.file_path + self.file_name, index=False)

    @vu.tryDiagnosticNone()    
    def sendDataFileToGithub(self):
        os.system(f"GitPushVentura.sh {self.file_path_export} {self.file_name}")
        vu.printBanner("Sent to Github - Done")
    
    @vu.tryDiagnosticNone()
    def prepareGithubTableAndSaveItAndSendItToGithub(self):
        if len(self.signals) != 0:    
            self.prepareGithubTableWhenWeHaveData()
        else:
            self.github = EMPTY_GITHUB_TABLE
        self.saveGithubFile()
        self.sendDataFileToGithub()

####################################################################################################
class PrepareNewOrdersList():
    # Prepares the list of new orders per strategy and saves it in a file
    def __init__(self):
        self.signals = self.nav = pd.DataFrame()
        self.n_trd_before = self.n_trd_now = self.n_trd_later = self.n_trd_total = 0
        self.oda_spot = self.oda_future = self.oda_etf_direct = self.oda_etf_index = pd.DataFrame()
        self.oda_future_tmp = self.oda_etf_direct_tmp = self.oda_etf_index_tmp = pd.DataFrame()
        
        self.colnames_oda = COLNAMES_ODA_NEW
        self.empty_signal = EMPTY_SIGNAL_NEW

    def __call__(self):
        self.prepareOrdersList()
        
    @vu.tryDiagnosticNone() 
    def readGithubSignalsFile(self, execution_time_id):
        file_path = "{}trades_new/{}/{}/".format(
            PATH_GITHUB, 
            init.TODAY.strftime("%Y-%m"), 
            init.TODAY_STR
            )
        file_name = "new_{}-{}.csv".format(
            init.TODAY.strftime("%Y%m%d"), 
            ("0" + str(execution_time_id))[-2:]
            )
        
        ### for testing
   #     if EXECUTION_TIME_ID == execution_time_id:
   #         file_path = "/home/fls/Mount/GDrv/Model/Ventura/"
   #         file_name = "sample_github.csv"
        ###
        
        vu.printBanner("Reading this file:", False)
        print(file_path + file_name)
        df = pd.read_csv(file_path + file_name)
        for col_name in ["strategy", "buy_sell"]:
            df[col_name] = df[col_name].map(int)
            
        if EXECUTION_TIME_ID == execution_time_id:
            self.signals = df
        return df
    
    @vu.tryDiagnosticNone()
    def initialFormatting(self):        
        print(self.signals)
        self.signals = addBackInstrumentId(self.signals)
        self.signals = keepOnlyBuySellOrders(self.signals)
        self.signals = keepOnlyTradesWeCanDo(self.signals)
        self.signals = self.signals \
            .merge(
                init.INSTRUMENTS[["instrument_id", "ccy", "asset_class", "trade_instrument_type"]], 
                on="instrument_id", how="left"
                ) \
            .rename({"ccy":"code", "strategy":"strategy_id"}, axis=1) \
            .merge(fx_spot_vs_usd, on="code", how="left") \
            .rename({
                "code":"ccy_instrument", 
                "fx":"fx_instrument"
                }, axis=1) \
            .assign(
                future_id = np.nan, 
                expiry = np.nan
                )
        return self.signals
    
    @vu.tryDiagnosticNone()  
    def getLatestClosingNAV(self):
        sql_q = """SELECT N.account_id, N.nav_usd AS nav
            FROM (
            	SELECT account_id, MAX(timestamp) as timestamp
            	FROM book_nav
                WHERE HOUR(timestamp) <= 6
            	GROUP BY account_id
            ) D 
            LEFT JOIN book_nav N 
                ON N.account_id = D.account_id 
                AND N.timestamp = D.timestamp"""
        self.nav = db.select(sql_q)
        return self.nav
     
    @vu.tryDiagnosticNone()
    def groupInstrumentOrdersInCaseSeveralStrategiesGiveASignalOnIt(self):
        df = self.orders.copy()
        dat_px = df[["instrument_id", "price_entry"]].drop_duplicates()
        df["notional_for_1k_pnl"] = df.notional_for_1k_pnl * df.buy_sell
        df = df.groupby("instrument_id", as_index=False).notional_for_1k_pnl.sum()
        df = df[df.notional_for_1k_pnl != 0]
        df = df.merge(dat_px, on="instrument_id", how="left")
        df["buy_sell"] = df.notional_for_1k_pnl.map(vu.sign)
        self.orders = df[["instrument_id", "buy_sell", "price_entry", "notional_for_1k_pnl"]]
        return self.orders
    
    @vu.tryDiagnosticNone()
    def calcNbTradesAlreadyDoneToday(self):
        i = 1
        while i < EXECUTION_TIME_ID:
            vu.printBanner(f"Reading past trades for execution_time_id {i}:", False)
            try:
                dat_i = self.readGithubSignalsFile(i)
                dat_i = addBackInstrumentId(dat_i)
                dat_i = keepOnlyTradesWeCanDo(dat_i)
                vu.printBanner(f"List of trades for past execution_time_id {i}:", False)    
                print(dat_i)
                if dat_i is not None:
                    self.n_trd_before = self.n_trd_before + len(dat_i)
            except:
                pass
            i += 1
        self.n_trd_total = self.n_trd_total + self.n_trd_before
        return self.n_trd_total
    
    @vu.tryDiagnosticNone()      
    def calcNbTradesThisRun(self):
        if self.signals is not None:
            self.n_trd_now = len(self.signals)
        self.n_trd_total = self.n_trd_total + self.n_trd_now
        return self.n_trd_now
    
    @vu.tryDiagnosticNone()      
    def estimateNbTradesLater(self):
        max_execution_time_id = EXECUTION_TIMES["execution_time_id"].max()
        i = EXECUTION_TIME_ID  + 1
        while i <= max_execution_time_id:
            vu.printBanner(f"Seeking future trades for execution_time_id {i}:", False)
            dat_i = getModelPredictions(i)
            dat_i = keepOnlyBuySellOrders(dat_i)
            dat_i = keepOnlyTradesWeCanDo(dat_i)
            vu.printBanner(f"List of future trades for execution_time_id {i}:", False)
            print(dat_i)
            if dat_i is not None:
                self.n_trd_later = self.n_trd_later + len(dat_i)
            i += 1
        self.n_trd_total = self.n_trd_total + self.n_trd_later
        return self.n_trd_later
    
    @vu.tryDiagnosticNone()
    def expectedTotalNbTradesToday(self):
        self.calcNbTradesAlreadyDoneToday()
        self.calcNbTradesThisRun()
        self.estimateNbTradesLater()
        return self.n_trd_total
    
    @vu.tryDiagnosticNone()
    def scaleDownNotionalsIfTooManyTrades(self):
        self.expectedTotalNbTradesToday()
        adj_ratio = MAX_NB_TRADES / max(MAX_NB_TRADES, self.n_trd_total)
        self.signals["notional_for_1k_pnl"] = self.signals.notional_for_1k_pnl * adj_ratio
        return self.signals
    
    @vu.tryDiagnosticNone()
    def capNotionalVsNAV(self):
        print(1)
        print(len(self.signals))
        if len(self.signals) > 0:
            print(2)
            notional_usd = self.signals.notional_for_1k_pnl * self.signals.price_entry \
                * self.signals.fx_instrument
            print(3)
            print(notional_usd)
            leverage = notional_usd / 100000
            print(4)
            print(leverage)
            adj_ratio = min(leverage, MAX_LEVERAGE_PER_TRADE) / leverage
            print(5)
            print(adj_ratio)
            self.signals["notional_for_1k_pnl"] = self.signals.notional_for_1k_pnl * adj_ratio
            print(6)
            print(self.signals)
        return self.signals
    
    @vu.tryDiagnosticNone()
    def instrumentsListForThisTradeType(self, trade_type):
        return self.signals[self.signals.trade_instrument_type == trade_type]
    
    @vu.tryDiagnosticNone()
    def prepareSpotOrDirectETF(self, order_type):
        df = self.instrumentsListForThisTradeType(order_type) \
            .merge(
                init.INSTRUMENTS[["instrument_id", "conid_spot"]], 
                on="instrument_id", how="left"
                ) \
            .rename({"conid_spot":"conid"}, axis=1) \
            [self.colnames_oda]
        return df
    
    @vu.tryDiagnosticNone()
    def prepareSpotTrades(self):
        self.oda_spot = self.empty_signal
        self.oda_spot = self.prepareSpotOrDirectETF("Spot")
        return self.oda_spot
    
    def prepareFutureData(self):
        df_fut = self.oda_future_tmp[["instrument_id"]] \
            .drop_duplicates() \
            .merge(FUTURES[FUTURES.tradable == 1], on="instrument_id", how="left") \
            .dropna(subset=["future_id"]) \
            .merge(FUTURES_ACTIVE, on="future_id", how="left") \
            .merge(FUTURES_EXPIRY, on=["future_id","conid"], how="left") \
            .merge(FUTURES_PRICE, on="conid", how="left") \
            .rename({"notional":"notional_future"}, axis=1) \
            [["instrument_id", "future_id", "ib_symbol", "expiry", "conid", "inverse_quote",
              "price", "notional_future", "ccy_id"]]
        return df_fut
    
    @vu.tryDiagnosticNone()
    def regroupFuturesContractInformation(self):
        df_fut = self.prepareFutureData()
        self.oda_future_tmp = self.oda_future_tmp \
            .merge(df_fut, on="instrument_id", how="left") \
            .merge(init.CURRENCIES[["ccy_id", "ccy"]], on="ccy_id", how="left") \
            .drop("ccy_id", axis=1) \
            .rename({"ccy":"code"}, axis=1) \
            .merge(fx_spot_vs_usd, on="code", how="left") \
            .rename({"code":"ccy_future", "fx":"fx_future"}, axis=1)
        return self.oda_future_tmp
    
    @vu.tryDiagnosticNone()
    def invertFutureDirectionForInverseQuotes(self):
        pos_inverted = self.oda_future_tmp.inverse_quote == 1
        self.oda_future_tmp["buy_sell"][pos_inverted] = \
            -self.oda_future_tmp["buy_sell"][pos_inverted]
        self.oda_future_tmp["notional_for_1k_pnl"][pos_inverted] = 1 \
            / self.oda_future_tmp["notional_for_1k_pnl"][pos_inverted]
        self.oda_future_tmp["predict"][self.oda_future_tmp.buy_sell == 1] = "up"
        self.oda_future_tmp["predict"][self.oda_future_tmp.buy_sell == -1] = "down"
        return self.oda_future_tmp
    
    @vu.tryDiagnosticNone()
    def recomputeNotionalForFutures(self):
        self.oda_future_tmp["notional_for_1k_pnl"] = self.oda_future_tmp.notional_for_1k_pnl \
            / self.oda_future_tmp.notional_future \
            * self.oda_future_tmp.fx_instrument / self.oda_future_tmp.fx_future
        return self.oda_future_tmp
    
    @vu.tryDiagnosticNone()
    def finalFormattingFutureOrders(self):
        self.oda_future_tmp["price_entry"] = self.oda_future_tmp.price
        self.oda_future_tmp = self.oda_future_tmp[self.colnames_oda]
        return self.oda_future_tmp
    
    @vu.tryDiagnosticNone()
    def prepareFutureTrades(self):
        self.oda_future = self.empty_signal
        self.oda_future_tmp = self.instrumentsListForThisTradeType("Future") \
            .drop(["future_id", "expiry"], axis=1)
        self.regroupFuturesContractInformation()
        self.invertFutureDirectionForInverseQuotes()
        self.recomputeNotionalForFutures()
        self.finalFormattingFutureOrders()
        
        self.oda_future = self.oda_future_tmp.copy()
        return self.oda_future
        
    @vu.tryDiagnosticNone()
    def prepareETFTradesDirect(self):
        self.oda_etf_direct = self.empty_signal
        self.oda_etf_direct = self.prepareSpotOrDirectETF("Stock")
        return self.oda_etf_direct
    
    @vu.tryDiagnosticNone()
    def prepareETFTradesForIndex(self):
        self.oda_etf_index = self.empty_signal
        self.oda_etf_index = self.instrumentsListForThisTradeType("ETF") \
            .merge(ETF[["instrument_id", "conid"]], on="instrument_id", how="left") \
            [self.colnames_oda]
        return self.oda_etf_index

    @vu.tryDiagnosticNone()
    def mergeAllOrderTypes(self):
        self.signals = self.empty_signal
        self.signals = self.oda_spot
        self.signals = self.signals.append(self.oda_future)
        self.signals = self.signals.append(self.oda_etf_direct)
        self.signals = self.signals.append(self.oda_etf_index)
        self.signals = self.signals.sort_values(by=["trade_id"], axis=0)
        return self.signals
    
    @vu.tryDiagnosticNone()
    def splitOrdersByTradeType(self):
        self.prepareSpotTrades()
        self.prepareFutureTrades()
        self.prepareETFTradesDirect()
        self.prepareETFTradesForIndex()
        self.mergeAllOrderTypes()
        return self.signals
    
    @vu.tryDiagnosticNone()
    def keepOnlyTradesWithAPrice(self):
        self.signals = self.signals.dropna(subset=["price_entry"])
        return self.signals
    
    @vu.tryDiagnosticNone()
    def finalOrderFormatting(self):
        col_int = ["strategy_id", "instrument_id", "future_id", "conid", "buy_sell"]
        self.signals = vu.forceColumnType(self.signals, col_int, int)
        return self.signals
    
    @vu.tryDiagnosticNone()
    def calcSizeByScalingNotionals(self, account_id):
        nav = self.nav[self.nav["account_id"] == account_id]["nav"].values[0]
        df = self.signals.copy()
        df["size_to_do"] = self.signals.notional_for_1k_pnl * nav / 100000
        return df

    @vu.tryDiagnosticNone()
    def prepareOrderListForOneAccount(self, account_id):
        vu.printBanner(f"Doing account_id: {account_id}", False)
        df = self.calcSizeByScalingNotionals(account_id)
        df = formatOrderListBeforeSaving(df, account_id)
        df = roundNotionals(df)
        df.dropna(subset=["conid", "size_to_do", "px_order"], inplace=True)
        saveOrderListFileByAccount(account_id, df, "new")        
        return df
    
    @vu.tryDiagnosticNone()
    def prepareOrderListsByAccount(self):
        vu.printBanner("Now ready to export new orders")
        print(self.signals)
        for account_id in list(ACCOUNTS["account_id"]):
            vu.printBanner(f"Doing account {account_id}")
            orders_i = self.prepareOrderListForOneAccount(account_id)
            vu.printBanner(f"Doing account {account_id} - done, result:", False)
            print(orders_i)
            
    @vu.tryDiagnosticNone() 
    def prepareOrdersList(self):
        self.readGithubSignalsFile(EXECUTION_TIME_ID)
        self.initialFormatting()
        self.scaleDownNotionalsIfTooManyTrades()
        self.capNotionalVsNAV()
        self.getLatestClosingNAV()
        self.splitOrdersByTradeType()
        self.keepOnlyTradesWithAPrice()
        self.finalOrderFormatting()
        self.prepareOrderListsByAccount()

####################################################################################################
        
class PrepareExitOrdersList():
    # Prepares the list of new orders per strategy and saves it in a file
    def __init__(self, account_id):
        self.account_id = account_id
        self.empty_signal = pd.DataFrame({
            "trade_id":[],
            "account_id":[],
            "strategy_id":[],
            "instrument_id":[],
            "ticker":[],
            "future_id":[],
            "conid":[],
            "buy_sell":[],
            "size_to_do":[],
            "price_order":[]
            })        
        self.orders_exit = self.empty_signal
    
    def __call__(self):
        self.prepareOrdersExit()

    @vu.tryDiagnosticNone()
    def loadExitTradesForTodayFromDB(self):
        #date_entry: TO DO PROPERLY WITH CALENDARS
        date_entry = (init.TODAY - timedelta(days = 7)).strftime("%Y-%m-%d") 
        sql_q = f"""SELECT T.trade_id, L.account_id, T.strategy_id,
            IFNULL(I.instrument_id, C.instrument_id) AS instrument_id,
            IFNULL(I.ticker, I2.ticker) AS ticker, 
            X.conid, X.future_id, T.date_entry, 
            -AVG(L.buy_sell) AS buy_sell,
            SUM(L.size) as size_to_do,
            IFNULL(AVG(F.price), AVG(P.close)) AS px_order
            FROM book_trade T
            LEFT JOIN book_trade_map M ON M.trade_id = T.trade_id
            LEFT JOIN book_trade_leg L ON L.leg_id = M.leg_id
            LEFT JOIN live_px P ON P.instrument_id = L.identifier
            LEFT JOIN static_instrument I ON I.instrument_id = L.identifier
            LEFT JOIN static_future_expiry X ON X.conid = L.identifier
            LEFT JOIN static_future_contract C ON C.future_id = X.future_id
            LEFT JOIN static_instrument I2 ON I2.instrument_id = C.instrument_id
            LEFT JOIN (
            	SELECT A1.conid, A1.price
            		FROM live_px_future A1
            		RIGHT JOIN (
            		SELECT conid, MAX(timestamp) AS timestamp
            		FROM live_px_future
            		GROUP BY conid
            		) A2 ON A1.conid = A2.conid AND A1.timestamp = A2.timestamp
            ) F ON F.conid = L.identifier
            WHERE T.strategy_id != 0
            AND T.trade_outcome_id = 0
            AND T.date_exit IS NULL
            AND T.date_entry = '{date_entry}'
            AND M.trade_category_id = 1
            AND L.account_id = {self.account_id}
            GROUP BY T.trade_id, T.strategy_id, L.identifier"""
        self.orders_exit = db.select(sql_q)
        return self.orders_exit

    @vu.tryDiagnosticNone()
    def keepOnlyTradesForThisExecutionTime(self):
        pos_keep = init.INSTRUMENTS["execution_time_id"] == EXECUTION_TIME_ID
        instruments_keep = list(init.INSTRUMENTS["instrument_id"][pos_keep])
        
        pos_keep = self.orders_exit["instrument_id"].isin(instruments_keep)
        self.orders_exit = self.orders_exit[pos_keep]
        
        return self.orders_exit
    
    @vu.tryDiagnosticNone()
    def retrieveConIdForFXAndSpotInstruments(self):
        df = self.orders_exit.merge(
                init.INSTRUMENTS[["instrument_id", "asset_class", "conid_spot"]], 
                on="instrument_id", how="left"
                )
        pos_conid = df.conid_spot.isnull()
        df["conid"][pos_conid] = df.conid_spot[pos_conid]
        df.drop("conid_spot", axis=1, inplace=True)
        self.orders_exit = df
        return self.orders_exit


    @vu.tryDiagnosticNone()
    def forceColumnTypes(self):
        col_int = ["trade_id", "account_id", "strategy_id", "instrument_id", 
                         "future_id", "conid", "buy_sell"]
        col_flt = ["size_to_do", "px_order"]
        self.orders_exit = vu.forceColumnType(self.orders_exit, col_int, int)
        self.orders_exit = vu.forceColumnType(self.orders_exit, col_flt, float)
        return self.orders_exit

    @vu.tryDiagnosticNone()
    def formatExitOrders(self):
        self.orders_exit = self.orders_exit[COL_EXIT_ODA]
        self.forceColumnTypes()
        return self.orders_exit
    
    @vu.tryDiagnosticNone()
    def saveExitOrders(self):
        vu.printBanner("Going to save this:", False)
        print(self.orders_exit)
        saveOrderListFileByAccount(self.account_id, self.orders_exit, "Exit")
    
    @vu.tryDiagnosticNone()
    def prepareOrdersExit(self):
        # output format:
        # trade_id, account_id, strategy_id, instrument_id, ticker, future_id, 
        #   conid, buy_sell, size_to_do, px_order
        self.loadExitTradesForTodayFromDB()
        self.keepOnlyTradesForThisExecutionTime()
        self.retrieveConIdForFXAndSpotInstruments()
        self.formatExitOrders()
        self.saveExitOrders()
        return self.orders_exit

####################################################################################################
class PrepareCombinedOrdersList():
    ### Prepare initial order list by gather new and exit trades
    ### We take data that looks like this:
    ### - New: From file export from previous class:
    ###     account_id, strategy_id, instrument_id, ticker, future_id, 
    ###     conid, buy_sell, size_to_do, px_order
    ### - Exit: From file export from previous class:
    ###     trade_id, account_id, strategy_id, instrument_id, ticker, future_id, 
    ###     conid, buy_sell, size_to_do, px_order
    ### We output a combined table that looks like this:
    ### account_id, ticker, asset_class, 
    ### buy_sell, size_to_do, px_order, buy_sell_action, 
    ### future_id, instrument_id, tick_size, conid, contract, 
    ### initial_position, position, filled, remaining, px_live, px_avg, 
    ### order_id, ib_order_id, status, ib_status

    def __init__(self, account_id):
        self.account_id = account_id
        self.empty_signal = pd.DataFrame({
            "account_id":[],
            "strategy_id":[],
            "instrument_id":[],
            "ticker":[],
            "future_id":[],
            "conid":[],
            "buy_sell":[],
            "size_to_do":[],
            "price_order":[]
            }) 
        self.orders_new = self.orders_exit = self.empty_signal
        
        self.orders = pd.DataFrame({
            "account_id":[],
            "strategy_id":[],
            "instrument_id":[],
            "ticker":[],
            "future_id":[],
            "conid":[],
            "buy_sell":[],
            "size_to_do":[],
            "price_order":[],
            "asset_class":[]
            }) 
        
        self.int_col = ["account_id", "strategy_id", "instrument_id", "future_id", 
                        "conid", "buy_sell"]
        self.float_col = ["size_to_do", "px_order"]
        
        self.tick_size = pd.DataFrame({"conid":[], "tick_size":[]})
        self.tick_attrid = ATTRIBUTES["attribute_id"][ATTRIBUTES.attribute == "tick_size"].values[0]
    
    def __call__(self):
        self.prepareDetailedOrderList()
        
    @vu.tryDiagnosticNone() 
    def formatNumericalColumnTypes(self, df):
        df = vu.forceColumnType(df, self.int_col, int)
        df = vu.forceColumnType(df, self.float_col, float)
        return df
    
    @vu.tryDiagnosticNone()
    def readOrdersFromFile(self, new_or_exit):
        df = readOrderListFileByAccount(self.account_id, new_or_exit)
        df = self.formatNumericalColumnTypes(df)
        return df
    
    @vu.tryDiagnosticNone()
    def prepareOrdersNew(self):
        df = self.readOrdersFromFile("New")
        if len(df) > 0:
            self.orders_new = df
        return self.orders_new
    
    @vu.tryDiagnosticNone()
    def prepareOrdersExit(self):
        df = self.readOrdersFromFile("Exit")
        if len(df) > 0:
            df.drop("trade_id", axis=1, inplace=True)
            self.orders_exit = df
        return self.orders_exit
        
    @vu.tryDiagnosticNone()
    def combineNewAndExitOrders(self):  
        df = self.orders_new.append(self.orders_exit)
        df = self.formatNumericalColumnTypes(df)
        self.orders = df.copy()
        return self.orders

    @vu.tryDiagnosticNone()
    def summarizePositions(self):
        df_pos = self.orders.copy()
        df_pos = df_pos \
            .assign(size_to_do = df_pos.size_to_do * df_pos.buy_sell) \
            .groupby(["conid"], as_index=False) \
            .agg({"size_to_do":"sum"})
    
        df_pos = df_pos \
            .assign(
                buy_sell = df_pos.size_to_do.map(vu.sign),
                size_to_do = df_pos.size_to_do.map(abs)
                )
        df_pos = vu.forceColumnType(df_pos, ["conid"], int)
            
        return df_pos
    
    @vu.tryDiagnosticNone()
    def summarizePrices(self):
        df = pd.DataFrame({"conid":[], "px_order":[]})
        df = vu.forceColumnType(df, ["conid"], int)
        if len(self.orders) > 0:
            df = self.orders \
                [["conid", "px_order"]] \
                .dropna() \
                .groupby("conid", as_index=False) \
                .agg({"px_order":"mean"})
        return df
        
    @vu.tryDiagnosticNone()
    def groupAndNetOrdersByInstrument(self):
        df_pos = self.summarizePositions()
        df_px = self.summarizePrices() 
        df_instruments = self.orders \
            [["account_id", "instrument_id","future_id", "ticker","conid"]] \
            .drop_duplicates()
        df_instruments = vu.forceColumnType(df_instruments, ["conid"], int)
        
        self.orders = pd.DataFrame({
            "account_id":[], 
            "instrument_id":[], 
            "future_id":[], 
            "ticker":[],
            "asset_class":[], 
            "conid":[], 
            "buy_sell":[], 
            "size_to_do":[], 
            "px_order":[]
            })
        self.orders = vu.forceColumnType(self.orders, ["conid"], int)
        
        self.orders = df_pos \
            [df_pos.size_to_do > 0] \
            .merge(df_px, on="conid", how="left") \
            .merge(df_instruments, on="conid", how="left") \
            [["account_id", "instrument_id", "future_id", "ticker",
              "asset_class", "conid", "buy_sell", 
              "size_to_do", "px_order"]]
        return self.orders
            
    @vu.tryDiagnosticNone()
    def addBuySellAction(self):
        df = self.orders.copy()
        df["buy_sell_action"] = "NOTHING"
        df["buy_sell_action"][df.buy_sell == 1] = "BUY"
        df["buy_sell_action"][df.buy_sell == -1] = "SELL"
        self.orders = df[df.buy_sell_action.isin({"BUY", "SELL"})]
        return self.orders
        
    @vu.tryDiagnosticNone() 
    def getTickSizeFuture(self):
        df = FUTURES_EXPIRY \
            .merge(FUTURES, on="future_id", how="left") \
            [["conid", "tick_size"]]
        self.tick_size = df.copy()
        return df
    
    @vu.tryDiagnosticNone() 
    def getTickSizeFXAndETFDirect(self):
        df = TICK_SIZE_FX \
            [TICK_SIZE_FX.attribute_id == self.tick_attrid] \
            .merge(init.INSTRUMENTS, on="instrument_id", how="left") \
            .rename({"conid_spot":"conid", "value":"tick_size"}, axis=1) \
            [["conid", "tick_size"]] \
            .dropna()
        self.tick_size = self.tick_size.append(df)
        return df
    
    @vu.tryDiagnosticNone()
    def getTickSizeETFProxy(self):
        df = ETF[["conid", "tick_size"]].dropna()
        self.tick_size = self.tick_size.append(df)
        return df
    
    @vu.tryDiagnosticNone()
    def buildTickSizeMap(self):
        self.getTickSizeFuture()
        self.getTickSizeFXAndETFDirect()
        self.getTickSizeETFProxy()
        return self.tick_size
    
    @vu.tryDiagnosticNone() 
    def wrapUpTickSize(self):
        self.orders = self.orders.merge(self.tick_size, on="conid", how="left")
        return self.orders
    
    @vu.tryDiagnosticNone()    
    def removeOrdersWithNoTickSize(self):
        self.orders.dropna(subset=["tick_size"], inplace=True)
        return self.orders
        
    @vu.tryDiagnosticNone() 
    def addTickSize(self):
        self.buildTickSizeMap()
        self.wrapUpTickSize()
        self.removeOrdersWithNoTickSize()
        return self.orders
        
    @vu.tryDiagnosticNone()
    def getContractDetails(self):
        contract_details = fetchIBContractDetails(list(self.orders.conid))
        self.orders = self.orders.merge(contract_details, on="conid", how="left")
        return self.orders
    
    @vu.tryDiagnosticNone()
    def removeOrdersWithNoContractDetails(self):
        self.orders.dropna(subset=["contract"], inplace=True)
        return self.orders
    
    @vu.tryDiagnosticNone()
    def prepareExecutionColumns(self):
        self.orders["initial_position"] = self.orders["position"] = self.orders["filled"] = 0
        self.orders["remaining"] = self.orders.size_to_do
        self.orders["px_live"] = self.orders["px_avg"] = self.orders["ib_order_id"] = np.nan
        self.orders["order_id"] = list(range(1, 1+len(self.orders)))
        self.orders["status"] = "not yet started"
        self.orders["ib_status"] = ""

        self.orders = self.formatNumericalColumnTypes(self.orders) \
            .rename({"price_order":"px_order"}, axis=1) \
            [COL_COMBINED_ODA]
        
        return self.orders
           
    @vu.tryDiagnosticNone()
    def prepareDetailedOrderList(self):
        self.prepareOrdersNew()
        self.prepareOrdersExit()
        self.combineNewAndExitOrders()
        self.groupAndNetOrdersByInstrument()
        self.addBuySellAction()
        self.addTickSize()
        self.getContractDetails()
        self.removeOrdersWithNoContractDetails()
        self.prepareExecutionColumns()
        saveOrderListFileByAccount(self.account_id, self.orders, "Combined")
        return self.orders
    
####################################################################################################
### Sub routines
####################################################################################################

def getMarketsToDo(execution_time_id):
    markets_id = list(init.INSTRUMENTS["market_id"][
        init.INSTRUMENTS["execution_time_id"] == execution_time_id])
    markets_id = list(set(markets_id))
    markets_id = map(str, markets_id)
    markets_id = ','.join(markets_id)
    return markets_id

@vu.tryDiagnosticNone()
def getModelPredictionsWithOrWithoutWeights(execution_time_id, use_weights, markets_id):
    sql_q = f"""SELECT P.instrument_id, P.strategy_id, P.date, P.timestamp, P.timestamp_px, 
        P.close, P.t_up, P.t_dn, T.outcome AS predict, P.proba_up, P.proba_flat, P.proba_down
        FROM live_predict P
        LEFT JOIN static_instrument I ON I.instrument_id = P.instrument_id
        LEFT JOIN static_market M ON M.market_id = I.market_id
        LEFT JOIN static_trade_outcome T ON T.outcome_id = P.outcome_id
        WHERE P.score = 10
        AND P.use_weights = {use_weights}
        AND M.market_id IN ({markets_id})"""
    dat = db.select(sql_q)

    if (use_weights == 1) and (dat is not None):
        dat = dat \
            [["instrument_id","strategy_id","date","timestamp","timestamp_px",
           "predict","proba_up","proba_flat","proba_down"]] \
           .rename({
                "predict": "predict_w",
                "proba_up": "proba_up_w",
                "proba_flat": "proba_flat_w",
                "proba_down": "proba_down_w"
            }, axis=1)

    return dat

@vu.trySimpleNone()
def getModelPredictions(execution_time_id):
    markets_id = getMarketsToDo(execution_time_id)
    dat_no_weights = getModelPredictionsWithOrWithoutWeights(execution_time_id, 0, markets_id)
    dat_weights = getModelPredictionsWithOrWithoutWeights(execution_time_id, 1, markets_id)
    
    merge_columns = ["strategy_id", "instrument_id", "date", "timestamp", "timestamp_px"]

    dat = dat_no_weights \
        .merge(dat_weights, on=merge_columns, how="left") \
        .merge(PROBAS, on="strategy_id", how="left") \
        .rename({"proba_threshold":"threshold"}, axis=1)

    if execution_time_id == EXECUTION_TIME_ID:
        vu.printBanner("getModelPredictions result:", False)
        print(dat)
    return dat

@vu.tryDiagnosticNone()
def calcDiffBetweenNowAndDFTime(dat, col_name):
    date_time = convertDBTimesIntoDateTime(dat, col_name)
    diff_vs_now = list(map(lambda x: (init.start_time - x).total_seconds() / 60, date_time))
    return diff_vs_now

def convertDBTimesIntoDateTime(dat, col_name):
    date_time = list(dat[col_name])
    date_time = map(lambda x: init.TODAY_STR + " " + str(x)[-8:], date_time)
    date_time = map(lambda x: datetime.strptime(x, "%Y-%m-%d %H:%M:%S"), date_time)
    date_time = map(lambda x: x if x.hour >= hour_new_day else x + timedelta(days = 1), date_time)
    return date_time

@vu.trySimpleNone()
def addBackInstrumentId(df):
    col_names = df.columns
    if "instrument_id" not in col_names:
        if "pair" in col_names:
            merge_on = "pair"
        elif "ticker" in col_names:
            merge_on = "ticker"
        df = df.merge(init.INSTRUMENTS[[merge_on, "instrument_id"]], on=merge_on, how="left")
    return df

@vu.tryDiagnosticDat()
def keepOnlyBuySellOrders(df):
    if df is not None:
        df = df[df.predict.isin(["up","down"])]
    return df
                      
@vu.trySimpleDat()          
def keepOnlyTradesWeCanDo(df):
    pos_keep = init.INSTRUMENTS.use_for_trading_ib == 1
    instruments_keep = list(init.INSTRUMENTS.instrument_id[pos_keep])
    return df[df.instrument_id.isin(instruments_keep)]

@vu.tryDiagnosticDat()
def calcTimeDifferencesBetweenExecutionTimesAndNow(exec_time):
    diff_vs_now = map(lambda x: (init.start_time - x).total_seconds() / 60, exec_time)
    diff_vs_now = map(
        lambda x: x if ((x >0) and (x<TIME_THRESHOLD_MINUTES_PREDICT_VS_NOW)) else None, 
        diff_vs_now)
    return list(diff_vs_now)

def prepareOrderDirectories(base_path):
    file_path = base_path + init.TODAY.strftime("%Y-%m")
    vu.checkIfDirectoryExistsAndCreateIfNot(file_path)
    file_path = file_path + "/" + init.TODAY_STR + "/"
    vu.checkIfDirectoryExistsAndCreateIfNot(file_path)
    return file_path

def getOrderListFilePath(account_id, file_type):
    path_used = PATH_ORDERS_NEW
    file_prefix = "new"
    if file_type == "Combined":
        path_used = PATH_ORDERS_COMBINED
        file_prefix = "combined"
    elif file_type == "Exit":
        path_used = PATH_ORDERS_EXIT
        file_prefix = "exit"
        
    file_path = prepareOrderDirectories(path_used)
    file_name = "{}_orders_{}_{}-{}.csv".format(
        file_prefix,
        str(account_id),
        init.TODAY.strftime("%Y%m%d"), 
        ("0" + str(EXECUTION_TIME_ID))[-2:]
        )
    return file_path + file_name

@vu.tryDiagnosticNone()
def saveOrderListFileByAccount(account_id, df, file_type):
    file_path_name = getOrderListFilePath(account_id, file_type)
    df.to_csv(file_path_name, index=False)
    return df

@vu.tryDiagnosticNone()
def readOrderListFileByAccount(account_id, file_type):
    file_path_name = getOrderListFilePath(account_id, file_type)
    return pd.read_csv(file_path_name)
    

####################################################################################################
### Sub Routines for prepareOrderList
####################################################################################################    
@vu.tryDiagnosticDat()
def fetchIBContractDetails(conid_list):
    account_id_contracts = 2
    res = pd.DataFrame({"conid":[], "contract":[]})
    if len(conid_list) > 0:
        conid_list = list(map(int, conid_list))
        ib_app = IBAppRetrieveContracts(conid_list)
        ib_app.connect(
            "127.0.0.1", 
            vx.getIBPort(account_id_contracts), 
            vx.getIBClientId(script_name)
            )
        ib_app.run()    
        res = pd.DataFrame({
            "conid":conid_list, 
            "contract":ib_app.contract_details
            })
    return res

@vu.tryDiagnosticDat()
def roundNotionals(df):
    df["size_to_do"] = vu.mround(df["size_to_do"] - 0.25, 1)
    return df[df.size_to_do > 0]

@vu.tryDiagnosticNone()   
def formatOrderListBeforeSaving(df, account_id):
    df.rename({"price_entry":"px_order", "strategy":"strategy_id"}, axis=1, inplace=True)
    df["account_id"] = account_id
    return df[["account_id", "strategy_id", "instrument_id", 
            "ticker", "future_id", "conid", 
            "buy_sell", "size_to_do", "px_order"]]

def getFXvsUSD():
    sql_q = """SELECT A.code, H.fx
            FROM (
                SELECT asset_id, MAX(date) as date
                FROM histo_fx_close_vs_usd
                GROUP BY asset_id
            ) D 
            LEFT JOIN histo_fx_close_vs_usd H 
                ON D.asset_id = H.asset_id 
                AND D.date = H.date
            LEFT JOIN static_asset A ON A.asset_id = H.asset_id"""
    return db.select(sql_q)
    
@vu.trySimpleNone()
def sendTelegramSignals():
    os.system("RScriptVentura.sh Communications/Send_Predict.R")
    os.system("RScriptVentura.sh Communications/Send_Signal.R")

####################################################################################################
### Script
####################################################################################################
if __name__ == "__main__":
    vx.waitTillPreviousJobHasFinished(
        previous_script_name = "Predict", 
        last_completed_stage = 1, 
        nb_jobs_total = 8, 
        max_lag_prices_minutes = 7,
        max_wait_time_before_continuing_minutes = 4
        )

    global fx_spot_vs_usd
    fx_spot_vs_usd = getFXvsUSD()
    dat_signals = PrepareSignalsList()()
    vu.printBanner("DAT_SIGNALS")
    print(dat_signals)
    
    dat_github = PrepareGithubSignals(dat_signals, fx_spot_vs_usd)()
    vu.printBanner("DAT_GITHUB")
    
    sendTelegramSignals()
    
    PrepareNewOrdersList()()
    vu.printBanner("DAT_NEW_DONE")
    
    for account_id in [1,2]:
        PrepareExitOrdersList(account_id)()
        vu.printBanner(f"DAT_EXIT_DONE_{account_id}")    
        PrepareCombinedOrdersList(account_id)()
        vu.printBanner(f"DAT_ORDERS_COMBINED_{account_id}")
    
    init.scriptFinish(script_name)