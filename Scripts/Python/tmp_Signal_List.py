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


####################################################################################################
### Script initialization
####################################################################################################
init.scriptInit(script_name, max_time_hours)

####################################################################################################
### Script variables
####################################################################################################
TIME_THRESHOLD_MINUTES_PREDICT_VS_NOW = 5
EXECUTION_TIME_ID = 2#vx.findExecutionTime()
hour_new_day = 6

cols_github_file = [
    "trade_id", "date_trade", "strategy", "ticker", "price_entry", "predict", "buy_sell",
    "target", "stop_loss", "date_exit_latest","range_dn", "range_up", 
    "tp_pct", "notional_for_1k_pnl", "timestamp_px", "timestamp_signal"
    ]

EMPTY_GITHUB_TABLE = pd.DataFrame(columns = cols_github_file)


tzhk = "Asia/Hong_Kong"

PATH_GITHUB = init.DIRECTORY_CODE + "Git/Ventura/"
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

IB_ACCOUNT_ID = 2

STRATEGY_LIST = list(range(1,15))

EXECUTION_TIMES = db.loadTableLocal("schedule_execution")
TICK_SIZE_FX = db.loadTableLocal("instrument_attribute_dbl")
ATTRIBUTES = db.loadTableLocal("instrument_attribute_type")
ETF = db.loadTableLocal("etf_proxy")
    

PROBAS = db.loadTableLocal("probability_threshold")

COLNAMES_ODA_NEW = ["trade_id", "asset_class", "trade_instrument_type", "date_trade", 
                "strategy_id", "ticker", 
                "instrument_id", "future_id", "expiry", "conid",
                "predict", "buy_sell", "price_entry", 
                "notional_for_1k_pnl", "timestamp_px", "timestamp_signal"
                ]

EMPTY_SIGNAL_NEW = pd.DataFrame({
            "trade_id":[],
            "asset_class":[],
            "trade_instrument_type":[],
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


print(init.INSTRUMENTS)
####################################################################################################
### Classes
####################################################################################################

####################################################################################################
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
        if EXECUTION_TIME_ID == execution_time_id:
            file_path = "/home/fls/Mount/GDrv/Model/Ventura/"
            file_name = "sample_github.csv"
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
        vu.printBanner("Initial data")
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
        notional_usd = self.signals.notional_for_1k_pnl * self.signals.price_entry \
            * self.signals.fx_instrument
        leverage = notional_usd / 100000
        adj_ratio = min(leverage, MAX_LEVERAGE_PER_TRADE) / leverage
        self.signals["notional_for_1k_pnl"] = self.signals.notional_for_1k_pnl * adj_ratio
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
        for col_name in ["strategy_id", "instrument_id", "future_id", "conid", "buy_sell"]:
            try:
                self.signals[col_name] = self.signals[col_name].map(int)
            except:
                pass
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
### Sub Routines
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

    if use_weights == 1:
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

@vu.tryDiagnosticNone()
def getModelPredictions(execution_time_id):
    markets_id = getMarketsToDo(execution_time_id)
    dat_no_weights = getModelPredictionsWithOrWithoutWeights(execution_time_id, 0, markets_id)
    dat_weights = getModelPredictionsWithOrWithoutWeights(execution_time_id, 1, markets_id)

    dat = dat_no_weights \
        .merge(
            dat_weights,
            on=["strategy_id", "instrument_id", "date", "timestamp", "timestamp_px"],
            how="left"
            ) \
        .merge(PROBAS, on="strategy_id", how="left") \
        .rename({"proba_threshold":"threshold"}, axis=1)

    if execution_time_id == EXECUTION_TIME_ID:
        print(dat)
    return dat

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

@vu.tryDiagnosticDat()
def retrieveConIdForFXInstruments(df):
    missing_instruments = df[df.asset_class.isin(["fx_dm","fx_em"])]
    missing_instruments = df[df.ticker != "BTCUSD"]
    missing_instruments["conid"] = np.nan
    pair_list = list(missing_instruments["ticker"])
    if len(pair_list) > 0:
        ib_app = IBContracts(pair_list)
        ib_app.connect("127.0.0.1", vx.getIBPort(IB_ACCOUNT_ID), vx.getIBClientId(script_name))
        ib_app.run()
        for i in range(len(df)):
            if df["pair_list"][i] == pair_list[i]:
                df["conid"][i] = ib_app.conid_list[i]
    else:
        df = missing_instruments
    return df[["instrument_id", "conid"]]

@vu.trySimpleNone()
def addBackInstrumentId(df):
    if "instrument_id" not in df.columns:
        if "pair" in df.columns:
            df_instrument = init.INSTRUMENTS[["pair","instrument_id"]]
            merge_on = "pair"
        elif "ticker" in df.columns:
            df_instrument = init.INSTRUMENTS[["ticker","instrument_id"]]
            merge_on = "ticker"
        df = df.merge(df_instrument, on=merge_on, how="left")
    return df

@vu.tryDiagnosticDat()
def keepOnlyBuySellOrders(df):
    return df[df.predict.isin(["up","down"])]
                      
@vu.trySimpleDat()          
def keepOnlyTradesWeCanDo(df):
    pos_keep = (init.INSTRUMENTS.use_for_trading_ib == 1)
    instruments_keep = list(init.INSTRUMENTS[pos_keep]["instrument_id"])
    return df[df.instrument_id.isin(instruments_keep)]

@vu.tryDiagnosticDat()
def calcTimeDifferencesBetweenExecutionTimesAndNow(exec_time):
    diff_vs_now = map(lambda x: (init.start_time - x).total_seconds() / 60, exec_time)
    diff_vs_now = map(
        lambda x: x if ((x >0) and (x<TIME_THRESHOLD_MINUTES_PREDICT_VS_NOW)) else None, 
        diff_vs_now)
    return list(diff_vs_now)

@vu.tryDiagnosticNone()
def instrumentListForIdentifier(df_identifier):
    vu.printBanner("UUU-1")
    print(df_identifier)
    df_identifier = df_identifier \
        .assign(instrument_id = df_identifier["identifier"]) \
        .merge(init.INSTRUMENTS[["instrument_id", "ticker", "asset_class"]]) \
        .dropna(subset = ["ticker"]) \
        .assign(future_id = np.nan) 
    vu.printBanner("UUU-2")
    print(df_identifier)
    
    df_conid = retrieveConIdForFXInstruments(df_identifier) \
        .rename({"conid":"conid_fx"}, axis=1)
    vu.printBanner("UUU-3")
    print(df_conid)
    
    df_identifier = df_identifier \
        .merge(df_conid, on="instrument_id", how="left") 
    vu.printBanner("UUU-4")
    print(df_identifier)
    
    pos_fx = df_identifier.asset_class.isin(["fx_em", "fx_dm"])
    df_identifier["conid"][pos_fx] = df_identifier["conid_fx"][pos_fx]
    vu.printBanner("UUU-5")
    print(df_identifier)
    df_identifier = df_identifier \
        [["identifier","instrument_id", "ticker", "conid", "future_id"]]
    vu.printBanner("UUU-6")
    print(df_identifier)
    return df_identifier
        
@vu.tryDiagnosticNone()
def futureListForIdentifier(df_identifier):
    return df_identifier \
        .assign(conid = df_identifier["identifier"]) \
        .merge(FUTURES_EXPIRY[["conid", "future_id"]], on="conid", how="left") \
        .dropna(subset = ["future_id"]) \
        .merge(FUTURES[["future_id", "instrument_id"]], on="future_id", how="left") \
        .merge(init.INSTRUMENTS[["instrument_id", "pair"]]) \
        [["identifier", "instrument_id", "pair", "conid", "future_id"]]

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
    ### For testing
    file_path_name = f"/home/fls/Mount/GDrv/Model/Ventura/test_order_new{account_id}.csv"
    ###
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
    print("AFZ-1")
    print(conid_list)

    account_id_contracts = 1
    res = pd.DataFrame({
            "conid":[], 
            "contract":[]
            })
    print("AFZ-2")
    print(res)
    if len(conid_list) > 0:
        print("AFZ-3")
        conid_list = list(map(int, conid_list))
        print("AFZ-4")
        print(conid_list)
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

####################################################################################################
### Script
####################################################################################################
if __name__ == "__main__":

    global fx_spot_vs_usd
    fx_spot_vs_usd = getFXvsUSD()
    
    PrepareNewOrdersList()()


    
    init.scriptFinish(script_name)