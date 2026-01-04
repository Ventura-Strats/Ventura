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
import time
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
cols_github_file = [
    "trade_id", "date_trade", "strategy", "ticker", "price_entry", "predict", "buy_sell",
    "target", "stop_loss", "date_exit_latest","range_dn", "range_up", 
    "tp_pct", "notional_for_1k_pnl", "timestamp_px", "timestamp_signal"
    ]

empty_github_table = pd.DataFrame(columns = cols_github_file)

tzhk = "Asia/Hong_Kong"

hour_new_day = 6

PATH_GITHUB = init.DIRECTORY_CODE + "Git/Ventura/"
PATH_ORDERS = init.DIRECTORY_DATA + "Orders/"

time_threshold_minutes_predict_vs_now = 5

ACCOUNTS = db.loadTableLocal("account")

MAX_NB_TRADES = 10

execution_time_id = vx.findExecutionTime()

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
    init.start_time - timedelta(minutes = time_threshold_minutes_predict_vs_now))
FUTURES_PRICE = db.select(sql_q)

IB_ACCOUNT_ID = 2

STRATEGY_LIST = list(range(1,15))
####################################################################################################
### Classes
####################################################################################################
class IBContracts(EWrapper, EClient):
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
        
    def start(self):
        for i in range(self.nb_items):
            if self.ticker_list[i] is None:
                self.conid_list[i] = [None]
            else:
                contract = buildQuickFXContract(self.ticker_list[i])
                self.reqContractDetails(i, contract)

    def stop(self):
        self.disconnect()
####################################################################################################
### Sub routines
####################################################################################################
def buildQuickFXContract(fx_pair):
    contract = Contract()
    contract.symbol = fx_pair[:3]
    contract.secType = "CASH"
    contract.exchange = "IDEALPRO"
    contract.currency = fx_pair[-3:]
    return contract

def convertDBTimesIntoDateTime(dat, col_name):
    date_time = list(dat[col_name])
    date_time = map(lambda x: init.TODAY_STR + " " + str(x)[-8:], date_time)
    date_time = map(lambda x: datetime.strptime(x, "%Y-%m-%d %H:%M:%S"), date_time)
    date_time = map(lambda x: x if x.hour >= hour_new_day else x + timedelta(days = 1), date_time)
    return date_time

def calcTimeDifferencesBetweenExecutionTimesAndNow(exec_time):
    diff_vs_now = map(lambda x: (init.start_time - x).total_seconds() / 60, exec_time)
    diff_vs_now = map(
        lambda x: x if ((x >0) and (x<time_threshold_minutes_predict_vs_now)) else None, 
        diff_vs_now)
    return list(diff_vs_now)

@vu.tryDiagnosticNone()
def getMarketsToDo(execution_time_id):
    markets_id = list(init.INSTRUMENTS["market_id"][
        init.INSTRUMENTS["execution_time_id"] == execution_time_id])
    markets_id = list(set(markets_id))
    markets_id = map(str, markets_id)
    markets_id = ','.join(markets_id)
    return markets_id

def calcDiffBetweenNowAndDFTime(dat, col_name):
    date_time = convertDBTimesIntoDateTime(dat, col_name)
    diff_vs_now = list(map(lambda x: (init.start_time - x).total_seconds() / 60, date_time))
    return diff_vs_now

@vu.tryDiagnosticNone()
def getModelPredictions(execution_time_id = None):
    markets_id = getMarketsToDo(execution_time_id)
    
    sql_q = """SELECT P.instrument_id, P.strategy_id, P.date, P.timestamp, P.timestamp_px, 
        P.close, P.t_up, P.t_dn, T.outcome AS predict, P.proba_up, P.proba_flat, P.proba_down
        FROM live_predict P
        LEFT JOIN static_instrument I ON I.instrument_id = P.instrument_id
        LEFT JOIN static_market M ON M.market_id = I.market_id
        LEFT JOIN static_trade_outcome T ON T.outcome_id = P.outcome_id
        WHERE P.score = 10"""
    if execution_time_id is not None:
        sql_q = sql_q + " AND M.market_id IN ({})""".format(markets_id)
    return db.select(sql_q)

@vu.tryDiagnosticNone()
def compareTimesNowVsPriceAndPredict(dat):
    dat["diff_vs_now_px"] = calcDiffBetweenNowAndDFTime(dat, "timestamp_px")
    dat["diff_vs_now_predict"] = calcDiffBetweenNowAndDFTime(dat, "timestamp")
    return dat

@vu.tryDiagnosticNone()
def keepOnlyVeryRecentPredictions(dat):
    dat = dat[(
        (dat["diff_vs_now_px"] > 0) 
        & (dat["diff_vs_now_px"] < time_threshold_minutes_predict_vs_now) 
        & (dat["diff_vs_now_predict"] > 0) 
        & (dat["diff_vs_now_predict"] < time_threshold_minutes_predict_vs_now) 
        )]
    return dat

@vu.tryDiagnosticNone()
def buildModelComparisonTable():
    df = pd.DataFrame()
    for strat_id in STRATEGY_LIST:
        file_name = f"{init.DIRECTORY_DATA}Model_Diagnostic/model_diagnostic_{strat_id}_latest.csv"
        file_time = datetime.fromtimestamp(os.path.getmtime(file_name))
        time_diff = (init.start_time - file_time).total_seconds() / 60
        if time_diff <= 20:
            df_i = pd.read_csv(file_name)
            df = df.append(df_i)
    return df \
        .assign(predict_match = df["predict"] == df["predict_weight"]) \
        .merge(init.INSTRUMENTS, on="pair", how="left") \
        [["strategy_id", "instrument_id", "predict_match"]]

@vu.tryDiagnosticNone()
def compareWithWeightedModelAndKeepOnlyWhenPredictionAreTheSame(df):
 #   df_compare = buildModelComparisonTable()
 #   df = df.merge(df_compare, on=["strategy_id","instrument_id"], how="left")
 #   vu.printBanner("Now removing trades where models give different predictions")
 #   print("Before:")
 #   print(df)
 #   df = df[df["predict_match"]]
 #   print("Before:")
 #   print(df)
    return df

@vu.tryDiagnosticNone()
def reworkProbabiliies(df):
    df["proba_signal"] = 0
    df["proba_signal_plus_flat"] = np.nan
    df["proba_antisignal"] = np.nan
    pos_up = df["predict"] == "up"
    pos_down = df["predict"] == "down"
    pos_flat = df["predict"] == "flat"
    df["proba_signal"][pos_up] = df["proba_up"][pos_up]
    df["proba_signal"][pos_down] = df["proba_down"][pos_down]
    df["proba_signal"][pos_flat] = df["proba_flat"][pos_flat]
    df["proba_antisignal"][pos_up] = df["proba_down"][pos_up]
    df["proba_antisignal"][pos_down] = df["proba_up"][pos_down]
    df["proba_signal_plus_flat"][pos_up] = df["proba_up"][pos_up] + df["proba_flat"][pos_up]
    df["proba_signal_plus_flat"][pos_down] = df["proba_down"][pos_down] + df["proba_flat"][pos_down]    
    df["proba_diff"] = df["proba_signal"] - df["proba_antisignal"]
    return df

@vu.tryDiagnosticNone()
def keepOnlyTradesWithProbabilityThresholds(df):
    df = reworkProbabiliies(df)
    vu.printBanner("Now removing trades where probabilities not good enough")
    print("Before:")
    print(df)
    df = df[
        (df["predict"] == "flat") 
        | (
            (df["proba_diff"] >= 0.05) #0.06
            | (df["proba_signal"] >= 0.4) #0.425
            | (df["proba_signal_plus_flat"] >= 0.75)
            )
        ]
    print("After:")
    print(df)
    return df

@vu.tryDiagnosticNone()
def formattingPredictions(dat, execution_time_id):
    dat["execution_time_id"] = execution_time_id
    col_names = ["execution_time_id", "strategy_id", "instrument_id", "date", 
                 "timestamp", "timestamp_px", "close", "t_up", "t_dn", "predict"]
    return dat[col_names]

@vu.tryDiagnosticNone()
def prepareTradesListWhenTradingTime(execution_time_id):
    dat = getModelPredictions(execution_time_id)
    dat = compareTimesNowVsPriceAndPredict(dat)
    dat = keepOnlyVeryRecentPredictions(dat)
    dat = compareWithWeightedModelAndKeepOnlyWhenPredictionAreTheSame(dat)
    dat = keepOnlyTradesWithProbabilityThresholds(dat)
    dat = formattingPredictions(dat, execution_time_id)
    return dat

@vu.tryDiagnosticNone()
def prepareTradesList(execution_time_id):
    dat_trades = None
    if execution_time_id is not None:
        dat_trades = prepareTradesListWhenTradingTime(execution_time_id)
    return dat_trades

def getFXvsUSD():
    sql_q = """SELECT A.code, H.fx
            FROM (
                SELECT asset_id, MAX(date) as date
                FROM histo_fx_close_vs_usd
                GROUP BY asset_id
            ) D 
            LEFT JOIN histo_fx_close_vs_usd H ON D.asset_id = H.asset_id AND D.date = H.date
            LEFT JOIN static_asset A ON A.asset_id = H.asset_id"""
    return db.select(sql_q)

@vu.tryDiagnosticNone()
def prepareGitTableBasicColumns(dat):
    dat = dat \
        .merge(init.INSTRUMENTS, on=["execution_time_id", "instrument_id"], how="left") \
        .merge(init.MARKETS, on=["market", "market_id"], how="left") \
        .merge(init.REGIONS, on="region_id", how="left")

    dat = dat \
        .assign(
            code = dat.pair.map(lambda x: x[:3]),
            buy_sell = np.where(
                dat["predict"] == "up", 1, 
                np.where(dat["predict"] == "down", -1, 0)
            )
        )

    return dat.merge(fx_spot_vs_usd, on="code", how="left") 

@vu.tryDiagnosticNone()
def calcTargetsAndNotionals(dat):
    pos_flat = (dat["predict"] == "flat")
    dat = dat.assign(target = dat["close"] + dat["buy_sell"] * (dat["t_up"] - dat["close"]))
    dat["target"][pos_flat] = dat["t_up"][pos_flat]
    dat = dat.assign(stop_loss = 2 * dat["close"] - dat["target"])
    dat = dat.assign(pnl_tgt_ccy_1 = 1000 / dat["fx"])
    dat = dat.assign(tp_pct = dat["t_up"] / dat["close"] - 1)
    dat = dat.assign(notional_for_1k_pnl = dat["pnl_tgt_ccy_1"] / dat["tp_pct"])
    return dat

@vu.tryDiagnosticNone()
def addTradeId(dat, execution_time_id):
    base_trade_id = "V"
    base_trade_id = base_trade_id + init.TODAY.strftime("%Y%m%d")
    base_trade_id = base_trade_id + ("00" + str(execution_time_id))[-2:]
    row_list = list(map(lambda i: ("000" + str(i+1))[-3:], range(len(dat))))
    dat["trade_id"] = list(map(lambda x: base_trade_id + x, row_list))
    return dat

@vu.tryDiagnosticNone()
def addLatestExitDate(dat):
    # To do properly once we have the calendars
    dat["date_exit_latest"] = dat["date"].map(lambda d: d + timedelta(days = 7))
    return dat

@vu.tryDiagnosticNone()
def finalGitFileFormatting(dat):
    dat.rename({
        "date":"date_trade",
        "strategy_id":"strategy",
        "close":"price_entry",
        "t_up":"range_up",
        "t_dn":"range_dn",
        "timestamp":"timestamp_signal"
        }, axis=1, inplace=True)
    dat = dat[cols_github_file]
    return dat

@vu.tryDiagnosticNone()
def prepareGithubTableWhenWeHaveData(dat, execution_time_id):
    vu.printBanner(0, False)
    print(dat)
    dat = prepareGitTableBasicColumns(dat)
    vu.printBanner(1, False)
    print(dat)
    dat = calcTargetsAndNotionals(dat)
    vu.printBanner(2, False)
    print(dat)
    dat = addTradeId(dat, execution_time_id)
    vu.printBanner(3, False)
    print(dat)
    dat = addLatestExitDate(dat)
    vu.printBanner(4, False)
    print(dat)
    dat = finalGitFileFormatting(dat)
    vu.printBanner(5, False)
    print(dat)
    return dat

@vu.tryDiagnosticNone()
def prepareGithubTable(dat, execution_time_id):
    dat_github = empty_github_table
    if len(dat) != 0:
        dat_github = prepareGithubTableWhenWeHaveData(dat, execution_time_id)
    return dat_github

def checkIfDirectoryExistsAndCreateIfNot(file_path):
    if not os.path.exists(file_path):
        os.mkdir(file_path)
        
def prepareGitDirectories():
    file_path = PATH_GITHUB + "trades_new/" + init.TODAY.strftime("%Y-%m")
    checkIfDirectoryExistsAndCreateIfNot(file_path)
    file_path = file_path + "/" + init.TODAY_STR + "/"
    checkIfDirectoryExistsAndCreateIfNot(file_path)
    return file_path

@vu.tryDiagnosticNone()
def saveGithubFile(dat, execution_time_id):   
    file_path = prepareGitDirectories()
    
    file_name = "new_{}-{}.csv".format(
        init.TODAY.strftime("%Y%m%d"), 
        ("0" + str(execution_time_id))[-2:]
        )
    
    #file_path_export = "trades_new/{}/{}".format(init.TODAY.strftime("%Y-%m"), init.TODAY_STR)
    file_path_export = file_path[file_path.find("trades_new"):]
    dat.to_csv(file_path + file_name, index=False)
    return [file_path_export, file_name]

@vu.tryDiagnosticNone()    
def sendDataFileToGithub(file_path_name):
    file_path = file_path_name[0]
    file_name = file_path_name[1]
    os.system("GitPushVentura.sh {} {}".format(file_path, file_name))
    return "Done"

@vu.tryDiagnosticNone()  
def getLatestClosingNAV():
    sql_q = """SELECT N.account_id, N.nav_usd AS nav
        FROM (
        	SELECT account_id, MAX(timestamp) as timestamp
        	FROM book_nav
            WHERE HOUR(timestamp) <= 6
        	GROUP BY account_id
        ) D 
        LEFT JOIN book_nav N ON N.account_id = D.account_id AND N.timestamp = D.timestamp"""
    return db.select(sql_q)

@vu.tryDiagnosticNone()
def addBackInstrumentId(dat):
    dat = dat.merge(init.INSTRUMENTS[["ticker","instrument_id"]], on = "ticker", how = "left")
    return dat

@vu.tryDiagnosticNone()
def keepOnlyBuySellOrders(dat):
    dat = dat[abs(dat["buy_sell"]) == 1]
    return dat

@vu.tryDiagnosticNone()
def keepOnlyTradesWeCanDo(dat):
    instruments_to_keep = list(init.INSTRUMENTS[
        (init.INSTRUMENTS["use_for_trading_ib"] == 1)
     #   & (init.INSTRUMENTS["asset_class"].isin(["fx_dm","fx_em"]))
        ]["instrument_id"])
    dat = dat[dat["instrument_id"].isin(instruments_to_keep)]
    return dat
 
def groupInstrumentOrdersInCaseSeveralStrategiesGiveASignalOnIt(dat):
    dat_px = dat[["instrument_id", "price_entry"]].drop_duplicates()
    dat["notional_for_1k_pnl"] = dat["notional_for_1k_pnl"] * dat["buy_sell"]
    dat = dat.groupby("instrument_id", as_index=False)["notional_for_1k_pnl"].sum()
    dat = dat[dat["notional_for_1k_pnl"] != 0]    
    dat = dat.merge(dat_px, on = "instrument_id", how = "left")
    dat["buy_sell"] = dat["notional_for_1k_pnl"].map(vu.sign)
    dat = dat[["instrument_id", "buy_sell", "price_entry", "notional_for_1k_pnl"]]
    return dat

def calcNbTradesAlreadyDoneToday(execution_time_id, file_path_github):
    nb_trades = 0
    i = 1
    while i < execution_time_id:
        file_path = file_path_github[0]
        file_name = "new_{}-{}.csv".format(
            init.TODAY.strftime("%Y%m%d"), 
            ("0" + str(i))[-2:]
            )
        file_path_name = PATH_GITHUB + file_path + file_name
        try:
            dat_i = pd.read_csv(file_path_name)
            dat_i = addBackInstrumentId(dat_i)
            dat_i = dat_i[dat_i["predict"].isin(["up", "down"])]
            dat_i = keepOnlyTradesWeCanDo(dat_i)
            nb_trades = nb_trades + len(dat_i)
        except:
            pass
        i += 1
    return nb_trades
    
def estimateNbTradesLater(execution_time_id):
    nb_trades = 0
    execution_times = db.loadTableLocal("schedule_execution")
    max_execution_time_id = execution_times["execution_time_id"].max()
    i = execution_time_id  + 1
    while i <= max_execution_time_id:
        dat_i = getModelPredictions(i)
        dat_i = dat_i[dat_i["predict"].isin(["up", "down"])]
        dat_i = keepOnlyTradesWeCanDo(dat_i)
        nb_trades = nb_trades + len(dat_i)
        i += 1
    return nb_trades

def expectedTotalNbTradesToday(dat, execution_time_id, file_path_github):
    nb_trades_done_before_now = calcNbTradesAlreadyDoneToday(execution_time_id, file_path_github)
    print("nb_trades_done_before_now: " + str(nb_trades_done_before_now))
    nb_trades_this_run = len(dat)
    print("nb_trades_this_run: " + str(nb_trades_this_run))
    nb_trades_to_do_later = estimateNbTradesLater(execution_time_id)
    print("nb_trades_to_do_later: " + str(nb_trades_to_do_later))
    return nb_trades_done_before_now + nb_trades_this_run + nb_trades_to_do_later

def scaleDownNotionalsIfTooManyTrades(dat, execution_time_id, file_path_github):
    nb_trades_total = expectedTotalNbTradesToday(dat, execution_time_id, file_path_github)
    adj_ratio = MAX_NB_TRADES / max(MAX_NB_TRADES, nb_trades_total)
    dat["notional_for_1k_pnl"] = dat["notional_for_1k_pnl"].map(lambda x: x * adj_ratio)
    return dat

def calcSizeByScalingNotionals(dat, nav):
    dat["size"] = dat["notional_for_1k_pnl"].map(lambda x: x * nav / 100000)
    return dat

def roundNotionals(dat):
    dat["size"] = dat["size"].map(lambda x: vu.mround(x - 0.25, 1))
    dat = dat[dat["size"] > 0]
    return dat

def formatOrderListBeforeSaving(dat, account_id):
    dat.rename({"price_entry":"px_order"}, axis=1, inplace=True)
    dat["account_id"] = account_id
    dat = dat[["account_id", "strategy_id", "instrument_id", "ticker", "future_id", "conid", 
               "buy_sell", "size", "px_order"]]
    return dat

def prepareOrderDirectories():
    file_path = PATH_ORDERS + init.TODAY.strftime("%Y-%m")
    checkIfDirectoryExistsAndCreateIfNot(file_path)
    file_path = file_path + "/" + init.TODAY_STR + "/"
    print(file_path)
    checkIfDirectoryExistsAndCreateIfNot(file_path)
    return file_path

@vu.tryDiagnosticNone()
def saveOrderListFileByAccount(account_id, dat, execution_time_id):
    file_path = prepareOrderDirectories()
    file_name = "orders_{}_{}-{}.csv".format(
        str(account_id),
        init.TODAY.strftime("%Y%m%d"), 
        ("0" + str(execution_time_id))[-2:]
        )
    file_path_name = file_path + file_name
    dat.to_csv(file_path_name, index=False)
    return dat
    
@vu.tryDiagnosticNone()
def prepareOrderListByAccount(account_id, dat, dat_nav, execution_time_id):
    nav = dat_nav[dat_nav["account_id"] == account_id]["nav"].values[0]
    dat = calcSizeByScalingNotionals(dat, nav)
    dat = formatOrderListBeforeSaving(dat, account_id)
    dat = roundNotionals(dat)
    dat.dropna(subset = ["conid"], inplace=True)
    dat.dropna(subset = ["size"], inplace=True)
    dat.dropna(subset = ["px_order"], inplace=True)
    saveOrderListFileByAccount(account_id, dat, execution_time_id)
    return dat

@vu.tryDiagnosticDat()
def addFuturesInformation(dat):
    dat_fut = dat[["instrument_id"]]
    dat_fut = dat_fut.merge(FUTURES, on="instrument_id", how="left")
    dat_fut.dropna(subset = ["future_id"], inplace=True)
    dat_fut = dat_fut[dat_fut["tradable"] == 1]
    dat_fut = dat_fut.merge(FUTURES_ACTIVE, on="future_id", how="left")
    dat_fut = dat_fut.merge(FUTURES_EXPIRY, on=["future_id","conid"], how="left")
    vu.printBanner("Before adding future prices:", False)
    print(dat_fut)
    dat_fut = dat_fut.merge(FUTURES_PRICE, on="conid", how="left")
    vu.printBanner("Recent future prices:", False)
    print(dat_fut)
    dat_fut = dat_fut.merge(init.CURRENCIES, on="ccy_id", how="left")
    
    dat_fut.rename({"ccy":"code"}, axis=1, inplace=True)
    dat_fut = dat_fut.merge(fx_spot_vs_usd, on="code", how="left")
    dat_fut.rename({"code":"ccy_future", "fx":"fx_future", "notional":"notional_future"}, 
                   axis=1, inplace=True)
    
    dat_fut = dat_fut[[
        "instrument_id", "future_id", "ib_symbol","expiry","conid", "price",
        "ccy_future", "fx_future", "notional_future"]]
    vu.printBanner("dat_fut before removing missing prices:", False)
    print(dat_fut)
    dat_fut.dropna(subset = ["price"], inplace=True)
    
    dat = dat.merge(dat_fut, on="instrument_id", how="left")
    dat = dat.merge(
        init.INSTRUMENTS[["instrument_id", "asset_class", "ccy"]], 
        on="instrument_id",how="left"
        )
    
    dat.rename({"ccy":"code"}, axis=1, inplace=True)
    dat = dat.merge(fx_spot_vs_usd, on="code", how="left")
    dat.rename({"code":"ccy_instrument", "fx":"fx_instrument"}, axis=1, inplace=True)
    
    return dat

def recomputeNotionalsForFutures(dat):
    dat = dat.assign(
        notional_future_for_1k_pnl = dat["notional_for_1k_pnl"] / dat["notional_future"]
         * dat["fx_instrument"] / dat["fx_future"]
        )
    return dat

@vu.tryDiagnosticDat() 
def retrieveConIdForFXInstruments(dat):
    missing_instruments = dat[dat["asset_class"].isin(["fx_dm","fx_em"])]
    missing_instruments = missing_instruments[missing_instruments["conid"].isnull()]
    ticker_list = list(missing_instruments["ticker"])
    if len(ticker_list) > 0:
        ib_app = IBContracts(ticker_list)
        ib_app.connect("127.0.0.1", vx.getIBPort(IB_ACCOUNT_ID), vx.getIBClientId(script_name))
        ib_app.run()
        for i in range(len(dat)):
            if dat["ticker"][i] == ticker_list[i]:
                dat["conid"][i] = ib_app.conid_list[i]
    return dat
    
@vu.tryDiagnosticDat()
def finalOrderFormatting(dat):
    for i in range(len(dat)):
        if not dat["notional_future"].isnull()[i]:
            dat["notional_for_1k_pnl"][i] = dat["notional_future_for_1k_pnl"][i]
            dat["price_entry"][i] = dat["price"][i]
            dat["range_up"][i] = dat["price_entry"][i] * (1 + dat["tp_pct"][i])
            dat["range_dn"][i] = 2 * dat["price_entry"][i] - dat["range_dn"][i]
            dat["target"][i] = dat["price_entry"][i] * (1 + dat["buy_sell"][i] * dat["tp_pct"][i])
            dat["stop_loss"][i] = 2 * dat["price_entry"][i] - dat["target"][i]
            dat["ticker"][i] = dat["ib_symbol"][i]
    
    dat = dat.rename({"strategy":"strategy_id"}, axis=1)
    dat = dat[[
        "trade_id", "asset_class", "date_trade", "strategy_id", "ticker", 
        "instrument_id", "future_id", "expiry", "conid",
        "predict", "buy_sell", 
        "price_entry", "target", "stop_loss", "date_exit_latest", "range_dn", "range_up", "tp_pct", 
        "notional_for_1k_pnl", "timestamp_px", "timestamp_signal"
        ]]
    return dat

@vu.tryDiagnosticDat() 
def convertIndexIntoFutures(dat):
    dat = addFuturesInformation(dat)
    dat = recomputeNotionalsForFutures(dat)
    dat = retrieveConIdForFXInstruments(dat)
    dat = finalOrderFormatting(dat)
    return dat
        
@vu.tryDiagnosticNone()    
def prepareOrdersList(dat, execution_time_id, file_path_github):
    vu.printBanner("Initial data")
    print(dat)
    dat = addBackInstrumentId(dat)
    dat = keepOnlyBuySellOrders(dat)
    dat = keepOnlyTradesWeCanDo(dat)
    dat = scaleDownNotionalsIfTooManyTrades(dat, execution_time_id, file_path_github)
    dat_nav = getLatestClosingNAV()
    dat = convertIndexIntoFutures(dat)
    res = []
    for account_id in list(ACCOUNTS["account_id"]):
        res_i = prepareOrderListByAccount(account_id, dat, dat_nav, execution_time_id)
        res.append(res_i)
    print(res)
    return res
    
####################################################################################################
### Script
####################################################################################################
if __name__ == "__main__":
  # vx.waitTillPreviousJobHasFinished("Predict", 1, 5, 5, 4)
    global fx_spot_vs_usd
    fx_spot_vs_usd = getFXvsUSD()
    dat_trades = prepareTradesList(execution_time_id)
    dat_github = prepareGithubTable(dat_trades, execution_time_id)
    file_path_github = saveGithubFile(dat_github, execution_time_id)
    sendDataFileToGithub(file_path_github)
    prepareOrdersList(dat_github, execution_time_id, file_path_github)
    init.scriptFinish(script_name)