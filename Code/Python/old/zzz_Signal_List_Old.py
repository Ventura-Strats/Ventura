#!/usr/bin/env python3
# -*- coding: utf-8 -*-

####################################################################################################
### Prepares trades list from DB after predictions have been saved in DB
### Then exports file to github
####################################################################################################

####################################################################################################
### Imports
####################################################################################################
import MySQLdb as mysql
import pandas as pd
from datetime import datetime
from datetime import timedelta
import pytz
import os

####################################################################################################
### Script variables
####################################################################################################
start_time = datetime.now()
script_name = "Signal_List"
max_time_hours = 0.25
tzhk = pytz.timezone("Asia/Hong_Kong")
str_today = start_time.strftime("%Y-%m-%d")
char_start_time = start_time.strftime("%Y-%m-%d %H:%M:%S")

cols_github_file = [
    "trade_id", "date_trade", "strategy", "ticker", "price_entry", "predict", "buy_sell",
    "target", "stop_loss", "date_exit_latest","range_dn", "range_up", 
    "tp_pct", "notional_for_1k_pnl", "timestamp_px", "timestamp_signal"
    ]

empty_github_table = pd.DataFrame(columns = cols_github_file)

####################################################################################################
### Script initialization
####################################################################################################
with open("/home/fls/Data/System/this_computer.txt", "r") as file_computer:
    this_computer = file_computer.read(1)
    
      
print("####################################################################################################")
print("### Starting Python script... " + char_start_time)
print("### Script Name: " + script_name) 
print("### This Computer: " + this_computer)
print("####################################################################################################")

DIRECTORY_MAIN = "/home/fls/Mount/GDrv/Model/Ventura/"

file_name = DIRECTORY_MAIN + "Data/static_{}.csv"
INSTRUMENTS = pd.read_csv(DIRECTORY_MAIN + "Data/INSTRUMENTS.csv")
TRADE_OUTCOMES = pd.read_csv(file_name.format("trade_outcome"))
MARKETS = pd.read_csv(file_name.format("market"))
REGIONS = pd.read_csv(file_name.format("region"))
MACHINES = pd.read_csv(file_name.format("machine"))
SCRIPTS = pd.read_csv(file_name.format("script"))
PATH_GITHUB = DIRECTORY_MAIN + "Git/Ventura/"

script_id = SCRIPTS["script_id"][SCRIPTS["script"] == script_name].values[0]
machine_id = MACHINES["machine_id"][MACHINES["machine"] == this_computer].values[0]
script_pid = os.getpid()
script_run_id = 0

db_connection = mysql.connect(
        host = "192.168.0.103",
        user = "ventura", 
        passwd = "psuY2oF4qq7B$Lw8U!If", 
        db="Ventura"
        )

sql_q = """INSERT INTO status_script 
        (script_id, start, complete, machine_id, pid, run_id) 
        VALUES ({}, '{}', 0, '{}', {}, {})""".format(
        script_id, char_start_time, machine_id, script_pid, script_run_id
        )
    
db_cursor = db_connection.cursor()
db_cursor.execute(sql_q)
db_connection.commit()
db_cursor.close()


####################################################################################################
### Classes
####################################################################################################

####################################################################################################
### Sub routines
####################################################################################################

def attachTZToDateTime(t_time, tz):
    used_tz = pytz.timezone(tz)
    return used_tz.localize(t_time)

def determineDateToday():
    tz_asia = "Asia/Hong_Kong"
    tz_america = "America/New_York"
    
    time_now = datetime.now()
    time_hk = attachTZToDateTime(time_now, tz_asia)
    time_ny = time_hk.astimezone(pytz.timezone(tz_america))
    date_today = time_hk.date()
    date_in_new_york = time_ny.date()
    
    if date_in_new_york != date_today:
        if date_today.weekday() != 0:
            hour_time_in_new_york = time_ny.hour
            if hour_time_in_new_york <= 17:
                date_today = date_today - timedelta(days = 1)
    
    this_weekday = date_today.weekday()
    add_days = 0
    if this_weekday == 5:
        add_days = 2
    elif this_weekday == 6:
        add_days = 1
    
    return date_today + timedelta(days = add_days)

def findExecutionTime():
    dat = pd.read_sql("SELECT * FROM static_schedule_execution", db_connection)
    dat["execution_time"] = str_today + " " + dat["execution_time"].map(lambda x: str(x)[-8:])
    dat["execution_time"] = dat["execution_time"].map(
        lambda x: datetime.strptime(x, "%Y-%m-%d %H:%M:%S")
        )
    dat["execution_time"] = dat["execution_time"].map(
        lambda x: x if x.hour >= 6 else x + timedelta(days = 1)
        )
    dat["diff_vs_now"] = dat["execution_time"].map(
        lambda x: (start_time - x).total_seconds() / 60
        )
    dat = dat.sort_values(by = "diff_vs_now")
    dat = dat[(dat["diff_vs_now"] > 0) & (dat["diff_vs_now"] < 10)]
    
    execution_time_id = None
    
    if len(dat) > 0:
        execution_time_id = dat["execution_time_id"].values[0]

    return execution_time_id



def getMarketsToDo(execution_time_id):
    sql_q = "SELECT execution_time FROM static_schedule_execution WHERE execution_time_id = "
    sql_q = sql_q + str(execution_time_id)
    dat = pd.read_sql(sql_q, db_connection)
    schedule_time = list(str_today + " " + dat["execution_time"].map(lambda x: str(x)[-8:]))
    schedule_time = map(lambda x: datetime.strptime(x, "%Y-%m-%d %H:%M:%S"), schedule_time)
    schedule_time = list(schedule_time)[0]
    schedule_time = tzhk.localize(schedule_time)
  
    sql_q = """SELECT DISTINCT M.market_id, M.market, M.execution_time_tz_close, T.time_zone 
          FROM static_instrument I
          LEFT JOIN static_market M ON M.market_id = I.market_id
          LEFT JOIN static_time_zone T ON T.time_zone_id = M.tz_close_id
          WHERE I.use_for_trading_ib = 1
          ORDER BY M.market_id"""
    dat = pd.read_sql(sql_q, db_connection)
    market_time = list(str_today + " " + dat["execution_time_tz_close"].map(lambda x: str(x)[-8:]))
    market_time = map(lambda x: datetime.strptime(x, "%Y-%m-%d %H:%M:%S"), market_time)
    market_time = list(market_time)
    tz_list = list(dat["time_zone"])
    
    for i in range(len(market_time)):
        market_time[i] = attachTZToDateTime(market_time[i], tz_list[i])
  
    markets_id = list(filter(lambda x: x == schedule_time, market_time))
    markets_id_pos = [i for i,val in enumerate(market_time) if val == schedule_time]
    markets_id = list(dat["market_id"][markets_id_pos])
    markets_id = map(str, markets_id)
    markets_id = ",".join(markets_id)
    
    if len(markets_id) == 0:
        markets_id = None
  
    return markets_id

def prepareTradesListWhenTradingTime(execution_time_id):
    markets_id = getMarketsToDo(execution_time_id)
    
    sql_q = """SELECT P.instrument_id, P.strategy_id, P.date, P.timestamp, P.timestamp_px, 
        P.close, P.t_up, P.t_dn, T.outcome AS predict
        FROM live_predict P
        LEFT JOIN static_instrument I ON I.instrument_id = P.instrument_id
        LEFT JOIN static_market M ON M.market_id = I.market_id
        LEFT JOIN static_trade_outcome T ON T.outcome_id = P.outcome_id
        WHERE P.score = 10
        AND M.market_id IN ({})""".format(markets_id)
    dat = pd.read_sql(sql_q, db_connection)
    
    time_px = list(str_today + " " + dat["timestamp_px"].map(lambda x: str(x)[-8:]))
    time_px = list(map(lambda x: datetime.strptime(x, "%Y-%m-%d %H:%M:%S"), time_px))
    diff_vs_now_px = list(map(lambda x: (start_time - x).total_seconds() / 60, time_px))
    dat["diff_vs_now_px"] = diff_vs_now_px
    
    time_predict = list(str_today + " " + dat["timestamp"].map(lambda x: str(x)[-8:]))
    time_predict = list(map(lambda x: datetime.strptime(x, "%Y-%m-%d %H:%M:%S"), time_predict))
    diff_vs_now_predict = list(map(lambda x: (start_time - x).total_seconds() / 60, time_predict))
    dat["diff_vs_now_predict"] = diff_vs_now_predict
        
    dat = dat[(
        (dat["diff_vs_now_px"] > 0) 
        & (dat["diff_vs_now_px"] < 10) 
        & (dat["diff_vs_now_predict"] > 0) 
        & (dat["diff_vs_now_predict"] < 10) 
        )]
    
    dat["execution_time_id"] = execution_time_id
    
    dat = dat[[
        "execution_time_id", "strategy_id", "instrument_id", "date", "timestamp", "timestamp_px",
        "close", "t_up", "t_dn", "predict"
        ]]
       
    return dat

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

    return pd.read_sql(sql_q, db_connection)

def formatGithubTableWithData(dat, execution_time_id):
    TODAY = determineDateToday()
    fx_spot_vs_usd = getFXvsUSD()
    
    dat = dat.merge(INSTRUMENTS, on = "instrument_id", how = "left")
    dat = dat.merge(MARKETS, on = "market", how = "left")
    dat = dat.merge(REGIONS, on = "region_id", how = "left")
    dat["code"] = dat["pair"].map(lambda x: x[:3])
    dat = dat.merge(fx_spot_vs_usd, on = "code", how = "left")
    dat["buy_sell"] = 0
    dat["buy_sell"][dat["predict"] == "up"] = 1
    dat["buy_sell"][dat["predict"] == "down"] = -1

    pos_flat = (dat["predict"] == "flat")

    dat = dat.assign(target = dat["close"] + dat["buy_sell"] * (dat["t_up"] - dat["close"]))
    dat["target"][pos_flat] = dat["t_up"][pos_flat]
    dat = dat.assign(stop_loss = 2 * dat["close"] - dat["target"])
    dat = dat.assign(pnl_tgt_ccy_1 = 1000 / dat["fx"])
    dat = dat.assign(tp_pct = dat["t_up"] / dat["close"] - 1)
    dat = dat.assign(notional_for_1k_pnl = dat["pnl_tgt_ccy_1"] / dat["tp_pct"])
    
    base_trade_id = "V"
    base_trade_id = base_trade_id + TODAY.strftime("%Y%m%d")
    base_trade_id = base_trade_id + ("00" + str(execution_time_id))[-2:]
    row_list = list(map(lambda i: ("000" + str(i+1))[-3:], range(len(dat))))

    dat["trade_id"] = list(map(lambda x: base_trade_id + x, row_list))

    dat["date_exit_latest"] = dat["date"].map(lambda d: d + timedelta(days = 7))

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

def formatGithubTable(dat, execution_time_id):
    dat_github = empty_github_table
    if len(dat) != 0:
        dat_github = formatGithubTableWithData(dat, execution_time_id)
    return dat_github

def saveGithubFile(dat, execution_time_id):
    TODAY = determineDateToday()
    
    file_path = PATH_GITHUB + "trades_new/" + TODAY.strftime("%Y-%m")
    if not os.path.exists(file_path):
        os.mkdir(file_path)
        
    file_path = file_path + "/" + TODAY.strftime("%Y-%m-%d") + "/"
    if not os.path.exists(file_path):
        os.mkdir(file_path)
    
    file_name = "new_{}-{}.csv".format(
        TODAY.strftime("%Y%m%d"), 
        ("0" + str(execution_time_id))[-2:]
        )
    
    
    file_path_export = "trades_new/{}/{}".format(
        TODAY.strftime("%Y-%m"), 
        TODAY.strftime("%Y-%m-%d")
        )
    dat.to_csv(file_path + file_name, index=False)
    return [file_path_export, file_name]
    
def sendDataFileToGithub(file_path_name):
    file_path = file_path_name[0]
    file_name = file_path_name[1]
    os.system("GitPushVentura.sh {} {}".format(file_path, file_name))
    return "Done"
    


####################################################################################################
### Script
####################################################################################################
if __name__ == "__main__":

    execution_time_id = findExecutionTime()
    dat_trades = prepareTradesList(execution_time_id)
    dat_github = formatGithubTable(dat_trades, execution_time_id)
    
    file_path_github = saveGithubFile(dat_github, execution_time_id)
    sendDataFileToGithub(file_path_github)
    
    char_end_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    completed_stage = 1
    
    sql_q = """UPDATE status_script 
        SET end =  '{}', last_completed_stage = {} 
        WHERE script_id = {} 
        AND start = '{}' 
        AND machine_id = {} 
        AND run_id = {} 
        AND pid = {}""".format(
                char_end_time, 
                completed_stage, 
                script_id, 
                char_start_time,
                machine_id,
                script_run_id,
                script_pid
            )

    db_cursor = db_connection.cursor()
    db_cursor.execute(sql_q)
    db_connection.commit()
    db_cursor.close()
    
    db_connection.close()
    
    print("####################################################################################################")
    print("### Finished Python script... " + start_time.strftime("%Y-%m-%d %H:%M:%S"))
    print("### Script Name: " + script_name) 
    print("####################################################################################################")