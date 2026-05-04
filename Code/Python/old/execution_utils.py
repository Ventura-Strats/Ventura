#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug  5 10:12:30 2020

@author: fls
"""
import utils as vu
import db
import init
from datetime import datetime
from datetime import timedelta
import time
import os

init.startInit("execution_utils")
init.initializeApplicationVariables()

winter_or_summer = "Summer"
hour_new_day = 6
time_threshold_minutes_predict_vs_now = 5
path_jobs = "/home/fls/Data/Ventura/SD/Log/"


#@vu.tryDiagnosticNone()
def getIBPort(account_id):
    ib_port = 7496
    if init.THIS_COMPUTER == "H":
        ib_port = 7498
    ib_port = ib_port + account_id - 1
    return ib_port

#@vu.tryDiagnosticNone()
def getIBClientId(script_name):
    dat = db.loadTableLocal("script")
    client_id = dat["ib_client_id"][dat["script"] == script_name].values[0]
    return int(client_id)

def executionDirectory():
    return init.DIRECTORY_DATA + "Executions/"

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

def findClosestAcceptableExecutionTime(diff_vs_now):
    try:
        min_diff = min(x for x in diff_vs_now if x is not None)
    except:
        min_diff = None
    return min_diff

@vu.tryDiagnosticNone()
def findExecutionTime():
    dat = db.loadTableLocal("schedule_execution")
    exec_time = convertDBTimesIntoDateTime(dat, "execution_time")
    diff_vs_now = calcTimeDifferencesBetweenExecutionTimesAndNow(exec_time)
    min_diff = findClosestAcceptableExecutionTime(diff_vs_now)

    execution_time_id = None
    if min_diff is not None:
        min_position = diff_vs_now.index(min_diff)
        execution_time_id = dat["execution_time_id"].values[min_position]

    return execution_time_id
    
def waitTillPreviousJobHasFinished(
        previous_script_name, last_completed_stage, 
        nb_jobs_total = 1, 
        max_lag_prices_minutes = 5,
        max_wait_time_before_continuing_minutes = 5
        ):
    
    wait_time_between_tests_seconds = 5
    
    SCRIPTS = db.loadTableLocal("script")
    script_id = SCRIPTS["script_id"][SCRIPTS["script"] == previous_script_name].values[0]
    
    vu.printBanner(f"Waiting till {previous_script_name} has completed: ")

    price_job_starttime_limit_chr = (datetime.now()
        - timedelta(minutes = max_lag_prices_minutes)) \
        .strftime("%Y-%m-%d %H:%M:%S")
    stop_waiting_time_limit = datetime.now() \
        + timedelta(minutes = max_wait_time_before_continuing_minutes)

    nb_jobs_done = 0
    have_we_waited_too_long = False
    still_wait = True
    while still_wait:
        vu.printBanner(f"Waiting... jobs done: {nb_jobs_done}/{nb_jobs_total}", False)
        time.sleep(wait_time_between_tests_seconds)
        sql_q = f"""SELECT *
            FROM status_script
            WHERE script_id = {script_id}
            AND start >= '{price_job_starttime_limit_chr}'
            AND last_completed_stage >= {last_completed_stage}"""
        dat = db.select(sql_q)
      #  print(dat)
        nb_jobs_done = len(dat)
        have_we_waited_too_long = (datetime.now() > stop_waiting_time_limit)
        
        still_wait = (nb_jobs_done < nb_jobs_total) and (not have_we_waited_too_long)
    vu.printBanner(f"Waiting finished ... jobs done: {nb_jobs_done}/{nb_jobs_total}")
    if have_we_waited_too_long:
        vu.printBanner(
            "WARNING the previous job has not finished and we'll continue with what we have"
            )
    
def checkIfStringIsInFile(file_name, string_to_search):
    with open(file_name, 'r') as read_obj:
        for line in read_obj:
            if string_to_search in line:
                return True
    return False

def getLatestFuturePricesFromDB(time_limit_chr = None):    
    sql_q = """SELECT J.conid, L.price
        FROM (
        	SELECT conid, MAX(timestamp) AS timestamp
            FROM live_px_future
            GROUP BY conid
        ) J
        LEFT JOIN live_px_future L
        ON L.conid = J.conid AND L.timestamp = J.timestamp"""
    if time_limit_chr is not None:
        sql_q = sql_q + f" WHERE J.timestamp >= '{time_limit_chr}'"
    dat_px = db.select(sql_q)
    return dat_px

def executionDataDirectory():
    return init.DIRECTORY_HOME + "Mount/Glenorchy_Fundamentals/LogFX/Execution/"