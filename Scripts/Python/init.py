#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 15 13:28:09 2020

@author: fls
"""
####################################################################################################
### Imports
####################################################################################################
import utils as ut
import os
import db
from datetime import datetime
import pandas as pd
from threading import Timer
pd.options.mode.chained_assignment = None
#import sys

####################################################################################################
### Script variables
####################################################################################################
start_time = datetime.now()
char_start_time = start_time.strftime("%Y-%m-%d %H:%M:%S")
pd.set_option("display.max_rows", None, "display.max_columns", None)
####################################################################################################
### Sub routines
####################################################################################################

def rootDirectoryBase():
    with open(DIRECTORY_HOME + "Data/System/Location_Projects.txt") as location_projects:
        file_path = location_projects.read() + "Ventura/HD/"
    return file_path.replace("\n", "")

def rootDirectoryData():
    with open(DIRECTORY_HOME + "Data/System/Location_Data.txt") as location_data:
        file_path = location_data.read() + "Ventura/HD/"
    return file_path.replace("\n", "")

def whichComputer():
    with open(DIRECTORY_HOME + "Data/System/this_computer.txt", "r") as file_computer:
        this_computer = file_computer.read(1)
    ut.printBanner(f"This computer: {this_computer}", False)
    return this_computer

def startInit(script_name):
    ut.printBanner("Main initialization - Start")
    global DIRECTORY_HOME
    DIRECTORY_HOME = os.path.expanduser("~") + "/"
    if script_name is not None:
        ut.printBanner("Script Name: " + script_name, False)
        ut.printBanner("Script PID: " + str(os.getpid()), False)
        
def initializeApplicationVariables():
    ut.printBanner("Initializating application parameters", False)
    global THIS_COMPUTER
    global TODAY
    global TODAY_STR
    global YESTERDAY
    global DIRECTORY_CODE
    global DIRECTORY_DATA
    THIS_COMPUTER = whichComputer()
    TODAY = ut.dateToday()
    TODAY_STR = TODAY.strftime("%Y-%m-%d")
    YESTERDAY = ut.calcPreviousDate(TODAY);
    ut.printBanner("System date: " + TODAY_STR, False)
    
    DIRECTORY_CODE = rootDirectoryBase()
    DIRECTORY_DATA = rootDirectoryData()
    os.chdir(DIRECTORY_CODE + "Scripts/Python/")
    ut.printBanner("Current directory: " + os.getcwd(), False)

def initializeInstrumentTable():
    ut.printBanner("Loading main data tables", False)
    global CURRENCIES
    global ASSETS
    global INSTRUMENTS
    global STRATEGIES
    global TRADE_OUTCOMES
    global TECH_PARAM
    global MARKETS
    global REGIONS
    
    INSTRUMENTS = db.loadTableLocal("INSTRUMENTS")
    for col_name in ["instrument_id", "use_for_training", "use_for_trading", 
                     "use_for_trading_ib", "market_id", "execution_time_id", 
                     "conid_spot", "is_etf"]:
        pos_nan = INSTRUMENTS[col_name].isnull()
        INSTRUMENTS[col_name][pos_nan] = 0
        INSTRUMENTS[col_name] = INSTRUMENTS[col_name].map(int)
    
    CURRENCIES = db.loadTableLocal("currency")
    ASSETS = db.loadTableLocal("asset")
    STRATEGIES = db.loadTableLocal("strategy")
    TRADE_OUTCOMES = db.loadTableLocal("trade_outcome")
    TECH_PARAM = db.loadTableLocal("technical_parameters")
    MARKETS = db.loadTableLocal("market")
    REGIONS = db.loadTableLocal("region")
  
def sessionInit(script_name = None):
    startInit(script_name)
    initializeApplicationVariables()
    initializeInstrumentTable()
    ut.printBanner("Main initialization - Done")
    
def getScriptId(script_name):
    SCRIPTS = db.loadTableLocal("script")
    return SCRIPTS["script_id"][SCRIPTS["script"] == script_name].values[0]

def getMachineId():
    MACHINES = db.loadTableLocal("machine")
    return MACHINES["machine_id"][MACHINES["machine"] == THIS_COMPUTER].values[0]

def testDirectoryNotEmpty(file_path):
    test_path = False
    try:
        file_list = os.listdir(file_path)
        if len(file_list) > 0:
            test_path = True
    except:
        pass
    return test_path

def testAccessHardDrives():
    test_code = testDirectoryNotEmpty(DIRECTORY_CODE)
    test_data = testDirectoryNotEmpty(DIRECTORY_DATA)
    return (test_code and test_data)

def testCanExecute():
    test_drives = testAccessHardDrives()
    test_db = db.testWriteDB()
    return (test_drives and test_db)

def addScriptInitialDetailsToDB(script_name):
    global script_id
    global machine_id
    global script_pid
    global script_run_id
    script_id = getScriptId(script_name)
    machine_id = getMachineId()
    script_pid = os.getpid()
    script_run_id = 0 
    sql_q = """INSERT INTO status_script 
        (script_id, start, complete, machine_id, pid, run_id) 
        VALUES ({}, '{}', 0, '{}', {}, {})""".format(
        script_id, char_start_time, machine_id, script_pid, script_run_id
        )
    db.executeSQL(sql_q)

def scriptInit(script_name, max_time_hours = 1):
    sessionInit(script_name)
    test_can_execute = testCanExecute()
    ut.printBanner("Good to start: " + str(test_can_execute))
    script_status = False
    if test_can_execute: 
        addScriptInitialDetailsToDB(script_name)
        script_status = True
    else:
        ut.printBanner("DB not writable - not running the script")
  #  max_time_seconds = max_time_hours * 3600
  #  Timer(max_time_seconds, scriptFinish(script_name)).start()
    return script_status

def scriptFinish(script_name):
    # Only update DB if script was properly initialized (script_id exists)
    if 'script_id' not in globals():
        ut.printBanner("Script finished (DB status not updated - init incomplete)")
        return

    char_end_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    completed_stage = 1
    sql_q = """UPDATE status_script
        SET complete = 1, end = '{}', last_completed_stage = {}
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
    db.executeSQL(sql_q)

    ut.printBanner("Script Complete - " + script_name)
   # quit()