# -*- coding: utf-8 -*-
"""
Created on Sat Sep 22 12:54:57 2018

@author: FLS
"""

import pandas as pd
# import MySQLdb as mysql
from sqlalchemy import create_engine
from datetime import datetime
# from datetime import timedelta
import utils as ut
import os
import atexit
import time

####################################################################################################
### Script variables
####################################################################################################
DIRECTORY_HOME = os.path.expanduser("~") + "/"
with open(DIRECTORY_HOME + "Data/System/Location_Projects.txt") as location_projects:
    DIRECTORY_CODE = (location_projects.read() + "Ventura/HD/").replace("\n", "")
with open(DIRECTORY_HOME + "Data/System/Location_Data.txt") as location_data:
    DIRECTORY_DATA_HD = (location_data.read() + "Ventura/HD/").replace("\n", "")

DB_USER = os.environ.get("VENTURA_DB_USER", "ventura")
DB_PASSWORD = os.environ.get("VENTURA_DB_PASSWORD")
DB_HOST = os.environ.get("VENTURA_DB_HOST", "192.168.0.37")

if not DB_PASSWORD:
    raise EnvironmentError(
        "VENTURA_DB_PASSWORD environment variable not set. Add to ~/.bashrc:\n"
        "  export VENTURA_DB_USER='ventura'\n"
        "  export VENTURA_DB_PASSWORD='your_password'"
    )

DB_CONNECT_STR = f'mysql+pymysql://{DB_USER}:{DB_PASSWORD}@{DB_HOST}/Ventura'

####################################################################################################
### Sub routines
####################################################################################################
@ut.trySimpleNone()
def executeSQL(sql_query):
    DB = create_engine(DB_CONNECT_STR)
    with DB.connect() as connection:
        connection.execute(sql_query)
    DB.dispose()

@ut.trySimpleNone()
def select(sql_query):
    DB = create_engine(DB_CONNECT_STR)
    with DB.connect() as connection:
        dat = pd.read_sql(sql_query, connection)
    DB.dispose()
    return dat

def dropTable(db_table_name):
    sql_query = "DROP TABLE %s" % db_table_name
    executeSQL(sql_query)
    
def loadTable(db_table_name):
    sql_query = "SELECT * FROM " + db_table_name
    return select(sql_query)
    
def loadTableLocal(tbl_name):
    # Technically not a DB function but used in the same context
    if tbl_name != "INSTRUMENTS" and tbl_name != "ETF":
        tbl_name = "static_" + tbl_name
    return pd.read_csv("{}Tables_Local/{}.csv".format(DIRECTORY_DATA_HD, tbl_name))
    
def createNewTableLikeExisting(db_table_name):
    tmp_tbl_name = "tmp_table"
    sql_create_new_tmp_table = "CREATE TABLE %s LIKE %s" % (tmp_tbl_name, db_table_name)
    executeSQL(sql_create_new_tmp_table)

def writeNewTable(db_table_name, data_table):
    DB = create_engine(DB_CONNECT_STR)
    with DB.connect() as connection:
        data_table.to_sql(name=db_table_name, con=connection, if_exists='fail', index=False)
    DB.dispose()

def appendToTable(db_table_name, data_table):
    DB = create_engine(DB_CONNECT_STR)
    with DB.connect() as connection:
        data_table.to_sql(name=db_table_name, con=connection, if_exists='append', index=False)
    DB.dispose()
    
def replaceToTable(db_table_name, data_table):
    DB = create_engine(DB_CONNECT_STR)
    with DB.connect() as connection:
        data_table.to_sql(name=db_table_name, con=connection, if_exists='replace', index=False)
    DB.dispose()
    
# def replaceToTableWithIndex(db_table_name, data_table, idx_col):
#     DB = create_engine(DB_CONNECT_STR)
#     with DB.connect() as connection:
#         data_table.to_sql(name=db_table_name, con=connection, if_exists='replace', index=False)
#     DB.dispose()
    
def nameTmpTable(script_name = None):
    tbl_script_name = "noscript"
    if script_name is not None:
        tbl_script_name = script_name[-15:]
    tbl_time = datetime.now().strftime("%m%d_%H%M")
    return "z{}_{}_{}".format(ut.genPassword(6, False), tbl_script_name, tbl_time)

def testWriteDB():
    test_db = False
    try:
        tmp_tbl_name = nameTmpTable()
        executeSQL("CREATE TABLE {} (test_txt CHAR(1) NULL)".format(tmp_tbl_name))
        executeSQL("INSERT INTO {} SET test_txt = 'A'".format(tmp_tbl_name))
        tbl_res = loadTable(tmp_tbl_name)
        dropTable(tmp_tbl_name)
        test_db = (len(tbl_res) == 1)
    except:
        pass
    finally:
        return test_db


def exitHandler():
#    ut.printBanner("Disconnecting from Database...")
#    try_disconnect = False
#    try:
#        print(DB)
#        DB.close()
#        print(DB)
#        try_disconnect = True
#   except:
#        pass
#    fail_success = "Failed"
#    if try_disconnect:
#        fail_success = "Success"
#    ut.printBanner("Disconnection: " + fail_success, False)
    ut.printBanner("Goodbye...")
    time.sleep(1)

####################################################################################################
### Sub routines
####################################################################################################
#ut.printBanner("Connecting to database", False)
#DB = dbConnect()
#print(DB)
atexit.register(exitHandler)
