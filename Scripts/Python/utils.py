#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug  5 10:12:30 2020

@author: fls
"""
####################################################################################################
### Imports
####################################################################################################
from datetime import datetime
from datetime import timedelta
import pytz
import random
import os

####################################################################################################
### Script variables
####################################################################################################
BANNER_SIZE = 100
HASH_BANNER = "#" * BANNER_SIZE

####################################################################################################
### Sub Routines
####################################################################################################
def sample(items_list, n = 1, with_replacement = False):
    if with_replacement:
        res = []
        for i in range(n):
            res.append(random.choice(items_list))
    else:
        res = random.sample(items_list, n)
    return res        
    
def shuffleString(this_string):
    return "".join(random.sample(this_string, len(this_string)))

def genPassword(n_char = 10, use_special_char = False):
    lower_case_codes = tuple(range(97, 123))
    upper_case_codes = tuple(range(65, 91))
    digit_codes = tuple(range(48, 58))
    special_codes = (33, 35, 36, 38, 42)
    all_codes = lower_case_codes + upper_case_codes + digit_codes
    
    password = chr(random.choice(lower_case_codes))
    password += chr(random.choice(upper_case_codes))
    password += chr(random.choice(digit_codes))
    
    if use_special_char:
        all_codes = all_codes + special_codes
        password += chr(random.choice(special_codes))
        
    while len(password) < n_char:
        password += chr(random.choice(all_codes))
    
    return shuffleString(password)

def mround(x, base):
    return base * round(x / base, 0)

def dateTimeFormatForExport():
    return datetime.now().strftime("%Y-%m-%d_%Hh%Mm%Ss")

def printBanner(doing_what, big_banner = True):
    doing_what = str(doing_what)
    hash_banner = "#" * BANNER_SIZE
    time_now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    size_timestamp = len(time_now)

    char_used = len(doing_what) + 9 + size_timestamp

    middle_line = f"### {doing_what} - {time_now}"
    
    if char_used <= BANNER_SIZE:
        fill_blanks = " " * (BANNER_SIZE - char_used)
        middle_line = f"### {doing_what}{fill_blanks} {time_now} ###"
    
    if big_banner:
        print(hash_banner)
    print(middle_line)
    if (big_banner):
        print(hash_banner)
        
def attachTZToDateTime(t_time, tz):
    used_tz = pytz.timezone(tz)
    return used_tz.localize(t_time)

def convertDateTimeFromTZToAnotherTZ(t_time, tz_from, tz_to):
    t_from = attachTZToDateTime(t_time, tz_from)
    t_to = t_from.astimezone(pytz.timezone(tz_to))
    return t_to

        
def dateToday():
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
        
def calcPreviousDate(date_from):
    today_weekday = date_from.weekday()
    substract_days = 1
    if today_weekday == 0:
        substract_days = 3
    elif today_weekday == 6:
        substract_days = 2
    return date_from - timedelta(days = substract_days)

def variableExists(x):
    return (x in globals()) or (x in locals())
    

def tryWithDescription(
        print_diagnostics = True, 
        text_diagnostic = None,
        error_return_none_or_arg1 = "none"
        ):
    def decorator(function):
        used_text_diagnostic = text_diagnostic
        if text_diagnostic == "":
            used_text_diagnostic = function.__name__
        def wrapper(*args, **kwargs):
            if print_diagnostics:
                print(HASH_BANNER)
                printBanner(f"{used_text_diagnostic} - Starting", False)
                
            result = None
            if error_return_none_or_arg1 == "arg":
                result = args[0]
            try:
                result = function(*args, **kwargs)
            except Exception as this_exception: 
                if print_diagnostics:
                    printBanner(f"{used_text_diagnostic} - Error while running:", False)
                    print(this_exception)
            finally:
                if print_diagnostics:
                    printBanner(f"{used_text_diagnostic} - Done. Result:", False)
                    print(result)
                    print(HASH_BANNER)
                return result
        return wrapper
    return decorator

def trySimpleNone():
    return tryWithDescription(False, None, "none")

def trySimpleDat():
    return tryWithDescription(False, None, "arg")

def tryDiagnosticNone():
    return tryWithDescription(True, "", "none")

def tryDiagnosticDat():
    return tryWithDescription(True, "", "arg")

def sign(x):
    try:
        if x > 0:
            res = 1
        elif x < 0:
            res = -1
        elif x == 0:
            res = 0
    except:
        res = None
    finally:
        return res
    
def checkIfDirectoryExistsAndCreateIfNot(file_path):
    if not os.path.exists(file_path):
        os.mkdir(file_path)

def leftJoin(df_left, df_right, col_names):
    return df_left.merge(df_right, on=col_names, how="left")

def forceColumnType(df, col_list, type_function):
    for col_name in col_list:
        try:
            df[col_name] = df[col_name].map(type_function)
        except:
            pass
    return df