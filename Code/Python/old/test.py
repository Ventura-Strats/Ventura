#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 15 15:41:55 2020

@author: fls
"""
import init
import db

script_name = "TESTTEST"

if __name__ == "__main__":
    print("This is a test")
    init.sessionInit(script_name)
    
    print("Init done")
  
    print("Leaving now")