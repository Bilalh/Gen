#!/usr/local/bin/python3
# -*- coding: utf-8 -*-

from pprint import pprint
from pathlib import Path
import argparse
import csv
import os
import sqlite3


parser = argparse.ArgumentParser(prog='dectect_fracturing')
parser.add_argument("db",             help="db path")
args = parser.parse_args()
# args.db="/Users/bilalh/Desktop/Experiment/aa/n_minion_iter30/results/nsample/prob013-PPP/out-30-0-8__1___4_False_True_False/results.db"

instances = []
with sqlite3.connect(args.db) as conn:
	instances =[ (name,  set(eprimes.split(", ") ) ) for (name, eprimes) in
		conn.execute( "Select paramHash, eprimes from ParamsData where quality < 1 order by quality asc" )]

resulting = instances[0][1]
for (name, eprimes) in instances[1:]:
	resulting &= eprimes
	print("{} ({:3d} eprimes)   resulting(size {}):{}".format(name[1:8], len(eprimes), len(resulting), resulting) )


print("Final results ".format( resulting) )