#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import argparse
import csv
import os
import sqlite3

parser = argparse.ArgumentParser(prog='parse_minion_tableout_sql')
parser.add_argument("minionTableout", help='from minion -tableout named {eprime}-{param}')
parser.add_argument("db",             help="db to put results in")
parser.add_argument("savileRowTime",  help="Time SR took", type=float)

args = parser.parse_args()

[eprime, *param] = os.path.basename(args.minionTableout).split(".")[0].split("-")
param = "-".join(param)

vs = []
with open(args.minionTableout, "r") as f:
	reader = csv.DictReader(f, skipinitialspace=True, delimiter=' ')
	for row in reader:
		del row['#"CommandLineArguments"']
		if "" in row: del row[""]
		for (k, v) in row.items():
			vs.append( (eprime, param, "Minion" + k, v) )

		vs.append((eprime, param, "SavileRowTotalTime", args.savileRowTime))

query = 'INSERT OR REPLACE INTO Experiment ( eprime, paramHash, attribute, value) VALUES(?,?,?,?)'
with sqlite3.connect(args.db) as conn:
	conn.executemany(query, vs)

print("Finished parsing", os.path.basename(args.minionTableout))