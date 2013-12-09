#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import argparse
import csv
import os
import sqlite3

parser = argparse.ArgumentParser(prog='parse_minion_tableout')
parser.add_argument("minionTableout", help='from minion -tableout named {eprime}-{param}')
parser.add_argument("db",             help="db to put results in")
parser.add_argument("savileRowTime",  help="Time SR took", type=float)

args = parser.parse_args()

[eprime, *param] = os.path.basename(args.minionTableout).split(".")[0].split("-")
param = "-".join(param)

with sqlite3.connect(args.db, timeout=10) as conn:
	with open(args.minionTableout, "r") as f:
		reader = csv.DictReader(f, skipinitialspace=True, delimiter=' ')
		for row in reader:
			del row['#"CommandLineArguments"']
			if "" in row: del row[""]
			for (k, v) in row.items():
				conn.execute('INSERT OR REPLACE INTO Experiment ( eprime, param, attribute, value) VALUES(?,?,?,?)',
					(eprime, param, "Minion" + k, v) )

	conn.execute('INSERT OR REPLACE INTO Experiment ( eprime, param, attribute, value) VALUES(?,?,?,?)',
					(eprime, param, "SavileRowTotalTime", args.savileRowTime) )

print("Finished parsing", os.path.basename(args.minionTableout) )