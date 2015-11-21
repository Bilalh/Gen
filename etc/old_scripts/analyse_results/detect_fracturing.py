#!/usr/local/bin/python3
# -*- coding: utf-8 -*-

from pprint import pprint
from pathlib import Path
import argparse
import csv
import os
import sqlite3


parser = argparse.ArgumentParser(prog='dectect_fracturing')
parser.add_argument("db",                         help="db path")
parser.add_argument("-q --quiet", dest='quiet',   help="only print the resulting set", action='store_true')
args = parser.parse_args()
# args.db="/Users/bilalh/Desktop/Experiment/aa/n_minion_iter30/results/nsample/prob013-PPP/out-30-0-8__1___4_False_True_False/results.db"

for e in ['asc ', 'desc']:
	instances = []
	with sqlite3.connect(args.db) as conn:
		instances =[ (name,  set(eprimes.split(", ") ) ) for (name, eprimes) in
		conn.execute( "Select paramHash, eprimes from ParamsData where quality < 1 order by quality {}".format(e) )]

	resulting = instances[0][1]
	for (name, eprimes) in instances[1:]:
		old_resulting = set(resulting)
		resulting &= eprimes
		if not args.quiet:
			print("{} {} ({:3d} eprimes)  resulting(size {}):{}".format(e, name[1:8], len(eprimes), len(resulting), resulting) )
		elif len(resulting) == 0 and len(old_resulting) != 0:
			print("{} {} ({:3d} eprimes)  before_empty(size {}):{}".format(e, name[1:8], len(eprimes), len(old_resulting), old_resulting) )


print("Result(size {}) {}".format(len(resulting), sorted(resulting)) )