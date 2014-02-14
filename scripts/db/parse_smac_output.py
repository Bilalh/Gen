#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import argparse
import os
from pprint import pprint

import glob
import re
import csv

import sqlite3


def extant_file(parser, fp):
	abs_fp = os.path.abspath(os.path.expanduser(fp))
	if not os.path.isfile(abs_fp):
		raise parser.error("{} does not exist".format(abs_fp))
	return abs_fp


def output_dir_smac(parser, fp):
	abs_fp = os.path.abspath(os.path.expanduser(fp))
	if not os.path.isdir(abs_fp):
		raise parser.error("{} does not exist".format(abs_fp))

	if not os.path.isdir(os.path.join(abs_fp, "smac-output")):
		raise parser.error("Smac output in {} does not exist".format(abs_fp))

	if not os.path.isfile(os.path.join(abs_fp, "results.db")):
		raise parser.error("results.db missing in {}".format(abs_fp))

	return abs_fp

parser = argparse.ArgumentParser(prog='parse_smac_output')
parser.add_argument('--essence',    type=lambda x: extant_file(parser, x),     required=True)
parser.add_argument('--output_dir', type=lambda x: output_dir_smac(parser, x), required=True)

args = parser.parse_args()
print("<parse_smac_output> Start")
pprint(args)

os.chdir(args.output_dir)


results_fp = glob.glob('smac-output/state-run*/runs_and_results*csv')
print(results_fp)

names_fp = glob.glob('smac-output/state-run*/paramstrings*.txt')
print(results_fp)


if len(results_fp) !=1 or len(names_fp) != 1:
	print("Invaild smac dir {}".format(args.output_dir) )
	exit(44)

results_fp = results_fp[0]
names_fp = names_fp[0]


def read_results(fp):
	"Reads results"
	with open(fp, newline='') as csvfile:
		reader = csv.DictReader(csvfile, skipinitialspace=True, quoting=csv.QUOTE_ALL)
		return [float(row['Run Quality']) for row in reader ]


def read_param_data(fp):
	""" Read the param names outputted by smac """
	with open(fp) as csvfile:
		lines = csvfile.readlines()

		# remove the numbers from the start
		remove = re.compile(r"^\d+:\s*")

		# Get the fields names
		names_re = re.compile(r"(\w+)=")
		names = names_re.findall(lines[0])
		print(names)

		def f(line):
			return names_re.sub("", remove.sub("", line))

		lines = [", ".join(names)] + [  f(line) for line in lines ]

		reader = csv.DictReader(lines, skipinitialspace=True, quoting=csv.QUOTE_ALL)
		rows = [ { k: int(v[1:-1]) for (k, v) in row.items() } for row in reader ]
		return rows


def get_param_order():
	"""Find out the order of smac put the params values"""
	param_fp = glob.glob("params/*.param")[0]
	with open(param_fp, 'r') as f:
		let =re.compile(r"letting (\w+)")

		def g(line):
			return re.search(let, line).group(1)

		return [g(line) for line in f if line.startswith("letting") ]


def save_quality(output_dir, param_name, quality):
	conn = sqlite3.connect(os.path.join(output_dir, 'results.db'))
	conn.execute('INSERT OR REPLACE INTO ParamQuality(param, quality) Values(?, ?)', (param_name, quality))
	conn.commit()



results_values = read_results(results_fp)
pprint(results_values)

param_values = read_param_data(names_fp)
pprint(param_values)

ordering = get_param_order()
pprint(ordering)

for (param_hash, quality) in zip(param_values, results_values):
	quality_normalised = quality / 100
	# Have to parse the smac variables into the param name
	param_name = "-".join( ["{%s:03}" % p for p in ordering] ).format(**param_hash)
	pprint((param_name, quality_normalised))
	save_quality(args.output_dir, param_name, quality_normalised)


print("<parse_smac_output> End")
