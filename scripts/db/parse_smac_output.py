#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import argparse
import os
from pprint import pprint

import glob
import re
import csv


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
pprint(args)

os.chdir(args.output_dir)


results = glob.glob('smac-output/state-run*/runs_and_results*csv')
print(results)

names = glob.glob('smac-output/state-run*/paramstrings*.txt')
print(names)


if len(results) !=1 or len(names) != 1:
	print("Invaild smac dir {}".format(args.output_dir) )
	exit(44)

results = results[0]
names = names[0]



def read_param_data(fp):
	""" Read the param names outputted by smac """
	with open(fp) as csvfile:
		lines = csvfile.readlines()

		remove = re.compile(r"^\d+:\s*")
		names_re = re.compile(r"(\w+)=")

		names = names_re.findall(lines[0])
		print(names)

		def f(line):
			return names_re.sub("", remove.sub("", line))

		lines = [", ".join(names)] + [  f(line) for line in lines ]

		reader = csv.DictReader(lines, skipinitialspace=True, quoting=csv.QUOTE_ALL)
		rows = [ { k: int(v[1:-1]) for (k, v) in row.items() } for row in reader ]
		return rows


param_values = read_param_data(names)
pprint(param_values)



