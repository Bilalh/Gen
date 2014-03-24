#!/usr/local/bin/python3
# -*- coding: utf-8 -*-
# Approximate the number of solutions to essence spec

import csv
import argparse

parser = argparse.ArgumentParser(prog='approx_solutions')
parser.add_argument("csvfile", help='With a column for each relevant find')

# args = parser.parse_args()
# fp = args.csvfile

fp="/Users/bilalh/Desktop/pppa.csv"


def approx_calucation(d):
	res =(d['n_upper'] ** d['n_boats']) ** 2 * d['n_periods_max']
	return res


with open(fp) as f:
	reader = csv.DictReader(f, skipinitialspace=True)
	results = []

	param_names = list(reader.fieldnames)

	for row in reader:
		d ={ k: int(v) for (k, v) in row.items()  }
		d['approx'] = approx_calucation(d)
		results.append(d)

	results = sorted(results, key=lambda l: l['approx'], reverse=False )
	fmt_str = " ".join( "{%s:>%d}" % (name, len(name)) for name in param_names )

	print( (fmt_str + " approx").format(**{k: k for k in results[0].keys()}) )

	for fields in results:
		print( (fmt_str + " {approx}").format(**fields) )
