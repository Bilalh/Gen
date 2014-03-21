#!/usr/local/bin/python3.4
# -*- coding: utf-8 -*-

import csv
import statistics
import argparse

parser = argparse.ArgumentParser(prog='number_of_solutions')
parser.add_argument("csvfile", help='With a column for each find & `solution`: total solutions ')

args = parser.parse_args()
fp = args.csvfile

# fp="/Users/bilalh/Desktop/counts.csv"
# fp="/Users/bilalh/Desktop/counts_no_where.csv"


def approx_calucation(d):
	res =(d['n_upper'] ** d['n_boats']) ** 2 * d['n_periods_max']
	return res


with open(fp) as f:
	reader = csv.DictReader(f, skipinitialspace=True)
	pcount = 0
	scount = 0
	acount = 0
	results = []

	for row in reader:
		d ={ k: int(v) for (k, v) in row.items()  }

		approx = approx_calucation(d)
		pcount+=1
		acount+=approx

		d['approx'] = approx
		results.append( d )

		scount += d['solutions']


	for result in results:
		result['sols_p'] = result['solutions'] / scount * 100
		result['even_p'] = 1 / pcount * 100
		result['approx_p'] = result['approx'] / acount * 100

		result['even_p_diff'] = abs(  result['even_p'] - result['sols_p'] )
		result['approx_p_diff'] = abs(  result['approx_p'] - result['sols_p'] )

	print("{n_upper:>7} {n_boats:>7} {n_periods_max:>13}"
			" {solutions:>13} {sols_p:>9}"
			" {even_p:>9} {even_p_diff:>9}"
			" {approx_p:>9} {approx_p_diff:>15}"
		.format(**{k: k for k in results[0].keys()}))

	for result in results:
		print("{n_upper:7} {n_boats:7} {n_periods_max:13}"
				" {solutions:13} {sols_p:9.4f}"
				" {even_p:9.4f} {even_p_diff:9.4f}"
				" {approx_p:9.4f} {approx_p_diff:9.4f}"
			.format(**result) )


	print("\nparams:{} total_solutions:{} approx_solutions:{}\n".format(
		pcount, scount, acount))

	print("% differences using various techniques")
	methods=[statistics.mean, statistics.stdev, statistics.variance]
	print(("{:>8} " * (len(methods) + 1)).format("", *[m.__name__ for m in methods] ))

	for name in ['even', 'approx']:
		vals = [  m( r[name + '_p_diff'] for r in results ) for m in methods ]
		print( ("{:8}" + " {:>8.4f}" * len(methods)) .format(name, *vals) )

	print()