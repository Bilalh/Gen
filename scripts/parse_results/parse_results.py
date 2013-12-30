#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain
from pathlib import Path
from pprint import pprint, pformat
import sqlite3

METHODS = ["markov", "uniform", "nsample", "smac", "ksample" ]

cached_results = {}

def parse_results(
		base_str,
		filterer=None,
		xfunc=lambda num, row: num,
		yfunc=lambda row: row['quality'],
		):

	base = Path(base_str)
	info_db = base / "results" / "Info.db"
	conn = sqlite3.connect(str(info_db))
	conn.row_factory = sqlite3.Row

	i = 0

	def do_method(method_name):

		data = []

		if filterer:
			execute = conn.execute("SELECT * FROM everything Where method = ? AND NOT {0} ISNULL ORDER BY {0}".format(filterer), (method_name,) )
		else:
			execute = conn.execute("SELECT * FROM everything Where method = ?", (method_name,) )

		for row in execute:
			# print(dict(row))
			results_db = base / row['output_dir'] / "results.db"

			try:
				row = cached_results[results_db]
			except KeyError:
				results_conn = sqlite3.connect(str(results_db))
				# results_conn.row_factory = sqlite3.Row
				quality_row = results_conn.execute("SELECT min(quality) FROM  DiscriminatingParams Limit 1")
				quality = list(quality_row)[0][0]

				discriminating_count_row = results_conn.execute("SELECT count(quality) FROM  DiscriminatingParams")
				discriminating_count = list(discriminating_count_row)[0][0]

				if not quality:
					quality = 1.5
				# print(quality)

				row = dict(row)
				del row['output_dir']
				del row['method']
				row['quality'] = quality
				row['discriminating'] = discriminating_count

				row = { k: v for (k, v) in row.items() if v is not None }
				results_conn.close()
				cached_results[results_db] = row

			nonlocal i
			x = xfunc(i, row)
			y = yfunc(row)

			def format(val):
				if isinstance(val, float) and val <=1 and val >=0:
					return "%.5a" % val
				else:
					return val

			info = str({ k: format(v) for (k, v) in row.items() })

			info = info.replace("'", "")
			info = info.replace("{", "")
			info = info.replace("}", "")

			d = {"x": x, "y": y,  "parts": info }
			d.update(row)
			data.append( d )
			i += 1

		print(method_name, len(data))
		return data

	methods_data = { k: do_method(k) for k in METHODS }

	return methods_data
	conn.close()


if __name__ == '__main__':
	from itertools import groupby
	data = parse_results(
		"/Users/bilalh/Desktop/Experiments",
		filterer="influence_radius"
	)

	for (i, (k, g)) in enumerate(groupby(data['nsample'], key=lambda d: d['influence_radius'] )):
		pprint( (k, i))


