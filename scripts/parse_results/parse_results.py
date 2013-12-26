#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain
from pathlib import Path
from pprint import pprint, pformat
import sqlite3


def parse_results(base_str):
	base = Path(base_str)
	info_db = base / "results" / "Info.db"
	conn = sqlite3.connect(str(info_db))
	conn.row_factory = sqlite3.Row

	i = 0

	def do_method(method_name):
		data = []
		for row in conn.execute("SELECT * FROM {}".format(method_name) ):
			# print(dict(row))
			results_db = base / row['output_dir'] / "results.db"

			results_conn = sqlite3.connect(str(results_db))
			# results_conn.row_factory = sqlite3.Row
			quality_row = results_conn.execute("SELECT min(quality) FROM  DiscriminatingParams Limit 1")
			quality = list(quality_row)[0][0]
			if not quality:
				quality = 1.5
			# print(quality)

			row = dict(row)
			del row['output_dir']
			del row['quality']
			del row['method']
			results_conn.close()

			info = str(dict(row))
			info = info.replace("'", "")
			info = info.replace("{", "")
			info = info.replace("}", "")

			nonlocal i
			data.append({"x": i, "y": quality,  "parts": info } )
			i += 1

		return data

	methods_data = { k: do_method(k) for k in ["markov", "uniform", "nsample" ] }

	return methods_data
	conn.close()


if __name__ == '__main__':
	data = parse_results("/Users/bilalh/Desktop/Experiments")
	pprint(data)

