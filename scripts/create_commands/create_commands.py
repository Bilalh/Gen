#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import argparse
import itertools
import json
import os
import sqlite3

from distutils import dir_util
from distutils import file_util
from itertools import groupby
from pprint import pprint

import bash
import ksample
import markov
import nsample
import smac
import uniform


def read_json(fp):
	os.path.abspath(os.path.expanduser(fp))
	with open( fp ) as f:
		json_in = f.read()
	return json.loads(json_in)


def common_product(data):
	common = data['common_settings']
	# convert to seconds
	common['total_time'] = [ t * 60 * 60 for t in common['total_time'] ]

	zipped =[ [(k, v) for v in vs] for (k, vs) in common.items() ]
	return [dict(kv) for kv in itertools.product(*zipped)]


def write_with_header(fp, lines):
	header =[
		"#!/bin/bash",
		"# Assumes python3 is on the $PATH ",
		"# and docopt is installed (i.e pip install docopt) ",
		""
	]

	with open(fp, "w") as f:
		f.write("\n".join(header))
		f.write("\n".join(lines))
	print("Wrote", fp)
	return fp



def run(fp, place_dir, num_runs):
	data = read_json(fp)
	commons = common_product(data)

	essences_dir = os.path.join(place_dir, "results", "essences")
	os.makedirs(essences_dir, exist_ok=True)

	# copy the essences and eprime
	# and changes the paths to use our copy of the eprimes and essence's
	for values in data['essences']:
		essence_dir = os.path.join(essences_dir, os.path.dirname(values['filepath']))
		os.makedirs(essence_dir, exist_ok=True)

		essence_name = os.path.splitext(os.path.basename(essence_dir))[0]
		eprimes_dir = essence_name + "-" + values['mode']
		values['name'] = essence_name

		results_essence = os.path.join(essences_dir, values['filepath'])

		file_util.copy_file(values['filepath'], results_essence)
		file_util.copy_file(os.path.join(os.path.dirname(values['filepath']), "params.pcs"),
							os.path.join(essences_dir, essence_name, "params.pcs"))
		dir_util.copy_tree( os.path.join(essence_name, eprimes_dir),
							os.path.join(essences_dir, essence_name, eprimes_dir))

		values['filepath'] = results_essence

	pprint(data)

	# Sort by number of races
	commons_grouped = { k: list(v) for (k, v) in
		groupby(sorted(commons, key=lambda d: d["races"]), key=lambda d: d["races"] )}
	pprint(commons_grouped)


	init_path = os.path.join(place_dir, "results", "init.sh")
	init_source = '. ' + os.path.join(place_dir, "results", "init.sh")

	methods = [markov, smac, uniform, ksample, nsample]

	# Get lines for each method
	results = { f.__name__: f.create_commands(data, commons_grouped, place_dir, init_source, num_runs)
					for f in methods }
	# pprint(results)

	def write_race(essence, races, lines):
		return write_with_header(os.path.join(dir_path, "{}-races-{:03}.sh".format(essence, races)), lines)

	scripts = [init_source]
	for (method, essences) in results.items():
		for (essence, races) in essences.items():
			dir_path = os.path.join(place_dir, "results", method, essence)
			os.makedirs(dir_path, exist_ok=True)

			races_fps = [ write_race(essence, races_no, lines) for (races_no, lines) in races.items() ]
			fp = write_with_header(os.path.join(dir_path, essence + ".sh"), sorted(races_fps))
			scripts.append(fp)

	write_with_header(os.path.join(place_dir, "results", "run_all.sh"), ["#Run from ../instancegen-models"] + sorted(scripts))
	write_with_header(init_path, [bash.record_funcs, "export JAVA_MEMORY="+ data['JAVA_MEMORY'] ])


if __name__ == "__main__":
	parser = argparse.ArgumentParser()
	parser.add_argument("json_settings")
	parser.add_argument("output_dir")
	parser.add_argument("num_runs", type=int)
	args = parser.parse_args()
	run(args.json_settings,
		args.output_dir,
		args.num_runs)





