#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import json
import os
import itertools
from pprint import pprint
import math
import argparse

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


def markov(data, commons, place_dir):
	cur = data['markov']
	cores = data['cores']

	def func(*, name, filepath, mode, num_models):
		lines = ["### markov ###"]
		lines.append("#-- {} --#".format(filepath))
		for common in commons:
			lines.append("   # {}h, {} races".format(common['total_time'] / 60 / 60, common['races'] )  )

			settings = {
				"essence": filepath,
				"essence_dir": os.path.dirname(filepath),
				"model_timeout": math.ceil(common['total_time'] / common['races'] / cores),
				"limit": math.ceil(common['total_time'] / data['cores']),
				"mode": mode,
				"output_dir": os.path.join(place_dir, "results", "markov", name, "out"),
				"log_path": os.path.join(place_dir, "results", "markov", name, "out", "logs", "log")
			}
			settings.update(cur)
			# print(settings)
			command ="""
			record_cp {log_path} ../instancegen/mchain/chain_main.py time {limit}\
				--mode={mode} --radius_as_percentage \
				--select_radius={select_radius} --influence_radius={influence_radius} \
				--chain_length={chain_length} \
				--model_timeout={model_timeout}\
				--essence={essence} --working_dir={essence_dir} --output_dir={output_dir}
			""".format(**settings).strip().replace("\t", " ")

			lines.append(command)

		return lines

	return { kv['name']: func(**kv) for kv in data['essences'] }


def write_with_header(fp, lines, **data):
	header =[
		"#!/bin/bash",
		"# Assumes python3 is on the $PATH ",
		"# and docopt is installed",
		"export PARAM_GEN_SCRIPTS=`pwd`/../instancegen/scripts/",
		"export NUM_JOBS={}".format(data['cores']),
		""
	]

	with open(fp, "w") as f:
		f.write("\n".join(header))
		f.write("\n".join(lines))
	print("Wrote", fp)


def run(fp, place_dir):
	data = read_json(fp)
	commons = common_product(data)
	pprint(commons)

	results = { f.__name__: f(data, commons, place_dir) for f in [markov] }
	# pprint(results)

	for (method, essences) in results.items():
		for (essence, lines) in essences.items():
			dir_path = os.path.join(place_dir, "results", method, essence)
			os.makedirs(dir_path, exist_ok=True)
			write_with_header(os.path.join(dir_path, essence + ".sh"), lines, **data)


if __name__ == "__main__":
	parser = argparse.ArgumentParser()
	parser.add_argument("json_settings")
	parser.add_argument("output_dir")
	args = parser.parse_args()
	run(os.path.abspath(os.path.expanduser(args.json_settings)),
		os.path.abspath(os.path.expanduser(args.output_dir)) )





