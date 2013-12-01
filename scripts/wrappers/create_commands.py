#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import json
import os
import itertools
from pprint import pprint
import math
import argparse

from itertools import groupby

from distutils import dir_util
from distutils import file_util


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


def markov(data, commons_grouped, place_dir, init_source, num_runs):
	cur = data['markov']
	cores = data['cores']

	def func(commons, *, name, filepath, mode, num_models):

		if num_models < cores:
			jobs = num_models
		else:
			jobs = cores

		lines = [
			"export NUM_JOBS={}".format(jobs),
			"### markov ###",
			init_source,
			"#-- {} --#".format(filepath),
			"for race_no in {1..%d}; do" % (num_runs)
		]
		for common in commons:
			tu = (int(math.ceil(common['total_time'] / 60 / 60)), common['races'] )
			lines.append("		# {:03}h, {:03} races".format(*tu) )

			extra = "out-{:03}-{:03}__{race_no}".format(*tu, race_no="${race_no}")
			settings = {
				"essence": filepath,
				"essence_dir": os.path.dirname(filepath),
				"model_timeout": math.ceil(math.ceil(common['total_time'] / common['races']) / cores),
				"limit": math.ceil(common['total_time'] / data['cores']),
				"mode": mode,
				"output_dir": os.path.join(place_dir, "results", "markov", name, extra),
				"log_path": os.path.join(place_dir, "results", "markov", name, extra, "logs", "log-${race_no}")
			}
			settings.update(cur)
			# print(settings)
			command ="\t" + """
			record_cp {log_path} ../instancegen/mchain/chain_main.py time {limit}\
				--model_timeout={model_timeout}\
				--mode={mode} --radius_as_percentage \
				--select_radius={select_radius} --influence_radius={influence_radius} \
				--chain_length={chain_length} \
				--essence={essence} --working_dir={essence_dir} --output_dir={output_dir}
			""".format(**settings).strip().replace("\t", " ")

			lines.append(command)

		lines.append("done")
		return lines

	return { kv['name']: {num: func(commons, **kv)
		for (num, commons) in commons_grouped.items()}
		for kv in data['essences']
		}


def write_with_header(fp, lines):
	header =[
		"#!/bin/bash",
		"# Assumes python3 is on the $PATH ",
		"# and docopt is installed (i.e pip install docopt) ",
		"export PARAM_GEN_SCRIPTS=`pwd`/../instancegen/scripts/",
		""
	]

	with open(fp, "w") as f:
		f.write("\n".join(header))
		f.write("\n".join(lines))
	print("Wrote", fp)
	return fp


record_funcs="""
function record(){
	if [[ -z "${1}" || -z "${2}" ]]; then
		echo "$0 name command"
	else
		local name="$1"
		shift
		if [ "`basename \"${name}\"`" != "${name}" ]; then
			mkdir -p "`dirname \"${name}\"`"
		fi
		local fp="${name}-`date +%F_%H-%M_%s`"
		echo "pwd:`pwd`"> "${fp}.cmds"
		echo "$@"  >> "${fp}.cmds"
		$@ 2>&1 | tee "${fp}.log"
	fi
}


function record_cp(){
	if [[ -z "${1}" || -z "${2}" ]]; then
		echo "$0 name command"
	else
		local name="$1"
		shift
		if [ "`basename \"${name}\"`" != "${name}" ]; then
			mkdir -p "`dirname \"${name}\"`"
		fi
		local fp="${name}-`date +%F_%H-%M_%s`"
		echo "pwd:`pwd`"> "${fp}.cmds"

		echo "conjure">> "${fp}.cmds"
		conjure 2>&1 | grep Version						  >> "${fp}.cmds"
		minion 2>&1	 | egrep 'Git version|Minion Version' >> "${fp}.cmds"
		savilerow	 | head -n 2 | tail -n 1			  >> "${fp}.cmds"

		local sr="$(dirname `which savilerow`)"
		if [ -f "${sr}/.hg/cache/branchheads" ]; then
			cat "${sr}/.hg/cache/branchheads" >> "${fp}.cmds"
		fi
		echo "Command:"  >> "${fp}.cmds"
		echo "$@"        >> "${fp}.cmds"
		$@ 2>&1 | tee "${fp}.log"
	fi
}

export -f record
export -f record_cp
"""


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

	# Get lines for each method
	results = { f.__name__: f(data, commons_grouped, place_dir, init_source, num_runs) for f in [markov] }
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

	write_with_header(os.path.join(place_dir, "run_all.sh"), sorted(scripts))
	write_with_header(init_path, [record_funcs])


if __name__ == "__main__":
	parser = argparse.ArgumentParser()
	parser.add_argument("json_settings")
	parser.add_argument("output_dir")
	parser.add_argument("num_runs", type=int)
	args = parser.parse_args()
	run(args.json_settings,
		args.output_dir,
		args.num_runs)





