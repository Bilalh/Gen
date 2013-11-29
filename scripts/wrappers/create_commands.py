#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import json
import os
import itertools
from pprint import pprint
import math

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


def markov(data, commons):
	cur = data['markov']
	cores = data['cores']

	lines = ["### markov ###"]

	for essence in data['essences']:
		lines.append("#-- {} --#".format(essence))
		for common in commons:
			lines.append("   # {}h, {} races".format(common['total_time'] / 60 / 60, common['races'] )  )

			settings = {
				"essence": essence,
				"essence_dir": os.path.dirname(essence),
				"model_timeout": math.ceil(common['total_time'] / common['races'] / cores),
				"limit": math.ceil(common['total_time'] / data['cores'])
			}
			settings.update(cur)
			print(settings)
			command ="""
			record logs/runs ../instancegen/mchain/chain_main.py time {limit}\
				--radius_as_percentage \
				--chain_length={chain_length} \
				--select_radius={select_radius} --influence_radius={influence_radius} \
				--model_timeout={model_timeout} \
				--essence={essence} --working_dir={essence_dir}
			""".format(**settings).strip().replace("\t", " ")

			lines.append(command)

	return lines



def run(fp):
	data = read_json(fp)
	common = common_product(data)
	pprint(common)
	lines = markov(data, common)

	header =[
		"#!/bin/bash",
		"export PARAM_GEN_SCRIPTS=`pwd`/../instancegen/scripts/",
		"export NUM_JOBS={}".format(data['cores']),
		""
	]

	with open("markov.sh", "w") as f:
		f.write("\n".join(header))
		f.write("\n".join(lines))



run("/Users/bilalh/CS/instancegen/settings.json")