#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import calendar
import os
import os.path as path
import sqlite3
import subprocess
import sys
import time
import math
from pprint import pprint

from lib import chain_lib
from lib import domains
import re
import itertools

from pathlib import Path
from lib.option_handing import Info
import json


def iter_many(it, length, num):
	for i in range(0, length, num):
		yield (it[i:i + num])


def calculate_outputdir(*fps):
	return os.path.join(os.path.expandvars("${OUT_BASE_DIR}"), *fps)


cpu_time_start = time.process_time()

#Where we are
prog_name = path.dirname(sys.argv[0])
abs_prog_dir = path.abspath(prog_name)
print(abs_prog_dir)


print("---------------------")

print(sys.argv)

essence = sys.argv[1]
info_path = sys.argv[2]
num_models = int(sys.argv[3])
argv = sys.argv[4:]

[eprime, instance_specific] = argv[0:2]
[cutoff_time, cutoff_length] = map(float, argv[2:4])
seed = argv[4]

params_arr = argv[5:]

with open(info_path) as fp:
		info = Info(**json.load(fp))


print("args: eprime:{} instance_specific:{} cutoff_time:{} cutoff_length:{} seed:{} ".format(
	eprime, instance_specific, cutoff_time, cutoff_length, seed))

print("---------------------")

datee = calendar.datetime.datetime.now()
print("<toolchain_wrapper.py> Start", datee.isoformat())
now = str(int(datee.timestamp()))

output_dir_s = calculate_outputdir()
output_dir = Path(output_dir_s)
working_dir_s = "../prob013-PPP"
mode = "df-no-channelling-better"

params_dir = output_dir / "params"
if not params_dir.exists():
	params_dir.mkdir()


def create_param_values():
	re_kind = re.compile(r"^(\w+)(%(\w+)%(\d+))?")  # patten to match type of variable
	raw_params = [ (re_kind.findall(name[1:]), int(val[1:-1])) for name, val in iter_many(params_arr, len(params_arr), 2) ]
	pprint(raw_params)

	param_info = domains.gather_param_info(essence, output_dir_s)


	param_values={}
	raw_params = sorted(raw_params, key=lambda k: k[0][0][0])
	grouped = {}

	for k, g in itertools.groupby(raw_params, key=lambda k: k[0][0][0]):
		grouped[k] = list(g)

	for k in info.ordering:
		parts = grouped[k]
		pprint(parts)
		# merge
		if len(parts) == 1:
			kv = [(parts[0][0][0][0], parts[0][1], None, None)]
		else:
			kv=[ (p[0][0][0], p[1], p[0][0][2], p[0][0][3]) for p in parts ]

		pprint(kv)
		param_values[k] = param_info[k].reconstruct_for_smac(param_values, kv)
	pprint(param_values[k])

	return param_values


ordering = ""  # no eprime ordering specifed
param_values = create_param_values()

(param_string, param_name) = chain_lib.create_param_essence(sorted(param_values.items()))
param_path = chain_lib.write_param(  str(params_dir), param_string, param_name)
chain_lib.run_models(now, param_path, cutoff_time, working_dir_s, output_dir_s, "df-no-channelling-better", "")

results = chain_lib.get_results(working_dir_s, output_dir_s, param_name, cutoff_time, now, mode)

timefile = (output_dir / ("stats-" + mode) / str(now)).with_suffix(".total_solving_time")
print("timefile %s" % (timefile))
with timefile.open() as f:
	runtime = float(f.readline())


if len(results) != 6:
	quality = 500  # for example SR where statement
	our_quality = 1
	result_kind ="UNSAT"
else:
	our_quality = chain_lib.quality(*results)
	quality = our_quality * 100

	def result_type(count, minionTimeout, minionSatisfiable, minionSolutionsFound, isOptimum, isDominated):
		if count == minionTimeout:
			return "TIMEOUT"
		else:
			return "SAT"

	result_kind = result_type(*results)

chain_lib.save_quality(output_dir_s, param_name, our_quality)



cpu_time_end = time.process_time()
our_cpu_time = cpu_time_end - cpu_time_start
print("smac_process cpu_time {}".format(our_cpu_time))
runtime += our_cpu_time

runlength=0
print("Final Result for ParamILS: {}, {}, {}, {}, {}\n".format(
	result_kind, runtime, runlength, quality, seed))
