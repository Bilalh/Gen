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


from lib import domains
import re
import itertools


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
num_models = int(sys.argv[2])
argv = sys.argv[3:]

[eprime, instance_specific] = argv[0:2]
[cutoff_time, cutoff_length] = map(float, argv[2:4])
seed = argv[4]

params_arr = argv[5:]


print("args: eprime:{} instance_specific:{} cutoff_time:{} cutoff_length:{} seed:{} ".format(
	eprime, instance_specific, cutoff_time, cutoff_length, seed))

print("---------------------")

datee = calendar.datetime.datetime.now()
print("<toolchain_wrapper.py> Start", datee.isoformat())
now = str(int(datee.timestamp()))

re_kind = re.compile(r"^(\w+)(%(\w+)%(\d+))?")  # patten to match type of variable
raw_params = [ (re_kind.findall(name[1:]), int(val[1:-1])) for name, val in iter_many(params_arr, len(params_arr), 2) ]
pprint(raw_params)

output_dir = calculate_outputdir()
param_info = domains.gather_param_info(essence, output_dir)

param_values={}
raw_params = sorted(raw_params, key=lambda k: k[0][0][0])
for k, g in itertools.groupby(raw_params, key=lambda k: k[0][0][0]):
	print(k)
	parts = list(g)
	pprint(parts)
	# merge
	if len(parts) == 1:
		kv = [(parts[0][0][0][0], parts[0][1], None, None)]
	else:
		kv=[ (p[0][0][0], p[1], p[0][0][2], p[0][0][3]) for p in parts ]

	pprint(kv)
	param_values[k] = param_info[k].reconstruct_for_smac(kv)
	pprint(param_values[k])




cpu_time_end = time.process_time()
print("cpu_time_end {}".format(cpu_time_end))
raise NotImplementedError("not done yet")


