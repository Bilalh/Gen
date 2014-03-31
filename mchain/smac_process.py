#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from lib import chain_lib
from lib import domains
from lib import instances
from lib.option_handing import Info

from pathlib import Path
from pprint import pprint, pformat

import calendar
import itertools
import json
import math
import os
import os.path as path
import re
import sys
import time
import logging

cpu_time_start = time.process_time()
runtime = 0
logger = logging.getLogger(__name__)
logging.basicConfig(format='%(name)s:%(lineno)d:%(funcName)s: %(message)s', level=logging.INFO)


def iter_many(it, length, num):
	for i in range(0, length, num):
		yield (it[i:i + num])


def calculate_outputdir(*fps):
	return os.path.join(os.path.expandvars("${OUT_BASE_DIR}"), *fps)



#Where we are
prog_name = path.dirname(sys.argv[0])
abs_prog_dir = path.abspath(prog_name)
logger.info(abs_prog_dir)


logger.info("---------------------")

logger.info("export PARAM_GEN_SCRIPTS=%s", os.path.expandvars("${PARAM_GEN_SCRIPTS}"))
logger.info("export OUT_BASE_DIR=%s", os.path.expandvars("${OUT_BASE_DIR}"))
logger.info(str(sys.argv).replace(',', ' ').replace('[', ' ').replace(']', ' ') )

essence = sys.argv[1]
info_path = sys.argv[2]
num_models = int(sys.argv[3])
mode = sys.argv[4]
argv = sys.argv[5:]

[eprime, instance_specific] = argv[0:2]
[cutoff_time, cutoff_length] = map(float, argv[2:4])

per_model_time = int(math.ceil(cutoff_time / num_models ))

seed = argv[4]

params_arr = argv[5:]

with open(info_path) as fp:
		info = Info(**json.load(fp))


logger.info("args: eprime:{} instance_specific:{} cutoff_time:{} cutoff_length:{} seed:{} ".format(
	eprime, instance_specific, cutoff_time, cutoff_length, seed))

logger.info("---------------------")

datee = calendar.datetime.datetime.now()
logger.info("<toolchain_wrapper.py> Start %s", datee.isoformat())
now = str(int(datee.timestamp()))

output_dir_s = calculate_outputdir()
output_dir = Path(output_dir_s)


# we are in the essence directory but os.curdir is .
# we need the essence's directory in the path
# so we hack around is this by doing
# ../<essence_dir>
working_dir_s = "../" + Path(essence).stem
logger.info(working_dir_s)


params_dir = output_dir / "params"
params_dir_tmp_dir = params_dir / "tmp"
if not params_dir.exists():
	params_dir.mkdir()

if not params_dir_tmp_dir.exists():
	params_dir_tmp_dir.mkdir()


def invaild_param(cpu_time_start, runtime):
	""" output when the param is invaild """
	result_kind ="SAT"
	runlength=0
	quality = 500  # invaild

	cpu_time_end = time.process_time()
	our_cpu_time = cpu_time_end - cpu_time_start
	logger.info("smac_process cpu_time {}".format(our_cpu_time))
	runtime += our_cpu_time

	logger.info("Returning Final Result for ParamILS: {}, {}, {}, {}, {}\n".format(
		result_kind, runtime, runlength, quality, seed))
	print("Final Result for ParamILS: {}, {}, {}, {}, {}\n".format(
		result_kind, runtime, runlength, quality, seed))
	sys.exit(0)


def create_param_values():
	re_kind = re.compile(r"^(\w+)(%(\w+)%(\d+))?")  # patten to match type of variable
	raw_params = [ (re_kind.findall(name[1:]), int(val[1:-1])) for name, val in iter_many(params_arr, len(params_arr), 2) ]
	logger.info(pformat(raw_params))

	domains.setup_domain(False)
	instances.setup_instances(False)
	param_info = domains.gather_param_info(essence, output_dir_s)


	param_values={}
	raw_params = sorted(raw_params, key=lambda k: k[0][0][0])
	grouped = {}

	for k, g in itertools.groupby(raw_params, key=lambda k: k[0][0][0]):
		grouped[k] = list(g)

	for k in info.ordering:
		parts = grouped[k]
		logger.info(pformat(parts))
		# merge
		if len(parts) == 1:
			kv = [(parts[0][0][0][0], parts[0][1], None, None)]
		else:
			kv=[ (p[0][0][0], p[1], p[0][0][2], p[0][0][3]) for p in parts ]

		logger.info(pformat(kv))
		param_values[k] = param_info[k].reconstruct_for_smac(param_values, kv)
	logger.info(pformat(param_values[k]))

	return param_values


ordering = ""  # no eprime ordering specifed

try:
	param_values = create_param_values()
except domains.InvaildValueException as e:
	logger.info("Invaild param")
	invaild_param(cpu_time_start, runtime)


(param_string, param_name) = chain_lib.create_param_file(sorted(param_values.items()))
param_hash = chain_lib.hash(param_name)

param_path = chain_lib.write_param(  str(params_dir_tmp_dir), param_string, param_hash)

(vaild, vaild_time) = chain_lib.vaildate_param_for_essence(essence + ".givens", param_path, cutoff_time)
logger.info("vaild:%s, vaildation_time:%s ", vaild, vaild_time)

runtime += vaild_time


if not vaild:
	invaild_param(cpu_time_start, runtime)

path_param = Path(param_path)
new_param_path = params_dir / path_param.name
path_param.rename(new_param_path)
param_path = str(new_param_path)

chain_lib.run_models(now, param_path, per_model_time, working_dir_s, output_dir_s, mode, ordering)

try:
	results = chain_lib.get_results(working_dir_s, output_dir_s, param_hash, per_model_time, now, mode)
except chain_lib.ParamInvaildExeception as e:
	logger.error("Failed to get results %s", e)
	results = []

timefile = (output_dir / ("stats-" + mode) / str(now)).with_suffix(".total_solving_time")
logger.info("timefile %s" % (timefile))
with timefile.open() as f:
	runtime += float(f.readline())

logger.info("new_param_path %s\n", new_param_path)

if len(results) != 6:
	logger.error("Failed to get results %s", new_param_path.stem)
	quality = 500  # for example SR where statement
	our_quality = 2
	result_kind ="SAT"
else:
	our_quality = chain_lib.quality(*results)
	quality = our_quality * 100

	def result_type(count, minionTimeout, minionSatisfiable, minionSolutionsFound, isOptimum, isDominated):
		if count == minionTimeout:
			return "TIMEOUT"
		else:
			return "SAT"

	result_kind = result_type(*results)

#FIXME Seems to give the same result as store the result later
chain_lib.save_quality(output_dir_s, param_name, param_hash, our_quality)



cpu_time_end = time.process_time()
our_cpu_time = cpu_time_end - cpu_time_start
logger.info("smac_process cpu_time {}".format(our_cpu_time))
runtime += our_cpu_time

runlength=0
logger.info("Returning Final Result for ParamILS: {}, {}, {}, {}, {}\n".format(
		result_kind, runtime, runlength, quality, seed))
print("Final Result for ParamILS: {}, {}, {}, {}, {}\n".format(
	result_kind, runtime, runlength, quality, seed))

