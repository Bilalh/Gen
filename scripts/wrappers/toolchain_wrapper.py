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


def iter_many(it, length, num):
	for i in range(0, length, num):
		yield (it[i:i + num])


#Where we are
prog_name = path.dirname(sys.argv[0])
abs_prog_dir = path.abspath(prog_name)
print(abs_prog_dir)


print("---------------------")

print(sys.argv)

num_models = int(sys.argv[1])
argv = sys.argv[2:]

[eprime, instance_specific] = argv[0:2]
[cutoff_time, cutoff_length] = map(float, argv[2:4])
seed = argv[4]

params_arr = argv[5:]


print("args: eprime:{} instance_specific:{} cutoff_time:{} cutoff_length:{} seed:{} ".format(eprime, instance_specific, cutoff_time, cutoff_length, seed))

print(cutoff_time)
print(params_arr)

params = [ (name[1:], int(val[1:-1])) for name, val in iter_many(params_arr, len(params_arr), 2) ]
print(params)

print("---------------------")

datee = calendar.datetime.datetime.now()
print("<toolchain_wrapper.py> Start", datee.isoformat())
now = str(int(datee.timestamp()))


def create_param_essence(params):
	"""Create  a essence param form (name,values) pairs """
	essence = ["language Essence 1.3"]
	name = []
	for (k, v) in params:
		essence.append( "letting {} be {}".format(k, v)  )
		name.append("%03d" % v)
	return ("\n".join(essence), "-".join(name))


def outputdir(*fps):
	return os.path.join(os.path.expandvars("${OUT_BASE_DIR}"), *fps)


(param_string, param_name)=create_param_essence(params)

print(param_string)
os.makedirs(outputdir("params"), exist_ok=True)

param_path= outputdir("params/{}.param".format(param_name))
with open(param_path, "w") as f:
	f.write(param_string)

res = "SAT"
runlength=0


cutoff_time = int(math.ceil(cutoff_time / num_models) )

cutoff_time_str = str(cutoff_time)

print("###Running SR/Minion###")


def wrappers(script_name):
	return os.path.join(os.path.expandvars("${PARAM_GEN_SCRIPTS}"), "wrappers", script_name)


def timeme(method):
	""" @timeme annotation which returns the time taken in ms as well as the result"""
	def wrapper(*args, **kw):
		startTime = int(round(time.time() * 1000))
		result = method(*args, **kw)
		endTime = int(round(time.time() * 1000))

		return (endTime - startTime, result)

	return wrapper


def runner():
	subprocess.Popen([
		wrappers("run.sh"), str(now), param_path, cutoff_time_str
	]).communicate()

runner()


gather_env= os.environ.copy()
gather_env["TOTAL_TIMEOUT"] = cutoff_time_str
gather_env["USE_DATE"] = now

subprocess.Popen([wrappers("run_gather.sh"), param_name], env=gather_env).communicate()

#TODO support modes other then df
timefile = outputdir("stats-" + "df", str(now) + ".total_solving_time")
print("timefile %s" % (timefile))
with open(timefile) as f:
	runtime = float(f.readline())


conn = sqlite3.connect(outputdir('results.db'))
# conn.row_factory = sqlite3.Row


results = [
	sum(x) for x in zip(*conn.execute(
	"""SELECT  1, MinionTimeout, MinionSatisfiable,MinionSolutionsFound, IsOptimum, isDominated
		FROM TimingsDomination
		Where param = ?""",
		(param_name,)
))]

# pp(results)


def quality_(count, minionTimeout, minionSatisfiable, minionSolutionsFound, isOptimum, isDominated):
	"""
	0 perfect  100 terrible
	"""
	if minionTimeout == 0:
		return 100
	elif minionTimeout == count:
		res="TIMEOUT"
		return 100
	else:
		return (1.0 - (minionTimeout / count)) * 100

if len(results) != 6:
	quality = 500  # The SR Error e.g where statements
else:
	quality = quality_(*results)

print("<toolchain_wrapper.py> End", calendar.datetime.datetime.now().isoformat())


print("                         : {}, {}, {}, {}, {}".format(
	"res", "runtime", "runlength", "quality", "seed"))

print("Final Result for ParamILS: {}, {}, {}, {}, {}\n".format(
	res, runtime, runlength, quality, seed))

