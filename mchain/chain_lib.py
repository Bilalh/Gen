import time
import os
import sys

from collections import namedtuple
import sqlite3
import subprocess
import json
import fnmatch
import math

import logging
logger = logging.getLogger(__name__)

Settings=namedtuple('Settings', ['chain_length', 'select_radius', 'influence_radius', 'seed', 'mode',
								'model_timeout', "essence", "working_dir", "output_dir", "limit", "radius_as_percentage"])


def wrappers(script_name):
	""" return the complete path to a script in $PARAM_GEN_SCRIPTS/wrappers """
	return os.path.join(os.path.expandvars("${PARAM_GEN_SCRIPTS}/"), "wrappers", script_name)


def gather_param_info(essence_file, output_dir):
	""" Get param bounds in json """

	json_path = os.path.join(output_dir, "essence.json")

	sys.stdout.flush()
	sys.stderr.flush()

	subprocess.Popen([
		wrappers("essenceGivensToJson.sh"), essence_file, json_path, "100"
	]).communicate()


	with open( json_path ) as f:
		json_in = f.read()

	raw_data = json.loads(json_in)

	def transform(data):
		d = data[1][0]
		return (data[0], (d['lower'], d['upper']) )

	param_data = [ transform(data) for data in raw_data ]
	return param_data


def create_param_essence(params):
	"""Create a essence param form (name,values) pairs"""
	essence = ["language Essence 1.3"]
	name = []
	for (k, v) in params:
		essence.append( "letting {} be {}".format(k, v)  )
		name.append("%03d" % v)
	return ("\n".join(essence), "-".join(name))


def write_param(output_dir, contents, param_name):
	""" Write the param at dirname with param_name """
	param_path = os.path.join(output_dir, "params", "{}.param".format(param_name) )
	logger.info("%s\n" % param_path)
	with open(param_path, "w") as f:
		f.write(contents)
	return param_path


def timeme(method):
	""" @timeme annotation which returns the time taken in ms as well as the result"""
	def wrapper(*args, **kw):
		startTime = int(round(time.time() * 1000))
		result = method(*args, **kw)
		endTime = int(round(time.time() * 1000))

		return (endTime - startTime, result)

	return wrapper


def run_models(now, param_path, cutoff_time, working_dir, output_dir):
	""" Run the toolchain """

	def get_number_of_models(dirname):
		return len([f for f in os.listdir(dirname) if fnmatch.fnmatch(f, "*.eprime")])


	models_dir = os.path.join(working_dir, os.path.basename(working_dir) + "-df")
	num_models = get_number_of_models( models_dir )
	logger.info(num_models)

	time_per_model = int(math.ceil(cutoff_time) / num_models)
	logger.info("time_per_model:%s cutoff_time:%s", time_per_model, cutoff_time)

	def runner():
		current_env= os.environ.copy()
		current_env["OUT_BASE_DIR"] = output_dir

		subprocess.Popen([
			wrappers("run.sh"), now, param_path, str(time_per_model), working_dir
		], env=current_env).communicate()

	return runner()


def get_results(working_dir, output_dir, param_name, cutoff_time, then):
	""" Get the results of running the toolchain """

	current_env= os.environ.copy()
	current_env["OUT_BASE_DIR"] = output_dir
	current_env["TOTAL_TIMEOUT"] = str(cutoff_time)
	current_env["USE_DATE"] = then

	sys.stdout.flush()
	sys.stderr.flush()

	subprocess.Popen([wrappers("run_gather.sh"), param_name, working_dir], env=current_env).communicate()
	conn = sqlite3.connect(os.path.join(output_dir, 'results.db'))
	# conn.row_factory = sqlite3.Row

	results = [
		sum(x) for x in zip(*conn.execute(
		"""SELECT  1, MinionTimeout, MinionSatisfiable,MinionSolutionsFound, IsOptimum, isDominated
			FROM TimingsDomination
			Where param = ?""",
			(param_name,)
	))]
	return results


def save_quality(output_dir, param_name, quality):
	conn = sqlite3.connect(os.path.join(output_dir, 'results.db'))
	conn.execute('INSERT OR REPLACE INTO ParamQuality(param, quality) Values(?, ?)', (param_name, quality))
	conn.commit()


def get_quailty(output_dir, param_name,):
	conn = sqlite3.connect(os.path.join(output_dir, 'results.db'))
	results = conn.execute("SELECT quality FROM ParamQuality WHERE param = ?", (param_name,))
	return list(results)[0][0]


def quality(count, minionTimeout, minionSatisfiable, minionSolutionsFound, isOptimum, isDominated):
	"""
	0.0 perfect  1.0 terrible
	"""
	logger.info("quality %s", (count, minionTimeout, minionSatisfiable, minionSolutionsFound, isOptimum, isDominated))

	if minionTimeout == 0:
		return 1
	elif minionTimeout == count:

		return 1
	else:
		return 1 - (isDominated / count)


def copydoc(fromfunc, sep="\n"):
	"""Decorator: Copy the docstring of `fromfunc`"""
	def _decorator(func):
		sourcedoc = fromfunc.__doc__
		if func.__doc__ is None:
			func.__doc__ = sourcedoc
		else:
			func.__doc__ = sep.join([sourcedoc, func.__doc__])
		return func
	return _decorator


def relfect(ranges, point):
	[lowers, uppers] = zip(*ranges)

	def f_upper(upper, v):
		if v > upper:
			return upper + (upper - v)
		else:
			return v

	def f_lower(lower, v):
		if v < lower:
			return lower + (lower - v)
		else:
			return v

	point = [f_lower(lower, (f_upper(upper, v))) for(upper, lower, v) in zip(uppers, lowers, point)]

	return point
