from collections import namedtuple
import fnmatch
import logging
import math
import os
import random
import sqlite3
import subprocess
import sys
import time

from pprint import pprint


logger = logging.getLogger(__name__)
Settings=namedtuple('Settings', ['chain_length', 'select_radius', 'influence_radius', 'seed', 'mode',
                                'models_timeout', "essence", "working_dir", "output_dir", "limit", "radius_as_percentage"])


def uniform_int(l, u):
    return math.ceil(random.uniform(l - 1, u))
    # import numpy as np
    # return math.ceil(np.random.uniform(l - 1, u))


def wrappers(script_name):
    """ return the complete path to a script in $PARAM_GEN_SCRIPTS/wrappers """
    return os.path.join(os.path.expandvars("${PARAM_GEN_SCRIPTS}/"), "wrappers", script_name)



def create_param_essence(params):
    """Create a essence param form (name,values) pairs"""
    essence = ["language Essence 1.3"]
    name = []
    for (k, v) in params:
        essence.append( "letting {} be {}".format(k, v.pretty)  )
        name.append("%s" % v.safe)
    return ("\n".join(essence), "-".join(name))


def write_param(output_dir, contents, param_name):
    """ Write the param at dirname with param_name """
    param_path = os.path.join(output_dir, "{}.param".format(param_name) )
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


def get_number_of_models(dirname):
        return len([f for f in os.listdir(dirname) if fnmatch.fnmatch(f, "*.eprime")])


def run_models(now, param_path, time_per_model, working_dir, output_dir, mode, model_ordering):
    """ Run the toolchain """

    models_dir = os.path.join(working_dir, os.path.basename(working_dir) + "-" + mode)
    num_models = get_number_of_models( models_dir )
    logger.info(num_models)

    logger.info("time_per_model:%s", time_per_model)

    def runner():
        current_env= os.environ.copy()
        current_env["OUT_BASE_DIR"] = output_dir
        current_env["USE_MODE"] = mode
        current_env["MODELS_TO_USE"]= model_ordering

        subprocess.Popen([
            wrappers("run.sh"), now, param_path, str(time_per_model), working_dir
        ], env=current_env).communicate()

    return runner()


def get_results(working_dir, output_dir, param_name, time_per_model, then, mode):
    """ Get the results of running the toolchain """


    current_env= os.environ.copy()
    current_env["OUT_BASE_DIR"] = output_dir
    current_env["TOTAL_TIMEOUT"] = str(time_per_model)
    current_env["USE_DATE"] = then
    current_env["USE_MODE"] = mode

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

    if minionTimeout == count:
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
