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
import hashlib

from pathlib import Path
from pprint import pprint
import shutil

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



def create_param_file(params):
    """Create a essence param form (name,values) pairs"""
    essence = ["language Essence 1.3"]
    name = []
    for (k, v) in params:
        essence.append( "letting {} be {}".format(k, v.pretty)  )
        name.append("%s" % v.safe)

    if not name:
        name = ['empty']

    return ("\n".join(essence), "-".join(name))


def write_param(output_dir, contents, param_hash):
    """ Write the param at dirname with param_hash """
    param_path = os.path.join(output_dir, "{}.param".format(param_hash) )
    logger.info("%s\n" % param_path)
    with open(param_path, "w") as f:
        f.write(contents)
    return param_path


def get_number_of_models(dirname):
        return len([f for f in os.listdir(dirname) if fnmatch.fnmatch(f, "*.eprime")])


def vaildate_param_for_essence(givens_essence, param_path, timeout):
    """Return True if the param is vaild"""

    conjure = shutil.which("conjure")
    assert conjure,  "conjure should be in the path"


    cputimeout = os.path.join(os.path.expandvars("${PARAM_GEN_SCRIPTS}/"), "tools", "cputimeout", "cputimeout")
    logger.info("cputimeout %s", cputimeout)
    assert cputimeout, "cputimeout not found"

    timefile = param_path + ".vaild-time"

    def time_taken():
        with open(timefile) as f:
            return float(f.readlines()[-1][3:])

    try:
        code = subprocess.check_call([
            cputimeout,
            "--interval", "1", "-k1", "--write-time", timefile, str(timeout),
            conjure,
            "--mode", "validateSolution",
            "--in-essence", givens_essence,
            "--in-solution", param_path ])
        assert code == 0,  "Failed to run conjure"
        return (True, time_taken())
    except subprocess.CalledProcessError as e:
        assert e.returncode == 1, "Failed to run conjure/ conjure failure"
        return (False, time_taken())



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


def get_results(working_dir, output_dir, param_hash, time_per_model, then, mode):
    """ Get the results of running the toolchain """


    current_env= os.environ.copy()
    current_env["OUT_BASE_DIR"] = output_dir
    current_env["TOTAL_TIMEOUT"] = str(time_per_model)
    current_env["USE_DATE"] = then
    current_env["USE_MODE"] = mode

    sys.stdout.flush()
    sys.stderr.flush()

    subprocess.Popen([wrappers("run_gather.sh"), param_hash, working_dir], env=current_env).communicate()
    conn = sqlite3.connect(os.path.join(output_dir, 'results.db'))
    # conn.row_factory = sqlite3.Row

    sys.stdout.flush()
    sys.stderr.flush()

    err_path=list(Path(output_dir).glob("results*/p-%s.errors" % param_hash))
    if err_path:
        err_path=err_path[0]
        logger.error("ERRORS for %s", param_hash)

        with err_path.open() as f:
            logger.error(f.read())

        logger.error("ERRORS for %s", param_hash)
        raise ParamInvaildExeception("invaild param {}".format(param_hash))

    results = [
        sum(x) for x in zip(*conn.execute(
        """SELECT  1, MinionTimeout, MinionSatisfiable,MinionSolutionsFound, IsOptimum, isDominated
            FROM TimingsDomination
            Where paramHash = ?""",
            (param_hash,)
    ))]
    return results


class ParamInvaildExeception(Exception):
    pass


def save_quality(output_dir, param_name, param_hash, quality):
    conn = sqlite3.connect(os.path.join(output_dir, 'results.db'))
    conn.execute('INSERT OR REPLACE INTO ParamQuality(param, paramHash, quality) Values(?, ?, ?)', (param_name, param_hash, quality))
    conn.commit()


def get_quailty(output_dir, param_hash,):
    logger.info("get_quailty  output_dir %s hash %s", output_dir, param_hash)
    conn = sqlite3.connect(os.path.join(output_dir, 'results.db'))
    results = conn.execute("SELECT quality FROM ParamQuality WHERE paramHash = ?", (param_hash,))
    lresults = list(results)
    assert len(lresults) == 1
    return lresults[0][0]


def quality(count, minionTimeout, minionSatisfiable, minionSolutionsFound, isOptimum, isDominated):
    """
    0.0 perfect  1.0 terrible
    """
    logger.info("quality %s", (count, minionTimeout, minionSatisfiable, minionSolutionsFound, isOptimum, isDominated))

    if minionTimeout == count:
        return 1
    else:
        return 1 - (isDominated / count)


def hash(name):
    "Return a hash of the name"
    as_bytes = name.encode("utf-8")
    # using sha512 since I don't want to deal with collisions maybe a uuid would be better?
    return hashlib.sha512(as_bytes).hexdigest()


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
