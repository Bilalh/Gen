#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Runs the whole toolchain (compact + rnd models >> SR + Minion)
"""
import hashlib
import json
import logging
import os
import random
import sys
import time

from collections import defaultdict
from enum import Enum
from functools import partial
from multiprocessing import Pool
from pathlib import Path
from pprint import pformat
from shutil import which

from toolchain import args
from toolchain import command
from toolchain import run
from toolchain.command import K
from toolchain.run import Status

logger = logging.getLogger(__name__)

SKIP_CODE = 73


def sha1_file(filepath):
    """ Return the sha1 of a Path """
    with filepath.open('rb') as f:
        return hashlib.sha1(f.read()).hexdigest()


def obj_to_json(obj):
    if isinstance(obj, Enum):
        s = obj.name
        return s[0].upper() + s[1:] + "_"
    else:
        return str(obj)


def with_settings(results, *, op, time_taken, successful, consistent):
    return dict(
        data_=results,
        essence_=op.essence,
        outdir_=op.outdir,
        given_time_=op.timeout,
        time_taken_=time_taken,
        successful_=successful,
        consistent_=consistent)


def remove_solve_dups(solve_results, outdir):
    exts = ['.eprime.logs', '.eprime.error', '.eprimes-solution', '.solution', '.output',
            '.eprime-param', '.refine-output', '.eprime', '.choices.json']

    error_hashes = defaultdict(set)

    def delete(ep, err_hash):
        logger.warn("Removing %s it has the same error as %s ", ep.stem, err_hash)
        for ext in exts:
            if ep.with_suffix(ext).exists():
                ep.with_suffix(ext).unlink()

    results_unique = []

    for (eprime_name, dic) in solve_results.items():
        if dic['last_kind'] in [K.savilerow, K.translateUp, K.validate]:
            if dic['erroed'] is None:
                delete((outdir / eprime_name), "<passing>")
                continue
            solve_output = (outdir / eprime_name).with_suffix(".output")
            err_hash = run.hash_path(solve_output)
            if err_hash in error_hashes[dic['last_kind']]:
                delete((outdir / eprime_name), err_hash)
                continue

            error_hashes[dic['last_kind']].add(err_hash)
            logger.info("hash %s is %s", err_hash, eprime_name)

        results_unique.append((eprime_name, dic))

    return dict(results_unique)


if __name__ == "__main__":

    op = args.do_args()

    def setup_logging(outdir):
        p = outdir / "_toolchain.logs"

        rootLogger = logging.getLogger()
        rootLogger.setLevel(logging.INFO)

        consoleHandler = logging.StreamHandler()
        consoleHandler.setLevel(logging.WARNING)
        logFormatter = logging.Formatter(
            '%(name)s:%(lineno)d:%(funcName)s:\n    %(message)s')
        consoleHandler.setFormatter(logFormatter)
        logging.getLogger().addHandler(consoleHandler)

        fileHandler = logging.FileHandler(str(p))
        fileHandler.setLevel(logging.INFO)
        logFormatter = logging.Formatter(
            '%(asctime)s⦙%(levelname)-10s⦙%(name)-10s⦙%(lineno)-4d⦙%(funcName)-50s⦙ %(message)s')
        fileHandler.setFormatter(logFormatter)
        logging.getLogger().addHandler(fileHandler)

    setup_logging(op.outdir)

    logger.warn("args %s", op)
    logger.info("@@sha1_spec.essence:%s", sha1_file(op.essence))

    here_dir = Path(__file__).parent

    if op.bin_dir:
        extra_env = dict(PATH=str(here_dir) + ":" + op.bin_dir + ":" + os.environ['PATH'])
    else:
        extra_env = dict(PATH=str(here_dir) + ":" + os.environ['PATH'])

    if 'LD_LIBRARY_PATH' in os.environ:
        logging.warn("LD_LIBRARY_PATH=%s", os.environ['LD_LIBRARY_PATH'])
    else:
        logging.warn("LD_LIBRARY_PATH unset")

    logger.warn("new PATH=%s", extra_env['PATH'])

    random.seed(op.seed)

    if op.new_conjure:
        if not which('conjureNew'):
            logger.error('ERROR: conjureNew not in $PATH')
            sys.exit(5)

        if "CONJURE_LOG_FOLLOW_AS_COMPACT" in os.environ:
            commands = command.ConjureNewLogFollowCompact()
        elif "CONJURE_LOG_FOLLOW_AS_FIRST" in os.environ:
            commands = command.ConjureNewLogFollowFirst()
        else:
            commands = command.ConjureNew()

    else:
        commands = command.ConjureOld()

    def get_refine_time(fp, arg):
        with (fp).open() as f:
            data = json.load(f)['data_']
            if "choices_made" in data:
                return data['cmd_used']['cpu_time']
            else:
                refine_times = {k: vs['cpu_time'] for (k, vs) in data.items()}
                if arg not in refine_times:
                    logger.error('{} not in {}'.format(arg, fp))
                    sys.exit(9)
                return refine_times[arg]

    def get_solve_time(fp, arg):
        with (fp).open() as f:
            solve_times = {
                k: vs['total_cpu_time']
                for (k, vs) in json.load(f)['data_'].items()
            }
            if arg not in solve_times:
                if arg == 'follow':
                    return max(solve_times.values())
                else:
                    logger.error('%s not in %s', arg, fp)
                    sys.exit(10)
            return solve_times[arg]

    if op.reduce_time:
        refine = op.essence.with_name('refine_essence.json')
        solve = op.essence.with_name('solve_eprime.json')

        new_timeout = None
        if solve.exists():
            if not refine.exists():
                logger.error("ERROR: solve_eprime exists but refine_essence is missing")
                sys.exit(8)
            else:
                refine_time = get_refine_time(refine, op.reduce_time)
                solve_time = get_solve_time(solve, op.reduce_time)
                new_timeout = refine_time + solve_time

        elif refine.exists():
            new_timeout = get_refine_time(refine, op.reduce_time)

        if new_timeout:
            if new_timeout > op.timeout:
                logger.warning("Timeout previously used %s, is greater then the given time %s",
                               new_timeout, op.timeout)

                if op.exit_if_not_enough_time:
                    sys.exit(SKIP_CODE)

            old = op.timeout
            op.timeout = int(min(max(new_timeout * 5, 30), op.timeout))

            logger.warn("Timeout changed from %s to %s", old, op.timeout)

    startTime = time.time()

    # Make the eprimes
    if op.choices:
        if not Path(op.choices).exists():
            logger.error("ERROR: --choices does not exist {}".format(op.choices))
            sys.exit(4)

        (essence_refine, refine_wall_time) = run.run_refine_essence_with_choices(
            op=op,
            commands=commands,
            extra_env=extra_env)
        successful = essence_refine['cmd_used']['status_'] in [Status.success,
                                                               Status.timeout]
    elif op.refine_all:
        (essence_refine, refine_wall_time) = run.run_refine_all_essence(
            op=op,
            commands=commands,
            extra_env=extra_env)
        successful = essence_refine['cmd_used']['status_'] in [Status.success,
                                                               Status.timeout]
    else:
        (essence_refine, refine_wall_time) = run.run_refine_essence(
            op=op,
            commands=commands,
            random=op.num_cores - 1,
            cores=op.num_cores,
            extra_env=extra_env)
        successful = all(res['status_'] in [Status.success, Status.timeout]
                         for res in essence_refine.values())

    endTime = time.time()
    logger.info("essence_refine: %s", pformat(essence_refine))

    settings = with_settings(essence_refine,
                             op=op,
                             time_taken=refine_wall_time,
                             successful=successful,
                             consistent=True)
    with (op.outdir / "refine_essence.json").open("w") as f:
        f.write(json.dumps(settings, indent=True, sort_keys=True, default=obj_to_json))

    if not successful:
        sys.exit(3)

    if op.refine_only:
        sys.exit(0)

    eprimes = list(op.outdir.glob('*.eprime'))

    if not eprimes:
        logger.warn("No eprimes produced exiting..")
        sys.exit(6)
    else:
        logger.warn("running %s", eprimes)

    # Use the sum of all wall time used by all processorss
    # limit = op.timeout - refine_wall_time

    # use clock time
    limit = op.timeout - (endTime - startTime)

    if limit <= 0:
        logger.warn("No time left after refine")
        sys.exit(0)

    # Run the SR Minion translate and vaildate
    solve_op = partial(run.run_solve, extra_env, op, commands, limit)

    pool = Pool(op.num_cores)
    solve_results = dict(pool.map(solve_op, eprimes))
    logger.info("solve_results: %s", pformat(solve_results))

    solve_wall_time = sum(res['total_real_time'] for res in solve_results.values())

    successful = all(res['erroed'] is None for res in solve_results.values())

    if not successful:
        solve_results = remove_solve_dups(solve_results, op.outdir)

    # checks that all eprimes that were solved either have a solution
    # or don't have a solution
    def is_consistent():
        fin_names = [k for (k, v) in solve_results.items()
                     if not v['erroed'] and v['solving_finished']]
        sol_exist = [(op.outdir / name).with_suffix(".solution").exists()
                     for name in fin_names]
        return all(sol_exist) or all([not b for b in sol_exist])

    settings = with_settings(solve_results,
                             op=op,
                             time_taken=solve_wall_time,
                             successful=successful,
                             consistent=is_consistent())

    with (op.outdir / "solve_eprime.json").open("w") as f:
        f.write(json.dumps(settings, indent=True, sort_keys=True, default=obj_to_json))

# logger.info("\033[1;31mtotal_cpu_time:%0.2f  total_real_time:%0.2f\033[1;0m",
#         total_cpu_time, total_real_time)

    if not successful:
        sys.exit(3)

    logger.warn("completed")
    sys.exit(0)
