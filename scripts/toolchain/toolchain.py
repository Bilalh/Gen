#!/usr/bin/env python3.4
# -*- coding: utf-8 -*-
"""
Runs the whole toolchain (compact + rnd models >> SR + Minion)
"""
import json
import logging
import math
import os
import shlex
import sys
import itertools


from functools import partial
from pathlib import Path
from pprint import pprint, pformat
from multiprocessing import Pool
from enum import Enum

import args
import run
import commands

from run import Status
from commands import ParamRefine, SR, UP, Vaildate

logger = logging.getLogger(__name__)

def with_settings(results, *, op, time_taken, successful, consistent):
    return dict(
        data_       = results,
        essence_    = op.essence,
        outdir_     = op.outdir,
        given_time_ = op.timeout,
        time_taken_ = time_taken,
        successful_ = successful,
        consistent_ = consistent
    )

if __name__ == "__main__":

    op = args.do_args()
    def setup_logging(outdir):
        p = outdir / "_toolchain.log"

        rootLogger = logging.getLogger()
        rootLogger.setLevel(logging.INFO)

        consoleHandler = logging.StreamHandler()
        consoleHandler.setLevel(logging.WARNING)
        logFormatter = logging.Formatter('%(name)s:%(lineno)d:%(funcName)s:\n    %(message)s')
        consoleHandler.setFormatter(logFormatter)
        logging.getLogger().addHandler(consoleHandler)

        fileHandler = logging.FileHandler(str(p))
        fileHandler.setLevel(logging.INFO)
        logFormatter = logging.Formatter('%(asctime)s⦙%(levelname)-10s⦙%(name)-10s⦙%(lineno)-4d⦙%(funcName)-30s⦙\n    %(message)s')
        fileHandler.setFormatter(logFormatter)
        logging.getLogger().addHandler(fileHandler)


    setup_logging(op.outdir)
    logger.info("info")
    logger.warn("warn")

    def obj_to_json(obj):
        if isinstance(obj, Enum):
            s = obj.name
            return s[0].upper() + s[1:] + "_"
        else:
            return str(obj)


    # Make the eprimes
    (essence_refine,refine_wall_time) = run.run_refine_essence(op=op)
    logger.info("essence_refine: %s", pformat(essence_refine))

    successful =  all(  res['status_'] in [Status.success, Status.timeout]
            for res in essence_refine.values() )
    settings = with_settings(essence_refine,op=op,
            time_taken=refine_wall_time,
            successful=successful, consistent=True)
    with (op.outdir / "refine_essence.json" ).open("w") as f:
        f.write(json.dumps(settings, indent=True,sort_keys=True,default=obj_to_json ))


    if op.refine_only:
        sys.exit(0)

    limit = op.timeout - refine_wall_time
    if limit <=0:
        logger.warn("No time left after refine")
        sys.exit(2)

    # Run the SR Minion translate and vaildate
    solve_op = partial(run.run_solve, op, limit)
    eprimes = op.outdir.glob('*.eprime')

    pool = Pool()
    solve_results = dict(pool.map(solve_op, eprimes))
    logger.info("solve_results: %s", pformat(essence_refine))

    solve_wall_time = sum(  res['total_real_time'] for res in  solve_results.values()  )
    successful = all(  not res['erroed']  for res in  solve_results.values() )


    # checks that all eprimes that were solved either have a solution
    # or don't have a solution
    def is_consistent():
        fin_names = [ k for (k,v) in solve_results.items()
                        if not v['erroed'] and v['solving_finished'] ]
        sol_exist = [ (op.outdir / name).with_suffix(".solution").exists()
                        for name in fin_names ]
        return all(sol_exist) or all( [ not b for b in sol_exist])

    settings = with_settings(solve_results, op=op,
            time_taken=solve_wall_time, successful=successful,consistent=is_consistent())

    with (op.outdir / "solve_eprime.json" ).open("w") as f:
        f.write(json.dumps(settings,
            indent=True,sort_keys=True,default=obj_to_json ))

# logger.info("\033[1;31mtotal_cpu_time:%0.2f  total_real_time:%0.2f\033[1;0m",
#         total_cpu_time, total_real_time)

    logger.warn("completed")
    sys.exit(0)
