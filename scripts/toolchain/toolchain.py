#!/usr/bin/env python3.4
# -*- coding: utf-8 -*-
"""
Runs the whole toolchain (compact + SR + Minion)
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
from pprint import pprint
from textwrap import indent
from multiprocessing import Pool

import args
import run
import commands

from run import Status
from commands import ParamRefine, SR, UP, Vaildate

logger = logging.getLogger(__name__)
logging.basicConfig(format='%(lineno)d:%(funcName)s: %(message)s',
        level=logging.INFO)

if __name__ == "__main__":
    op = args.do_args()
    
    # Make the eprimes
    (essence_refine,refine_wall_time) = run.run_refine_essence(op=op)
    # (essence_refine,refine_wall_time) = ({}, 4)
    pprint(essence_refine)
    limit = op.timeout - refine_wall_time

    # Run the SR Minion translate and vaildate
    solve_op = partial(run.run_solve, op, limit)
    eprimes = op.outdir.glob('*.eprime')
    
    pool = Pool()
    solve_results = dict(pool.map(solve_op, eprimes))

    def first_upper(enum):
        s = enum.name
        return s[0].upper() + s[1:] + "_"
    with (op.outdir / "refine_essence.json" ).open("w") as f:
        f.write(json.dumps(essence_refine, indent=True,sort_keys=True,default=first_upper ))

    with (op.outdir / "solve_eprime.json" ).open("w") as f:
        f.write(json.dumps(solve_results, 
            indent=True,sort_keys=True,default=first_upper ))

# logger.info("\033[1;31mtotal_cpu_time:%0.2f  total_real_time:%0.2f\033[1;0m",
#         total_cpu_time, total_real_time)
#
# sys.exit(last_status.value)
#
