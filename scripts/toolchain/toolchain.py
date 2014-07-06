#!/usr/bin/env python3
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

from pathlib import Path
from pprint import pprint
from textwrap import indent

import args
import run
from run import Status
from commands import ParamRefine, SR, UP, Vaildate
import commands

logger = logging.getLogger(__name__)
logging.basicConfig(format='%(lineno)d:%(funcName)s: %(message)s',
        level=logging.INFO)

op = args.do_args()


def run_commands(eprime):
    
    essence          = op.essence
    essence_param    = op.param
    eprime           = op.outdir / "0001-{}.eprime".format(op.param.stem)
    eprime_param     = eprime.with_suffix(".eprime-param")
    eprime_solution  = eprime.with_suffix(".eprime-solution")
    eprime_info      = eprime.with_suffix(".eprime.info")
    essence_solution = eprime.with_suffix(".solution")
    minion           = eprime.with_suffix(".minion")
    
    out_json = eprime.with_name("result.json")
    out_log  = eprime.with_name("result.output")
    
    cmds = [ParamRefine, SR, UP, Vaildate]
    results=[]
    outputs=[]
    total_cpu_time=0
    total_real_time=0
    all_finished=True
    erroed = None
    last_status=Status.success
    
    limit = op.timeout
    
    for (i,cmd) in enumerate(cmds):  
        itimeout=int(math.ceil(limit))
        mstimeout=itimeout*1000
        
        c=shlex.split(cmd.format(**locals()))
        logger.warn("running %s", c)      
        (res, output) = run.run_with_timeout(limit, c)

        dres = res.__dict__
        results.append(dres)
        
        otimeout = limit
        limit -= res.cpu_time
        total_cpu_time += res.cpu_time
        total_real_time += res.real_time

        outputs.append(" ".join(c))
        outputs.append(output)

        if res.status_ == Status.timeout:
            logger.warn("###TIMEDOUT(%0.2f) for cmd \n%s\n%s", otimeout,
                    " ".join(c), indent(output," \t") )
            all_finished=False
            break
        elif res.status_ != Status.success:
            logger.warn("###ERROR %s for cmd \n%s\n%s",
                    res.status_, " ".join(c), indent(output," \t") )
            erroed=i
            all_finished=False
            last_status = res.status_
            break
        elif limit <= 0:
            logger.warn("### NO_TIME_LEFT after cmd %s", c)
            break



(essence_refine,er_cpu_time) = run.run_refine_essence(op=op)
pprint(essence_refine)
# pool = Pool()
# results = pool.map(run_commands, [1])
# results = run_commands("")

raise
# with out_json.open("w") as f:
#     d= dict(results=results,
#             total_cpu_time=total_cpu_time,
#             total_real_time=total_real_time,
#             all_finished=all_finished,
#             erroed=erroed,
#             last_status=last_status,
#             given_time=int(args.timeout),
#             result_dir=outdir)
#
#     def first_upper(enum):
#         s = enum.name
#         return s[0].upper() + s[1:] + "_"
#
#     f.write(json.dumps(d, indent=True,sort_keys=True,default=first_upper ))
#
# with out_log.open("w") as f:
#     f.write("\n".join(outputs))
#
# logger.info("\033[1;31mtotal_cpu_time:%0.2f  total_real_time:%0.2f\033[1;0m",
#         total_cpu_time, total_real_time)
#
# sys.exit(last_status.value)
#
