#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Runs the whole toolchain (compact + SR + Minion)
"""
import argparse
import json
import logging
import math
import os
import shlex
import shutil
import subprocess
import sys

from signal import alarm, signal, SIGALRM, SIGKILL
from pprint import pprint
from pathlib import Path
from collections import namedtuple, OrderedDict
from textwrap import indent
from datetime import datetime

logger = logging.getLogger(__name__)
logging.basicConfig(format='%(lineno)d:%(funcName)s: %(message)s',
        level=logging.INFO)

parse_args = argparse.ArgumentParser()
parse_args.add_argument("essence", help='')
parse_args.add_argument("--outdir", required=True, help='')
parse_args.add_argument("--timeout", required=True,type=int, help='CPU time for everything')
parse_args.add_argument("--param", help='', default=None)
args = parse_args.parse_args()

outdir = Path(args.outdir)
if not outdir.exists():
    outdir.mkdir(parents=True)
if not args.param:
    essence_param = outdir.with_name("empty.param")
    with open(str(essence_param),"w") as f:
        f.write("language ESSENCE' 1.0")
        f.write("\n$This Fine is empty")

else:
    essence_param = args.param

essence          = Path(args.essence)
eprime           = outdir / "0001-{}.eprime".format(essence_param.stem)
eprime_param     = eprime.with_suffix(".eprime-param")
eprime_solution  = eprime.with_suffix(".eprime-solution")
eprime_info      = eprime.with_suffix(".eprime.info")
essence_solution = eprime.with_suffix(".solution")
minion           = eprime.with_suffix(".minion")


out_json = eprime.with_name("result.json")
out_log = eprime.with_name("result.output")
timeout = args.timeout

Results = namedtuple("results", "code cpu_time real_time timeout finished cmd")
def run_with_timeout(timeout, cmd):
    code = 0
    finished = True
    py_timeout=False
    try:
        date_start = datetime.utcnow()
        start_usr = os.times().children_user
        start_sys = os.times().children_system
        output = subprocess.check_output(cmd, timeout=timeout,
                stderr=subprocess.STDOUT, universal_newlines=True)
    except subprocess.TimeoutExpired as e:
        output = e.output
        finished = False
        py_timeout=True
    except subprocess.CalledProcessError as e:
        output = e.output
        code = e.returncode
        finished = False
    end_usr = os.times().children_user
    end_sys = os.times().children_system
    date_end=datetime.utcnow()

    diff = date_end - date_start
    cputime_taken = (end_usr - start_usr) + (end_sys - start_sys)

    if "savilerow" in c:
        with eprime_info.open() as f:
            (m_timeout,m_total,sr_real)  = [float(l.split(":")[1]) for l in f.readlines()
                if l.split(":")[0] in {"MinionTimeOut","MinionTotalTime","SavileRowTotalTime" }]
            if int(m_timeout) == 1:
                #because some killed processes don't return cputime
                logger.warn("Adding %2.0f to cputime_taken(%2.0f) because of cpulimit timeout",
                        m_total, cputime_taken)
                if cputime_taken == 0: #Best we can do at this point
                    cputime_taken +=  sr_real
                cputime_taken+=m_total


    logger.info("Took %0.2f (%0.2f real), reported user %0.2f sys %0.2f for\n\t%s",
            cputime_taken, diff.total_seconds(),
            (end_usr - start_usr), (end_sys - start_sys), " ".join(cmd)  )

    return (Results(code=code,
                  cpu_time=cputime_taken, real_time= diff.total_seconds(),
                  timeout=timeout, finished=finished,cmd=cmd)
           ,output)


Conjure = """
time conjure --mode compact  --in-essence "{essence}" --out-eprime "{eprime}"
"""
ParamRefine="""
conjure
    --mode       refineParam
    --in-essence       {essence}
    --in-eprime        {eprime}
    --in-essence-param {essence_param}
    --out-eprime-param {eprime_param}
"""

SR ="""
time savilerow  -mode Normal
-in-eprime                   {eprime}
-in-param                    {eprime_param}
-out-minion                  {minion}
-out-solution                {eprime_solution}
-out-info                    {eprime_info}
-run-solver
-solver-options '-cpulimit {itimeout}'
"""

UP = """
time conjure
    --mode translateSolution
    --in-essence            {essence}
    --in-eprime             {eprime}
    --in-eprime-solution    {eprime_solution}
    --out-essence-solution  {essence_solution}
    --essence-param         {essence_param}
    --eprime-param          {eprime_param}
"""

Vaildate= """
time conjure --mode validateSolution
        --in-essence  {essence}
        --in-solution {essence_solution}
"""

cmds = [Conjure, ParamRefine, SR, UP, Vaildate]
results=[]
outputs=[]
total_cpu_time=0
total_real_time=0
all_finished=True
erroed = None
for (i,cmd) in enumerate(cmds):
    itimeout=int(math.ceil(timeout))
    c=shlex.split(cmd.format(**locals()))
    (res, output) = run_with_timeout(timeout, c)

    dres = res.__dict__
    results.append(dres)
    otimeout = timeout
    timeout = timeout - res.cpu_time
    total_cpu_time += res.cpu_time
    total_real_time += res.real_time

    outputs.append(" ".join(c))
    outputs.append(output)
    if res.code != 0:
        logger.warn("###ERRORS for cmd %s\n%s", c, indent(output," \t") )
        erroed=i
        all_finished=False
        break
    elif not res.finished:
        logger.warn("###TIMEDOUT(%s) for cmd %s\n%s",otimeout, c, indent(output," \t") )
        all_finished=False
        break
    elif timeout <= 0:
        logger.warn("### NO_TIME_LEFT after cmd %s", c)
        break


with out_json.open("w") as f:
    d= dict(results=results,
            total_cpu_time=total_cpu_time,
            total_real_time=total_real_time,
            all_finished=all_finished,
            erroed=erroed,
            given_time=int(args.timeout))
    f.write(json.dumps(d, indent=True,sort_keys=True))

with out_log.open("w") as f:
    f.write("\n".join(outputs))

logger.info("total_cpu_time:%0.2f  total_real_time:%0.2f", total_cpu_time, total_real_time)


if erroed:
    sys.exit(1)
elif not all_finished:
    sys.exit(2)
else:
    sys.exit(0)
