#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Runs the whole toolchain (compact + SR + Minion)
"""
import argparse
import json
import logging
import os
import shlex
import shutil
import subprocess
from signal import alarm, signal, SIGALRM, SIGKILL

from pprint import pprint
from pathlib import Path
from collections import namedtuple, OrderedDict
from textwrap import indent
from datetime import datetime

logger = logging.getLogger(__name__)
logging.basicConfig(format='%(name)s:%(lineno)d:%(funcName)s: %(message)s',
        level=logging.INFO)

parser = argparse.ArgumentParser()
parser.add_argument("essence", help='')
parser.add_argument("--out", required=True, help='')
parser.add_argument("--timeout", required=True,type=int, help='CPU time for everything')
parser.add_argument("--param", help='')
args = parser.parse_args()

essence          = Path(args.essence)
eprime           = essence.with_suffix(".eprime")
eprime_solution  = essence.with_suffix(".eprime-solution")
essence_solution = essence.with_suffix(".solution")
minion           = essence.with_suffix(".minion")

out_json = Path(args.out)
out_log = out_json.with_suffix(".output")
timeout = args.timeout

Results = namedtuple("results", "code cpu_time real_time timeout finished cmd")
def run_with_timeout(timeout, cmd):
    code = 0
    finished = True
    try:
        date_start = datetime.utcnow()
        start_usr = os.times().children_user
        start_sys = os.times().children_system
        output = subprocess.check_output(cmd, stderr=subprocess.STDOUT, universal_newlines=True)
    except subprocess.CalledProcessError as e:
        output = e.output
        code = e.returncode
        finished = False
    except subprocess.TimeoutExpired as e:
        output = e.output
        code=139
        finished = False

    end_usr = os.times().children_user
    end_sys = os.times().children_system
    date_end=datetime.utcnow()
    diff = date_end - date_start
    cputime_taken = (end_usr - start_usr) + (end_sys - start_sys)

    logger.info("Took %0.2f (%0.2f real), user %0.2f sys %0.2f for\n\t%s",
            cputime_taken, diff.total_seconds(),
            (end_usr - start_usr), (end_sys - start_sys), " ".join(cmd)  )

    return (Results(code=code,
                  cpu_time=cputime_taken, real_time= diff.total_seconds(),
                  timeout=timeout, finished=finished,cmd=cmd)
           ,output)

def get_process_children(pid):
    p = Popen('ps --no-headers -o pid --ppid %d' % pid,
            shell = True, stdout = PIPE, stderr = PIPE)
    stdout, stderr = p.communicate()
    return (int(p) for p in stdout.split())


Conjure = """
time conjure --mode compact  --in-essence "{essence}" --out-eprime "{eprime}"
"""

SR ="""
time savilerow  -mode Normal
    -in-eprime {eprime}
    -out-minion {minion}
    -out-solution {eprime_solution}
    -run-minion minion
"""

UP = """
time conjure
    --mode translateSolution
    --in-essence            {essence}
    --in-eprime             {eprime}
    --in-eprime-solution    {eprime_solution}
    --out-essence-solution  {essence_solution}
"""

Vaildate= """
time conjure --mode validateSolution
        --in-essence {essence}
        --in-solution {essence_solution}
"""

cmds = [Conjure, SR, UP, Vaildate]
results=[]
outputs=[]
total_cpu_time=0
total_real_time=0
all_finished=True
erroed = None
for (i,cmd) in enumerate(cmds):
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
        break
    elif not res.finished:
        logger.warn("###TIMEDOUT(%s) for cmd %s\n%s",otimeout, c, indent(output," \t") )
        break
    elif timeout <= 0:
        logger.warn("### NO_TIME_LEFT after cmd %s", c)
        break


with out_json.open("w") as f:
    d= dict(results=results,
            total_cpu_time=total_cpu_time,
            total_real_time=total_real_time,
            finished=all_finished,
            erroed=erroed)
    f.write(json.dumps(d, indent=True,sort_keys=True))

with out_log.open("w") as f:
    f.write("\n".join(outputs))
