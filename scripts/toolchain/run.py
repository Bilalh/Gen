import logging
import math
import os
import sys
import subprocess
import shlex
import itertools
import hashlib

import commands

from datetime import datetime
from pathlib import Path
from pprint import pprint, pformat

from collections import namedtuple
from enum import Enum,IntEnum, unique
from functools import partial
from multiprocessing import Pool
from textwrap import indent


logger = logging.getLogger(__name__)

def hash_path(path):
    sha = hashlib.sha1()
    with path.open('rb') as f:
        sha.update(f.read())
    return sha.hexdigest()


# global function for run_refine_essence
# because nested function can't be pickled
def run_refine(kwargs,i):
    if i == 0:
        eprime =  kwargs['outdir'] / "comp.eprime"
        c=shlex.split(commands.ConjureCompact.format(
            eprime=eprime, **kwargs))
    else:   
        eprime = kwargs['outdir'] / "{:04}.eprime".format(i)
        c=shlex.split(commands.ConjureRandom.format(eprime=eprime, **kwargs))
        
    logger.warn("running %s", c)
    (res, output) = run_with_timeout(kwargs['itimeout'], c)
    return ((eprime.stem,res.__dict__), " ".join(c) + "\n" + output)

def run_refine_essence(*,op,compact=True,random=3):
    limit = op.timeout
    date_start = datetime.utcnow()

    mapping = dict(essence=op.essence,outdir=op.outdir)
    mapping['itimeout'] = int(math.ceil(limit))

    rr = partial(run_refine,mapping)
    pool = Pool()
    rnds = list(pool.map(rr,range(0,random+1)))

    (results,outputs) =list(zip( *(  rnds ) ))

    with (op.outdir / "_refine.output").open("w") as f:
        f.write("\n".join(outputs))

    date_end=datetime.utcnow()
    diff = date_end - date_start


    results_unique = {}
    for result in results:
        (eprime_name,_) = result
        ep = (op.outdir/ eprime_name).with_suffix(".eprime")
        if not ep.exists():
            results_unique[eprime_name] = result
            continue
        hf = hash_path( ep )
        if hf not in results_unique:
            results_unique[hf] = result
        else:
            logger.warn("Removing eprime %s it a duplicate of %s",
                    eprime_name, results_unique[hf][0] )
            ep.unlink()
            ep.with_suffix(".eprime.logs").unlink()


    return (dict(results_unique.values()), sum( data['real_time']
                for (_,data) in results  ) )


def run_solve(op, limit, eprime):
    essence          = op.essence
    essence_param    = op.param
    eprime_param     = eprime.with_suffix(".eprime-param")
    eprime_solution  = eprime.with_suffix(".eprime-solution")
    eprime_info      = eprime.with_suffix(".eprime.info")
    essence_solution = eprime.with_suffix(".solution")
    minion           = eprime.with_suffix(".minion")

    cmds = [commands.ParamRefine, commands.SR, commands.UP, commands.Vaildate]
    results=[]
    outputs=[]
    total_cpu_time=0
    total_real_time=0
    all_finished=True
    solving_finished=False
    erroed = None
    last_status=Status.success


    for (i,cmd) in enumerate(cmds):

        # always perform solution translation and vaildation
        if cmd in {commands.UP, commands.Vaildate}:
            limit = max([limit, 10])

        if limit <= 0:
            logger.warn("### NO_TIME_LEFT before cmd %s", c)
            all_finished=False
            break

        itimeout=int(math.ceil(limit))
        mstimeout=itimeout*1000

        c=shlex.split(cmd.format(**locals()))
        logger.warn("running %s", c)
        (res, output) = run_with_timeout(limit, c)

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
        elif cmd == commands.UP:
            solving_finished=True

        if cmd == commands.SR and not eprime_solution.exists():
            logger.info("No eprime solution")
            solving_finished=True
            break;

    ret = dict(results=results,
                total_cpu_time=total_cpu_time,
                total_real_time=total_real_time,
                all_finished=all_finished,
                solving_finished=solving_finished,
                erroed=erroed,
                last_status=last_status)

    with eprime.with_suffix(".output").open("w") as f:
        f.write("\n".join(outputs))

    return (eprime.stem,ret)


@unique
class Status(Enum):
    success       = 0,
    errorUnknown  = 1,
    timeout       = 2,
    numberToLarge = 3,
    heapSpace     = 4

errors_not_useful = {Status.numberToLarge}

def classify_error(c,e):
    if "savilerow" in c:
        if "java.lang.NumberFormatException: For input string: " in e.output:
            return Status.numberToLarge
    if "conjure" in c and e.returncode == 252:
        return Status.heapSpace

    return Status.errorUnknown

Results = namedtuple("Results", "rcode cpu_time real_time timeout finished cmd, status_")
def run_with_timeout(timeout, cmd):
    code = 0
    finished = True
    status = Status.success
    try:
        date_start = datetime.utcnow()
        start_usr = os.times().children_user
        start_sys = os.times().children_system
        output = subprocess.check_output(cmd,
                stderr=subprocess.STDOUT, universal_newlines=True)
    except subprocess.CalledProcessError as e:
        output = e.output
        code = e.returncode
        finished = False
        status = classify_error(cmd,e)

    # does not work with Pool.map
    end_usr = os.times().children_user
    end_sys = os.times().children_system

    date_end=datetime.utcnow()

    diff = date_end - date_start
    cputime_taken = (end_usr - start_usr) + (end_sys - start_sys)


    if "conjure" in cmd and "Timed out" in output:
        status=Status.timeout
        finished=False

    # Might be simpler to run SR and minion our self
    if code == 0 and "savilerow" in cmd:
        if "Savile Row timed out" in output:
            finished = False
            status=Status.timeout
        else:
            with open(cmd[13]) as f:
                (m_timeout,m_total,sr_real)  = [float(l.split(":")[1])
                        for l in f.readlines()
                    if l.split(":")[0] in
                         {"MinionTimeOut","MinionTotalTime","SavileRowTotalTime"}]
                if int(m_timeout) == 1:
                    if cputime_taken == 0: #Best we can do at this point
                        #because some killed processes don't return cputime
                        logger.warn("Adding %2.0f to cpu_taken(%2.0f) cpu timeout",
                            m_total, cputime_taken)
                        cputime_taken +=  sr_real
                    cputime_taken+=m_total
                    finished=False
                    status=Status.timeout


        logger.info("Took %0.2f (%0.2f real), reported user %0.2f sys %0.2f",
                cputime_taken, diff.total_seconds(),
                (end_usr - start_usr), (end_sys - start_sys))

    return (Results(rcode=code,
                  cpu_time=cputime_taken, real_time=diff.total_seconds(),
                  timeout=timeout, finished=finished,cmd=cmd, status_=status)
           ,output)
