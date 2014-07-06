import logging
import math
import os
import sys
import subprocess
import shlex
import commands


from datetime import datetime
from pathlib import Path
from pprint import pprint

from collections import namedtuple
from enum import Enum,IntEnum, unique
from functools import partial
from multiprocessing import Pool


logger = logging.getLogger(__name__)
logging.basicConfig(format='%(lineno)d:%(funcName)s: %(message)s',
        level=logging.INFO)


# global function because nested function can't be pickled
def run_refine(kwargs,i):
    eprime = kwargs['outdir'] / "{:04}-{}.eprime".format(i,kwargs['essence'].stem)
    c=shlex.split(commands.ConjureRandom.format(eprime=eprime, **kwargs))
    logger.warn("running %s", c)      
    (res, output) = run_with_timeout(kwargs['itimeout'], c)
    return (res,output)


def run_refine_essence(*,op,compact=True,random=4):
    limit = op.timeout
    
    eprime = op.outdir / "comp-{}.eprime".format(op.essence.stem)
    c=shlex.split(commands.ConjureCompact.format(
        itimeout = int(math.ceil(limit)), 
        eprime=eprime, essence=op.essence))
    logger.warn("running %s", c)      
    compact = run_with_timeout(limit, c)

    limit -= compact[0].cpu_time
    mapping = dict(essence=op.essence,outdir=op.outdir)
    mapping['itimeout'] = int(math.ceil(limit))
    
    rr = partial(run_refine,mapping)
    pool = Pool()
    rnds = list(pool.map(rr,range(1,random+1)))

    results = [compact] + rnds
    return (results, sum( data.cpu_time for (data,_) in results  ) )


@unique
class Status(Enum):
    success       = 0,
    errorUnknown  = 1,
    timeout       = 2,
    numberToLarge = 3

errors_not_useful = {Status.numberToLarge}

def classify_error(c,e):
    if "savilerow" in c:
        if "java.lang.NumberFormatException: For input string: " in e.output:
            return Status.numberToLarge

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
                with eprime_info.open() as f:
                    (m_timeout,m_total,sr_real)  = [float(l.split(":")[1]) for l in f.readlines()
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
                  cpu_time=cputime_taken, real_time= diff.total_seconds(),
                  timeout=timeout, finished=finished,cmd=cmd, status_=status)
           ,output)
