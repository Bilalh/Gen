import logging
import math
import os
import sys
import subprocess

from datetime import datetime
from pathlib import Path
from pprint import pprint

from collections import namedtuple
from enum import Enum,IntEnum, unique

logger = logging.getLogger(__name__)
logging.basicConfig(format='%(lineno)d:%(funcName)s: %(message)s',
        level=logging.INFO)

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
