import logging
import math
import os
import subprocess
import shlex
import hashlib


from datetime import datetime
from pprint import pprint, pformat

from collections import namedtuple
from enum import Enum, unique
from functools import partial
from multiprocessing import Pool
from textwrap import indent

from command import K
import command
import random

logger = logging.getLogger(__name__)

def hash_path(path):
    sha = hashlib.sha1()
    with path.open('rb') as f:
        sha.update(f.read())
    return sha.hexdigest()


# global function for run_refine_essence
# because nested function can't be pickled
def run_refine(extra_env, commands, kwargs, i):
    if i == 0:
        eprime = kwargs['outdir'] / "model000000.eprime"
        (cmd_kind, cmd_template) = commands.refine_compact
    else:
        eprime = kwargs['outdir'] / "model{:06}.eprime".format(i)
        (cmd_kind, cmd_template) = commands.refine_random

    seed = kwargs['seed_base'] + i
    cmd_arr=shlex.split(cmd_template.format(eprime=eprime, index=i, seed=seed, **kwargs))

    logger.warn("running %s:  %s\n\t%s", cmd_kind, cmd_arr, " ".join(cmd_arr))
    vals = dict(eprime=eprime, index=i, seed=seed, **kwargs)
    (res, output) = run_with_timeout(kwargs['itimeout'], cmd_kind, cmd_arr, extra_env=extra_env, vals=vals)

    dic = res.__dict__
    dic.update(vals=vals)
    return ((eprime.stem, dic), " ".join(cmd_arr) + "\n" + output)


def run_refine_all_essence(*, op, commands, extra_env):
    limit = op.timeout
    date_start = datetime.utcnow()

    mapping = dict(essence=op.essence, outdir=op.outdir)
    mapping['itimeout']  = int(math.ceil(limit))
    mapping['seed'] = uniform_int(0, 2 ** 24)


    (cmd_kind, cmd_template) = commands.refine_all

    cmd_arr=shlex.split(cmd_template.format(**mapping))

    logger.warn("running %s:  %s\n\t%s", cmd_kind, cmd_arr, " ".join(cmd_arr))
    (res, output0) = run_with_timeout(mapping['itimeout'], cmd_kind, cmd_arr, extra_env=extra_env, vals=mapping)
    output = " ".join(cmd_arr) + "\n" + output0

    date_end=datetime.utcnow()
    diff = date_end - date_start

    with (op.outdir / "_refine.outputs").open("w") as f:
        f.write(output)

    with (op.outdir / "all.refine-output").open("w") as f:
        f.write(output)

    dic = res.__dict__
    dic.update(vals=mapping)
    return (dict(all=dic), res.real_time)



def run_refine_essence(*, op, commands, random, cores, extra_env):
    limit = op.timeout
    date_start = datetime.utcnow()

    mapping = dict(essence=op.essence, outdir=op.outdir)
    mapping['itimeout']  = int(math.ceil(limit))
    mapping['seed_base'] = uniform_int(0, 2 ** 24)

    rr = partial(run_refine, extra_env, commands, mapping)
    pool = Pool(cores)
    rnds = list(pool.map(rr, range(0, random + 1)))
    (results, outputs) =list(zip( *(  rnds ) ))

    with (op.outdir / "_refine.outputs").open("w") as f:
        f.write("\n".join(outputs))

    date_end=datetime.utcnow()
    diff = date_end - date_start


    results_unique = {}
    for (result, output) in zip(results, outputs):
        (eprime_name, _) = result
        ep = (op.outdir/ eprime_name).with_suffix(".eprime")
        log = ep.with_suffix('.refine-output')
        with log.open('w') as f:
            f.write(output)

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

            if isinstance(commands, command.ConjureOld):
                ep.with_suffix(".eprime.logs").unlink()
            ep.with_suffix(".refine-output").unlink()


    return (dict(results_unique.values()), sum( data['real_time']
                for (_, data) in results  ) )

def uniform_int(l, u):
    return math.ceil(random.uniform(l - 1, u))

def run_solve(extra_env, op, commands, limit, eprime):
    essence          = op.essence
    essence_param    = op.param
    eprime_param     = eprime.with_suffix(".eprime-param")
    eprime_solution  = eprime.with_suffix(".eprime-solution")
    eprime_info      = eprime.with_suffix(".eprime.info")
    essence_solution = eprime.with_suffix(".solution")
    minion           = eprime.with_suffix(".minion")

    cmds = commands.sovlve_cmds
    results=[]
    outputs=[]
    total_cpu_time=0
    total_real_time=0
    all_finished=True
    solving_finished=False
    erroed = None
    last_status=Status.success
    last_kind  = None


    for i, (cmd_kind, cmd_template) in enumerate(cmds):

        # always perform solution translation and vaildation
        if cmd_kind in {"translate_up", "validate"}:
            limit = max([limit, 10])

        if limit <= 0:
            logger.warn("### NO_TIME_LEFT before cmd %s %s", cmd_kind, cmd_template)
            all_finished=False
            break

        itimeout=int(math.ceil(limit))
        mstimeout=itimeout * 1000

        c=shlex.split(cmd_template.format(**locals()))
        logger.warn("running %s\n%s", c, " ".join(c))
        vals = dict(essence=essence,
                            essence_param=essence_param,
                            eprime_solution=eprime_solution,
                            eprime_info=eprime_info,
                            essence_solution=essence_param,
                            minion=minion,
                            itimeout=itimeout,
                            mstimeout=mstimeout
                    )
        (res, output) = run_with_timeout(limit, cmd_kind, c, extra_env=extra_env, vals=vals)

        dres = res.__dict__
        dres['vals'] = vals

        results.append(dres)

        otimeout = limit
        limit -= res.cpu_time
        total_cpu_time += res.cpu_time
        total_real_time += res.real_time

        outputs.append(" ".join(c))
        outputs.append(output)

        if res.status_ == Status.timeout:
            logger.warn("###TIMEDOUT(%0.2f) for cmd \n%s\n%s", otimeout,
                    " ".join(c), indent(output, " \t") )
            all_finished=False
            break
        elif res.status_ != Status.success:
            logger.warn("###ERROR %s for cmd \n%s\n%s",
                    res.status_, " ".join(c), indent(output, " \t") )
            erroed=i
            all_finished=False
            last_status = res.status_
            last_kind   = cmd_kind
            break
        elif cmd_kind == K.translateUp:
            solving_finished=True

        if cmd_kind == K.savilerow and not eprime_solution.exists():
            logger.info("No eprime solution")
            solving_finished=True
            break

    ret = dict(results=results,
            total_cpu_time=total_cpu_time,
            total_real_time=total_real_time,
            all_finished=all_finished,
            solving_finished=solving_finished,
            erroed=erroed,
            last_status=last_status,
            last_kind=last_kind)

    with eprime.with_suffix(".output").open("w") as f:
        f.write("\n".join(outputs))

    return (eprime.stem, ret)


@unique
class Status(Enum):
    success          = 0,
    errorUnknown     = 1,
    timeout          = 2,
    numberToLarge    = 3,
    heapSpace        = 4,
    cannotEvaluate   = 5,
    valueNotInDom    = 6,
    parseError       = 7,
    typeChecking     = 8,
    varDuplicated    = 9,
    negativeExponent = 10,
    divideByZero     = 11,
    conjureNA        = 12,
    conjureInvalid   = 13,
    statusAny        = 14

errors_not_useful = {Status.numberToLarge}


def classify_error(kind, c, e):
    kind_conjure = {K.refineRandom, K.refineCompact, K.refineParam, K.translateUp, K.validate, K.validateOld}
    if kind == K.savilerow:
        if "java.lang.NumberFormatException: For input string: " in e.output:
            return Status.numberToLarge
        if "Failed when parsing rest of structure following" in e.output:
            return Status.parseError
        if 'Failed type checking' in e.output:
            return Status.typeChecking
        if 'declared more than once.' in e.output:
            return Status.varDuplicated

    if kind in kind_conjure:
        if e.returncode == 252:
            return Status.heapSpace

    if kind == K.validateOld and 'Value not in' in e.output:
        return Status.valueNotInDom

    if kind == K.validate:
        if ': negativeExponent' in e.output:
            return Status.negativeExponent
        if ': divideByZero' in e.output:
            return Status.divideByZero
        if ': Invalid' in e.output:
            return Status.conjureInvalid

    if kind in kind_conjure:
        if 'Cannot fully evaluate' in e.output:
            return Status.cannotEvaluate
        if 'N/A:' in e.output:
            return Status.conjureNA

    return Status.errorUnknown

Results = namedtuple("Results", "rcode cpu_time real_time timeout finished cmd status_ kind_")
def run_with_timeout(timeout, kind, cmd, *, extra_env, vals):
    code = 0
    finished = True
    status = Status.success
    try:
        date_start = datetime.utcnow()
        start_usr = os.times().children_user
        start_sys = os.times().children_system
        if extra_env:
            my_env = os.environ
            my_env.update(extra_env)
        else:
            extra_env=None
        output = subprocess.check_output(cmd,
                stderr=subprocess.STDOUT, universal_newlines=True, env=extra_env)
    except subprocess.CalledProcessError as e:
        output = e.output
        code = e.returncode
        finished = False
        status = classify_error(kind, cmd, e)

    # does not work with Pool.map
    end_usr = os.times().children_user
    end_sys = os.times().children_system

    date_end=datetime.utcnow()

    diff = date_end - date_start
    cputime_taken = (end_usr - start_usr) + (end_sys - start_sys)


    if (kind == K.refineCompact or kind == K.refineRandom) \
            and "Timed out" in output:
        status=Status.timeout
        finished=False

    # Might be simpler to run SR and minion our self
    if code == 0 and kind == K.savilerow:
        if "Savile Row timed out" in output:
            finished = False
            status=Status.timeout
        else:
            with vals['eprime_info'].open() as f:
                (sr_real, m_timeout, m_total)  = [ float(l.split(":")[1])
                    for l in sorted(f.readlines()) if l.split(":")[0]
                        in {"SavileRowTotalTime", "SolverTimeOut", "SolverTotalTime", "MinionTimeOut", "MinionTotalTime"}]
                if int(m_timeout) == 1:
                    if cputime_taken == 0:  # Best we can do at this point
                        # because some killed processes don't return cputime
                        logger.warn("Adding %2.0f to cpu_taken(%2.0f) cpu timeout",
                            m_total, cputime_taken)
                        cputime_taken += sr_real
                    cputime_taken+=m_total
                    finished=False
                    status=Status.timeout


        logger.info("Took %0.2f (%0.2f real), reported user %0.2f sys %0.2f",
                cputime_taken, diff.total_seconds(),
                (end_usr - start_usr), (end_sys - start_sys))

    return (Results(rcode=code,
                  cpu_time=cputime_taken, real_time=diff.total_seconds(),
                  timeout=timeout, finished=finished,
                  cmd=cmd, status_=status, kind_=kind), output)
