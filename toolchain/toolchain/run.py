# -*- coding: utf-8 -*-
import hashlib
import logging
import math
import os
import shlex
import subprocess
import random

from collections import namedtuple
from datetime import datetime
from enum import Enum, unique
from functools import partial
from multiprocessing import Pool
from pprint import pprint, pformat
from textwrap import indent

from .command import K


logger = logging.getLogger(__name__)


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
    statusAny        = 14,
    javaException    = 15,
    notAHomoType     = 16,
    forgetRepr       = 17,
    notRefined       = 18,
    unknownLexeme    = 19,
    ruleApplication  = 20
    typeError        = 21




def run_refine_essence(*, op, commands, random, cores, extra_env):
    limit = op.timeout
    date_start = datetime.utcnow()

    mapping = dict(essence=op.essence, outdir=op.outdir)
    mapping['itimeout']  = int(math.ceil(limit))
    mapping['seed_base'] = uniform_int(0, 2 ** 24)
    mapping['saved_choices'] = op.choices

    rr = partial(run_refine, extra_env, commands, mapping)
    pool = Pool(cores)

    if op.choices:
        end = 1
    else:
        end = random + 1

    rnds = list(pool.map(rr, range(0, end)))
    (results, outputs) =list(zip( *(  rnds ) ))

    with (op.outdir / "_refine.outputs").open("w") as f:
        f.write("\n".join(  [ "###" + arr + "\n" + output for (arr, output) in outputs ] ))

    date_end=datetime.utcnow()
    diff = date_end - date_start


    results_unique = remove_refine_dups(results=results, outputs=outputs, op=op)


    return (dict(results_unique.values()), sum( data['real_time']
                for (_, data) in results  ) )



# global function for run_refine_essence
# because nested function can't be pickled
def run_refine(extra_env, commands, kwargs, i):

    if i == 0:
        eprime = kwargs['outdir'] / "model000000.eprime"
        (cmd_kind, cmd_template) = commands.refine_compact
    else:
        eprime = kwargs['outdir'] / "model{:06}.eprime".format(i)
        (cmd_kind, cmd_template) = commands.refine_random

    choices_json= eprime.with_suffix('.choices.json')

    seed = kwargs['seed_base'] + i
    cmd_arr=shlex.split(cmd_template.format(eprime=eprime, index=i, seed=seed, **kwargs))

    # logger.warn("running %s:  %s\n\t%s", cmd_kind, cmd_arr, " ".join(cmd_arr))
    vals = dict(eprime=eprime, index=i, seed=seed, choices_json=choices_json, **kwargs)
    (res, output) = run_with_timeout(kwargs['itimeout'], cmd_kind, cmd_arr, extra_env=extra_env, vals=vals)

    dic = res.__dict__
    dic.update(vals=vals)
    return ((eprime.stem, dic), (" ".join(cmd_arr), output) )




def run_refine_essence_with_choices(*, op, commands, extra_env):
    limit = op.timeout
    date_start = datetime.utcnow()

    mapping = dict(essence=op.essence, outdir=op.outdir)
    mapping['itimeout']  = int(math.ceil(limit))
    mapping['seed'] = uniform_int(0, 2 ** 24)
    mapping['saved_choices'] = op.choices
    mapping['choices_json']  = op.outdir / 'follow.choices.json'


    (cmd_kind, cmd_template) = commands.log_follow

    cmd_arr=shlex.split(cmd_template.format(**mapping))

    (res, output0) = run_with_timeout(mapping['itimeout'], cmd_kind, cmd_arr, extra_env=extra_env, vals=mapping)
    output = "###" + " ".join(cmd_arr) + "\n" + output0

    date_end=datetime.utcnow()
    diff = date_end - date_start

    with (op.outdir / "_refine.outputs").open("w") as f:
        f.write(output)

    with (op.outdir / "follow.refine-output").open("w") as f:
        f.write(output0)

    dic = res.__dict__
    dic.update(vals=mapping)

    eprimes = [ ep.name for ep in op.outdir.glob('*.eprime') ]
    return (dict(eprime_names=eprimes, choices_made=mapping['choices_json'],
                     cmd_used=dic), res.real_time)


def run_refine_all_essence(*, op, commands, extra_env):
    limit = op.timeout
    date_start = datetime.utcnow()

    mapping = dict(essence=op.essence, outdir=op.outdir)
    mapping['itimeout']      = int(math.ceil(limit))
    mapping['seed']          = uniform_int(0, 2 ** 24)
    mapping['choices_json']  = op.outdir / 'all.choices.json'


    (cmd_kind, cmd_template) = commands.refine_all

    cmd_arr=shlex.split(cmd_template.format(**mapping))

    logger.warn("running %s:  %s\n\t%s", cmd_kind, cmd_arr, " ".join(cmd_arr))
    (res, output0) = run_with_timeout(mapping['itimeout'], cmd_kind, cmd_arr, extra_env=extra_env, vals=mapping)
    output = "###" + " ".join(cmd_arr) + "\n" + output0

    date_end=datetime.utcnow()
    diff = date_end - date_start

    with (op.outdir / "_refine.outputs").open("w") as f:
        f.write(output)

    with (op.outdir / "all.refine-output").open("w") as f:
        f.write(output0)

    dic = res.__dict__
    dic.update(vals=mapping)
    eprimes = [ ep.name for ep in op.outdir.glob('*.eprime') ]
    return (dict(eprime_names=eprimes, choices_made=mapping['choices_json'],
                     cmd_used=dic), res.real_time)




def remove_refine_dups(*, results, outputs, op):
    results_unique = {}
    refine_error_hashes=set()

    exts = ['.eprime.logs', '.eprime.error', '.eprime-solution', '.solution',
             '.output', '.eprime-param', '.refine-output', '.choices.json']

    for (result, (_, output)) in zip(results, outputs):
        (eprime_name, _) = result
        ep = (op.outdir/ eprime_name).with_suffix(".eprime")
        log = ep.with_suffix('.refine-output')
        with log.open('w') as f:
            f.write(output)

        if not ep.exists():
            err_hash = hash_path( log )
            if err_hash not in refine_error_hashes:
                refine_error_hashes.add(err_hash)
                results_unique[eprime_name] = result
            else:
                logger.warn("Removing %s it is a duplicate  ", log.name)
                for ext in exts:
                    if ep.with_suffix(ext).exists():
                        ep.with_suffix(ext).unlink()

            continue

        hf = hash_path( ep )
        if hf not in results_unique:
            results_unique[hf] = result
        else:
            logger.warn("Removing eprime %s it a duplicate of %s",
                    eprime_name, results_unique[hf][0] )
            ep.unlink()

            for ext in exts:
                if ep.with_suffix(ext).exists():
                    ep.with_suffix(ext).unlink()

    return results_unique

def run_solve(extra_env, op, commands, limit, eprime):
    essence          = op.essence
    essence_param    = op.param
    eprime_param     = eprime.with_suffix(".eprime-param")
    eprime_solution  = eprime.with_suffix(".eprime-solution")
    eprime_info      = eprime.with_suffix(".eprime.info")
    essence_solution = eprime.with_suffix(".solution")
    minion           = eprime.with_suffix(".minion")
    solve_log        = eprime.parent / "_solve.outputs"

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

        if cmd_kind == K.savilerow:
            try:
                out = subprocess.check_output(["savilerow"], universal_newlines=True)
            except subprocess.CalledProcessError as e:
                out = e.output
            if '-run-minion' in out:
                # cmd_template = cmd_template.replace('-run-solver', '-run-minion')
                cmd_template = cmd_template.replace('-run-solver', '-run-minion minion')
            if '-minion-options <string>' in out:
                cmd_template = cmd_template.replace('-solver-options', '-minion-options')

        c=shlex.split(cmd_template.format(**locals()))
        vals = dict(essence=essence,
                            essence_param=essence_param,
                            essence_solution=essence_solution,
                            eprime_solution=eprime_solution,
                            eprime_info=eprime_info,
                            eprime_param=eprime_param,
                            eprime=eprime,
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

        outputs.append("###" + " ".join(c))
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

    with solve_log.open("a") as f:
        f.write("\n".join(outputs))

    return (eprime.stem, ret)




def classify_error(*, kind, output, returncode):
    kind_conjure = {K.refineAll, K.refineRandom, K.refineCompact, K.refineParam,
                    K.translateUp, K.validate, K.validateOld}
    if kind == K.savilerow:
        if "java.lang.NumberFormatException: For input string: " in output:
            return Status.numberToLarge
        if "Failed when parsing rest of structure following" in output:
            return Status.parseError
        if 'Failed type checking' in output:
            return Status.typeChecking
        if 'declared more than once.' in output:
            return Status.varDuplicated
        if 'Exception in thread' in output:
            return Status.javaException
        if 'Savile Row timed out' in output:
            return Status.timeout

    if kind in kind_conjure:
        if returncode == 252:
            return Status.heapSpace
        if 'conjureNew: Heap exhausted' in output:
            return Status.heapSpace

    if kind == K.validateOld and 'Value not in' in output:
        return Status.valueNotInDom

    if kind == K.validate:
        if ': negativeExponent' in output:
            return Status.negativeExponent
        if ': Negative exponent' in output:
            return Status.negativeExponent
        if ': divideByZero' in output:
            return Status.divideByZero
        if ': divide by zero' in output:
            return Status.divideByZero
        if ': Invalid' in output:
            return Status.conjureInvalid

    if kind in kind_conjure:
        if 'Shunting Yard failed' in output:
            return Status.parseError
        if 'Cannot fully evaluate' in output:
            return Status.cannotEvaluate
        if 'not a homoType' in output:
            return Status.notAHomoType
        if 'N/A:' in output:
            return Status.conjureNA
        if 'forgetRepr' in output:
            return Status.forgetRepr
        if 'Unknown lexeme' in output:
            return Status.unknownLexeme
        if 'Not refined:' in output:
            return Status.notRefined
        if 'Type error after rule application' in output:
            return Status.ruleApplication
        if 'Type error before rule application' in output:
            return Status.ruleApplication
        if 'Error: Type error' in output:
            return Status.typeError
        if 'There were type errors' in output:
            return Status.typeChecking

    return Status.errorUnknown

def run_with_timeout(timeout, kind, cmd, *, extra_env, vals):
    logging.warn("Running %s", " ".join(cmd))

    if kind == K.refineCompact or kind == K.refineRandom or kind == K.refineAll:
        return run_conjure_with_choices(timeout, kind, cmd, extra_env=extra_env, vals=vals)
    else:
        return run_process(timeout, kind, cmd, extra_env=extra_env, vals=vals)


Results = namedtuple("Results", "rcode cpu_time real_time timeout finished cmd status_ kind_")
def run_process(timeout, kind, cmd, *, extra_env, vals):
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
        status = classify_error(kind=kind, output=e.output, returncode=e.returncode)

    # does not work with Pool.map
    end_usr = os.times().children_user
    end_sys = os.times().children_system

    date_end=datetime.utcnow()

    diff = date_end - date_start
    cputime_taken = (end_usr - start_usr) + (end_sys - start_sys)


    # Might be simpler to run SR and minion our self
    if code == 0 and kind == K.savilerow:
        # Old SR returns zero sometimes on error
        mayErr = classify_error(kind=kind,output=output,returncode=0)
        if mayErr != Status.errorUnknown:
            finished = False
            status=mayErr
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


    return (Results(rcode=code,
                  cpu_time=cputime_taken, real_time=diff.total_seconds(),
                  timeout=timeout, finished=finished,
                  cmd=cmd, status_=status, kind_=kind), output)



def run_conjure_with_choices(timeout, kind, cmd, *, extra_env, vals):

    code = 0
    finished = True
    status = Status.success

    date_start = datetime.utcnow()
    start_usr = os.times().children_user
    start_sys = os.times().children_system
    if extra_env:
        my_env = os.environ
        my_env.update(extra_env)
    else:
        extra_env=None

    lines = []
    saved_first_choice=False

    with vals['choices_json'].open('w') as choices:
        choices.write("[")
        with subprocess.Popen(cmd, stdout=subprocess.PIPE, universal_newlines=True,
                        env=extra_env, stderr=subprocess.STDOUT, bufsize=1 ) as proc:
            for line in iter( proc.stdout.readline, ''):
                if line.startswith("LF:") and line.endswith(" END:\n"):
                    if saved_first_choice:
                        choices.write(",")
                        choices.write(line[3:-6])
                        choices.write("\n")
                    else:
                        choices.write(line[3:-6])
                        saved_first_choice = True
                        choices.write("\n")

                else:
                    print(line, end='')
                    lines.append(line)

            code = proc.wait()


        choices.write("]")


    # does not work with Pool.map
    end_usr = os.times().children_user
    end_sys = os.times().children_system

    date_end=datetime.utcnow()

    diff = date_end - date_start
    cputime_taken = (end_usr - start_usr) + (end_sys - start_sys)

    output = "".join(lines)


    if code != 0:
        finished = False
        status = classify_error(kind=kind, output=output, returncode=code)

    if "Timed out" in output:
        status=Status.timeout
        finished=False

    return (Results(rcode=code,
                  cpu_time=cputime_taken, real_time=diff.total_seconds(),
                  timeout=timeout, finished=finished,
                  cmd=cmd, status_=status, kind_=kind), output)


def hash_path(path):
    sha = hashlib.sha1()
    with path.open('rb') as f:
        sha.update(b"".join([ line for line in f.readlines()
            if not line.startswith(b"###") ]  ))
    return sha.hexdigest()

def uniform_int(l, u):
    return math.ceil(random.uniform(l - 1, u))
