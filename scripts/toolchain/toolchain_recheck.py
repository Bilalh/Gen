#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Runs the whole toolchain (compact + rnd models >> SR + Minion)
"""
import json
import logging
import math
import os
import sys


from functools import partial
from pathlib import Path
from pprint import pprint, pformat
from multiprocessing import Pool

import run
import toolchain
from command import K, ConjureOld, ConjureNew

from run import Status
import time

import argparse
from distutils import file_util
from textwrap import indent
import shlex

logger = logging.getLogger(__name__)

def data_convert_back(d):
    if 'kind_' in d:
        k = d['kind_'][0].lower()+ d['kind_'][1:-1]
        d['kind_'] = getattr(K, k)

    if 'status_' in d:
        status = d['status_'][0].lower() + d['status_'][1:-1]
        d['status_'] = getattr(Status, status)

    for n in ['eprime', 'essence', 'eprime_info', 'eprime_param', 'minion',
            'eprime_solution', 'essence_param', 'essence_solution',
            'outdir', 'outdir_']:
        if n in d:
            d[n] = Path(d[n])

    return d


def copy_file(path1, path2):
    file_util.copy_file(str(path1), str(path2))

def do_args():
    parse_args = argparse.ArgumentParser()
    parse_args.add_argument("indir", help='directory refine_essence.json and/or solve_eprime.json')
    parse_args.add_argument("outdir", help='Where to store the new results')
    parse_args.add_argument("--num_cores", type=int, default=1)
    parse_args.add_argument("--new_conjure", action='store_true',
            help='Use new conjure, must be called conjure-new')
    parse_args.add_argument("--bin_dir", help='Use the specifed directory for binaries (give a full path)')

    args = parse_args.parse_args()

    args.indir = Path(args.indir)
    if not args.indir.exists():
        print("%s does not exist" % args.indir )
        sys.exit(4)

    args.outdir = Path(args.outdir)
    if not args.outdir.exists():
        args.outdir.mkdir(parents=True)

    return args


def rerun_refine(extra_env, itimeout, data):
    (eprime_name, cmd_data) =data
    cmd_arr = cmd_data['cmd']


    (res, output) = run.run_with_timeout(itimeout, cmd_data['kind_'], cmd_arr,
        extra_env=extra_env, vals=cmd_data['vals'])
    dic = res.__dict__
    dic.update(vals=cmd_data['vals'])
    return ((eprime_name, dic), " ".join(cmd_arr) + "\n" + output)


def rerun_refine_essence(*, extra_env, outdir, limit, cores, datas):

    rr = partial(rerun_refine, extra_env, int(math.ceil(limit)))
    pool = Pool(cores)
    rnds = list(pool.map(rr, datas))
    (results, outputs) =list(zip( *(  rnds ) ))

    with (outdir / "_refine.outputs").open("w") as f:
        f.write("\n".join(outputs))

    results_unique = {}
    for (result, output) in zip(results, outputs):
        (eprime_name, _) = result
        ep = (outdir/ eprime_name).with_suffix(".eprime")
        log = ep.with_suffix('.refine-output')
        with log.open('w') as f:
            f.write(output)

        results_unique[eprime_name] = result

    return (dict(results_unique.values()), sum( data['real_time']
                for (_, data) in results  ) )


op = do_args()
pprint(op)
print("")


if op.new_conjure:
    Com = ConjureNew()
else:
    Com = ConjureOld()


if op.bin_dir:
    extra_env = dict(PATH= op.bin_dir + ":" + os.environ['PATH'])
else:
    extra_env = {}

with (op.indir / "refine_essence.json").open() as f:
    refine_json=json.load(f, object_hook=data_convert_back)

oldBase = refine_json['outdir_']
newBase = op.outdir
os.makedirs(str(newBase), exist_ok=True)
essence=Path(newBase / "spec.essence")

copy_file(Path(op.indir / "spec.essence"), essence )
copy_file(Path(op.indir / "empty.param"), Path(newBase / "empty.param") )


def update_cmd_paths(v):
    v['vals'].update(outdir=newBase)

    for name in ['eprime', 'essence', 'eprime_info', 'eprime_param', 'minion',
            'eprime_solution', 'essence_param', 'essence_solution']:
        if name in v['vals']:
            v['vals'][name] = newBase / v['vals'][name].name



    (new_kind, new_cmd_) = Com.kind_to_template(v['kind_'])
    new_cmd = new_cmd_.format(**v['vals'])
    v['cmd'] = shlex.split(new_cmd)
    v['kind_'] = new_kind
    return v

datas = [ (k, update_cmd_paths(v)) for k, v in refine_json['data_'].items()  ]


startTime = time.time()
# Make the eprimes
(new_essence_refine, new_refine_wall_time) = rerun_refine_essence(
    outdir=newBase,
    limit=refine_json['given_time_'],
    cores=op.num_cores, datas=datas, extra_env=extra_env)

endTime = time.time()
logger.warn("essence_refine: %s", pformat(new_essence_refine))



def with_re_settings(results, *, essence, outdir,
                    given_time, time_taken, successful, consistent):
    return dict(
        data_       = results,
        essence_    = essence,
        outdir_     = outdir,
        given_time_ = given_time,
        time_taken_ = time_taken,
        successful_ = successful,
        consistent_ = consistent
    )

successful = all(  res['status_'] in [Status.success, Status.timeout]
        for res in new_essence_refine.values() )

settings = with_re_settings(new_essence_refine,
        essence=essence,
        outdir=newBase,
        given_time=refine_json['given_time_'],
        time_taken=new_refine_wall_time,
        successful=successful, consistent=True)


with ( newBase / "refine_essence.json" ).open("w") as f:
    f.write(json.dumps(settings, indent=True, sort_keys=True, default=toolchain.obj_to_json ))


if not successful:
    logger.warn("Not successful when refining")
    sys.exit(5)

if not (op.indir / "solve_eprime.json").exists():
    logger.warn("no solve_eprime.json")
    sys.exit(0)

# solve

def rerun_solve(extra_env, outdir, limit, kv):
    eprime_name, data=kv

    results=[]
    outputs=[]
    total_cpu_time=0
    total_real_time=0
    all_finished=True
    solving_finished=False
    erroed = None
    last_status=Status.success
    last_kind=None


    for i, (cmd_kind, cmd_data) in enumerate(data):
        solve_log        = cmd_data['vals']['eprime'].parent / "_solve.outputs"

        cmd_arr = cmd_data['cmd']

        logger.warn("running:  %s\n", " ".join(cmd_arr))
        (res, output) = run.run_with_timeout(limit, cmd_kind, cmd_arr,
            vals=cmd_data['vals'], extra_env=extra_env)

        dres = res.__dict__
        dres['vals'] = cmd_data['vals']
        results.append(dres)

        limit -= res.cpu_time
        total_cpu_time += res.cpu_time
        total_real_time += res.real_time

        outputs.append(" ".join(cmd_arr))
        outputs.append(output)

        if res.status_ != Status.success:
            logger.warn("###ERROR %s for cmd \n%s\n%s",
                    res.status_, " ".join(cmd_arr), indent(output, " \t") )
            erroed=i
            all_finished=False
            last_status = res.status_
            last_kind  = cmd_kind
            break


    ret = dict(results=results,
            total_cpu_time=total_cpu_time,
            total_real_time=total_real_time,
            all_finished=all_finished,
            solving_finished=solving_finished,
            erroed=erroed,
            last_status=last_status,
            last_kind=last_kind)

    with (outdir / eprime_name).with_suffix(".output").open("w") as f:
        f.write("\n".join(outputs))

    with solve_log.open("a") as f:
        f.write("\n".join(outputs))

    return (eprime_name, ret)


with (op.indir / "solve_eprime.json").open() as f:
    solve_eprime_json=json.load(f, object_hook=data_convert_back)


def check_kind(r):
    if op.new_conjure and r['kind_'] == 'ValidateOld_':
        return False
    else:
        return True


def org_data(datas):
    return [  (r['kind_'], update_cmd_paths(r)) for r in datas['results'] if check_kind(r) ]


sdatas = [ (k, org_data(v) ) for k, v in solve_eprime_json['data_'].items()  ]
logger.warn("sdatas %s", pformat(sdatas))

solve_op = partial(rerun_solve, extra_env, newBase, solve_eprime_json['given_time_'])

pool = Pool(op.num_cores)
solve_results = dict(pool.map(solve_op, sdatas))
logger.info("solve_results: %s", pformat(solve_results))

solve_wall_time = sum(  res['total_real_time'] for res in solve_results.values()  )
solve_successful = all( not res['erroed'] for res in solve_results.values() )


# checks that all eprimes that were solved either have a solution
# or don't have a solution
def is_consistent():
    fin_names = [ k for (k, v) in solve_results.items()
                    if not v['erroed'] and v['solving_finished'] ]
    sol_exist = [ (op.outdir / name).with_suffix(".solution").exists()
                    for name in fin_names ]
    return all(sol_exist) or all( [ not b for b in sol_exist])

settings = with_re_settings(solve_results,
        essence=essence,
        outdir=newBase,
        given_time=refine_json['given_time_'], time_taken=solve_wall_time,
        successful=solve_successful, consistent=is_consistent())

with (newBase / "solve_eprime.json" ).open("w") as f:
    f.write(json.dumps(settings,
        indent=True, sort_keys=True, default=toolchain.obj_to_json ))


logger.warn("completed")

if not successful:
    sys.exit(2)

if not solve_successful:
    sys.exit(3)

sys.exit(0)
