#!/usr/bin/env python3
# Reduce a spec create by `gen`

# -*- coding: utf-8 -*-
import json
import sys
import shlex
import subprocess
# from toolchain_recheck import data_convert_back

import logging
import argparse
from pathlib import Path
logger = logging.getLogger(__name__)

parser = argparse.ArgumentParser()
parser.add_argument("essence", help='essence file')
parser.add_argument('-o',  dest='output',          help='output base dir ', required=True)
parser.add_argument('-p',  dest='total_time',      help='time per run', required=True, type=int)
parser.add_argument('-bin_dir',  dest='bin_dir',   help='--bin-dir' )
parser.add_argument('-c',  dest='cores',           help='cores to use, default 1', type=int)
parser.add_argument('--passing_db',                help='for gen reduce --db-passing-in')


args = parser.parse_args()

essence = Path(args.essence)
essence_dir = essence.parent

cmd_str="""
     gen reduce {essence_dir} -o '{output}' -p {total_time}  --kind {kind} --status {status} --delete-passing --delete-steps -N  --db-dir '{db_dir}'
"""

if args.passing_db:
    cmd_str+= " --db-passing-in '{}'".format(args.passing_db)

def process(status, kind,refinement, name, vals,refine_times,cmd_str,is_last):
    if is_last:
        cmd_str+=" --db-only-passing "

    if status == "ErrorUnknown_":
        status = "StatusAny_"

    total_time=args.total_time
    db_dir = Path(args.output) / "temp_db"
    if refinement:
        if 'choices_json' in vals['vals']:
            cs=Path(vals['vals']['choices_json']).name
            choices = essence_dir / cs


            if choices.suffixes == [".choices", ".json"]:
                new_name = choices.name.replace(".choices.json", ".choices-new.json")
                if (essence_dir / new_name).exists():
                    choices = essence_dir / new_name

            out_dir = Path(args.output) / choices.stem

            cmd_str += " --choices {}".format(choices)

        else:
            out_dir = Path(args.output) / "all"

        cpu_used = vals['cpu_time']
        total_time = min( max(cpu_used * 1.5, 10), total_time)

    else:
        cmd_str += " --choices {}".format( (essence_dir / name).with_suffix('.eprime')  )
        out_dir = Path(args.output) / name
        cpu_used = refine_times[name] + vals['total_cpu_time']
        total_time = min( max(cpu_used * 1.5, 10), total_time)

    total_time=int(total_time)
    cmd_str = cmd_str.format(essence_dir=essence_dir, kind=kind, status=status,
                       total_time=total_time, output=out_dir, db_dir=db_dir)

    if args.bin_dir:
        cmd_str += " --bin-dir {}".format(args.bin_dir)

    if args.cores:
        cmd_str += " --cores {}".format(args.cores)


    cmd_arr=shlex.split(cmd_str)
    try:
        subprocess.check_call(cmd_arr)
    except subprocess.CalledProcessError as e:
        print("error for {} - {} ".format(args.essence, name) )
        sys.exit(e.returncode)

def lookahead(iterable):
    it = iter(iterable)
    last = next(it)
    for val in it:
        yield last, False
        last = val
    yield last, True

if (essence_dir / "solve_eprime.json").exists():

    with (essence_dir / "refine_essence.json").open() as f:
        refine_times = { k:vs['cpu_time']  for (k,vs) in  json.load(f)['data_'].items()   }

    with (essence_dir / "solve_eprime.json").open() as f:
        data = json.load(f)

    for ((name, vals),is_last) in lookahead(data['data_'].items()):
        kind   = vals['last_kind']
        status = vals['last_status']
        process(status, kind, refinement=False, name=name,
                vals=vals, refine_times=refine_times,cmd_str=cmd_str,is_last=is_last)


elif (essence_dir / "refine_essence.json").exists():
    with (essence_dir / "refine_essence.json").open() as f:
        data=json.load(f)


    for ((name, vals),is_last) in lookahead(data['data_'].items()):
        kind   = vals['kind_']
        status = vals['status_']
        process(status, kind, refinement=True, name=name,
                vals=vals, refine_times=None, cmd_str=cmd_str, is_last=is_last)

else:
    print("no solve_eprime.json or refine_essence.json file found")
    sys.exit(3)

