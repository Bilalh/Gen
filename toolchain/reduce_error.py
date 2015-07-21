#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Reduce a spec create by `gen`

import argparse
import json
import logging
import shlex
import subprocess
import sys
import eprime_to_choices
import datetime

from pathlib import Path

logger = logging.getLogger(__name__)

parser = argparse.ArgumentParser()
parser.add_argument("essence", help='essence file')
parser.add_argument('-o', dest='output', help='output base dir ', required=True)
parser.add_argument('-p', dest='spec_time', help='time per run', required=True, type=int)
parser.add_argument('-t', dest='total_time', help='Total real time', type=int)
parser.add_argument('-bin_dir', dest='bin_dir', help='--bin-dir')
parser.add_argument('-c', dest='cores', help='cores to use, default 1', type=int)
parser.add_argument('--passing_db', help='for gen reduce --db-passing-in')
parser.add_argument('--temp_db', help='for gen reduce --db-passing-in', required=True)

args = parser.parse_args()

essence = Path(args.essence)
essence_dir = essence.parent

cmd_str = """
     gen reduce {essence_dir} -o '{output}' -p {spec_time}  --kind {kind} --status {status} --delete-steps --delete-others -N  --db-dir '{db_dir}'
"""

if args.passing_db:
    cmd_str += " --db-passing-in '{}'".format(args.passing_db)

if args.total_time is not None:
    cmd_str += " --total-time '{}'".format(args.total_time)

logger.warn("args %s", args)


def process(status, kind, refinement, name, vals, refine_times, cmd_str, is_last):
    if is_last:
        cmd_str += " --db-only-passing "

    if status == "ErrorUnknown_":
        status = "StatusAny_"

    spec_time = args.spec_time
    db_dir = args.temp_db

    ts = int(datetime.datetime.now().timestamp())
    if refinement:
        if 'choices_json' in vals['vals']:
            cs = Path(vals['vals']['choices_json']).name
            choices = essence_dir / cs

            if choices.suffixes == [".choices", ".json"]:
                new_name = choices.name.replace(".choices.json", ".choices-new.json")
                if (essence_dir / new_name).exists():
                    choices = essence_dir / new_name
            out_dir = Path(args.output) / "{}_r-.{}@{}".format(
                essence_dir.name, choices.stem.replace('.choices-new', '.choices'), ts)
            cmd_str += " --choices {}".format(choices)
        else:
            out_dir = Path(args.output) / ("all_" + ts)

        cpu_used = vals['cpu_time']
        spec_time = min(max(cpu_used * 2, 20), spec_time)

    else:
        eprime = (essence_dir / name).with_suffix('.eprime')
        eprime_to_choices.main(eprime, update=True)

        cmd_str += " --choices {}".format(
            (essence_dir / name).with_suffix('.choices-eprime.json'))

        out_dir = Path(args.output) / "{}_r-.{}.choices@{}".format(essence_dir.name, name,
                                                                   ts)

        cpu_used = refine_times[name] + vals['total_cpu_time']
        spec_time = min(max(cpu_used * 2, 20), spec_time)

    spec_time = int(spec_time)
    cmd_str = cmd_str.format(essence_dir=essence_dir,
                             kind=kind,
                             status=status,
                             spec_time=spec_time,
                             output=out_dir,
                             db_dir=db_dir)

    if args.bin_dir:
        cmd_str += " --bin-dir {}".format(args.bin_dir)

    if args.cores:
        cmd_str += " --cores {}".format(args.cores)

    cmd_arr = shlex.split(cmd_str)
    print("running {}".format(cmd_str))
    try:
        subprocess.check_call(cmd_arr)
    except subprocess.CalledProcessError as e:
        print("error for {} - {} ".format(args.essence, name))
        sys.exit(e.returncode)


def lookahead(iterable):
    it = iter(iterable)
    last = next(it)
    for val in it:
        yield last, False
        last = val
    yield last, True


kind = None
status = None

if (essence_dir / "dir_error.json").exists():
    with (essence_dir / "dir_error.json").open() as f:
        data = json.load(f)
        kind = data['kind']
        status = data['status']

if (essence_dir / "solve_eprime.json").exists():
    with (essence_dir / "refine_essence.json").open() as f:
        refine_times = {k: vs['cpu_time'] for (k, vs) in json.load(f)['data_'].items()}

    with (essence_dir / "solve_eprime.json").open() as f:
        data = json.load(f)

    for ((name, vals), is_last) in lookahead(data['data_'].items()):
        if not kind:
            kind_ = vals['last_kind']
        else:
            kind_ = kind
        if not status:
            status_ = vals['last_status']
        else:
            status_ = status
        process(status_, kind_,
                refinement=False,
                name=name,
                vals=vals,
                refine_times=refine_times,
                cmd_str=cmd_str,
                is_last=is_last)

elif (essence_dir / "refine_essence.json").exists():
    with (essence_dir / "refine_essence.json").open() as f:
        data = json.load(f)

    for ((name, vals), is_last) in lookahead(data['data_'].items()):
        if not kind:
            kind_ = vals['kind_']
        else:
            kind_ = kind
        if not status:
            status_ = vals['status_']
        else:
            status_ = status
        process(status_, kind_,
                refinement=True,
                name=name,
                vals=vals,
                refine_times=None,
                cmd_str=cmd_str,
                is_last=is_last)

else:
    print("no solve_eprime.json or refine_essence.json file found")
    sys.exit(3)
