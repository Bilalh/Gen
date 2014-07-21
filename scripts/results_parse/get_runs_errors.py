#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse
import json
import logging
import shutil

from pprint import pprint
from pathlib import Path

logger = logging.getLogger(__name__)
parser = argparse.ArgumentParser()

parser.add_argument("json", help='state.json')
parser.add_argument("outdir", help='')

args = parser.parse_args()
json = json.load(open(args.json))
outdir = Path(args.outdir)


def process_refine_error(refine):
    resdir = Path(refine['outdir_'])

    for k,vs in refine['data_'].items():
        out = outdir / "gErrorsRefine"
        if vs['rcode'] == 251:
            out = out /  "HeapSpace_" / resdir.name
        elif vs['status_'] != 'Success_':
            out = out / vs['status_'] / resdir.name
        else:
            continue

        if not out.exists():
            shutil.copytree(str(resdir), str(out) )


def process_inconsistent(errors):
    resdir = Path(errors['outdir_'])
    out = outdir / "gInconsistent" / resdir.name

    if not out.exists():
        shutil.copytree(str(resdir), str(out) )


cmd_names = [ "refineParam", "savilerow", "translateSolution", "validateSolution" ]
def process_solve_errors(errors):
    resdir = Path(errors['outdir_'])
    outbase = outdir /  "gErrorsSolve"

    for k, vs in errors['data_'].items():
        if not vs['erroed']:
            continue

        out = outbase /  cmd_names[ vs['erroed'] ] / (resdir.name +  "_" + k )
        cmd_err = vs['results'][vs['erroed']]

        if not out.exists():
            out.mkdir(parents=True)

        for glob in [ k + '*', '_*', '*.json', '*.essence']:
            for f in resdir.glob(glob):
                shutil.copy(str(f), str(out))


for e in json['gErrorsRefine']:
    process_refine_error(e)

for e in json['gInconsistent']:
    process_inconsistent(e)

for e in json['gErrorsSolve']:
    process_solve_errors(e)


