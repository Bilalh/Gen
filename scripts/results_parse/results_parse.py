#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Creates CSVs with data on all runs of the experiments

import argparse
import csv
import logging
import sqlite3
import itertools
import json

from pprint import pprint
from pathlib import Path

from multiprocessing import Pool

logger = logging.getLogger(__name__)
parser = argparse.ArgumentParser()

parser.add_argument("start_dir", help='Top level dir with results folder in it at any level')
parser.add_argument("output_dir", help='Where to put the csvs')
args = parser.parse_args()

# Get all runs from all subdirectories

start_dir = Path(args.start_dir)
output = Path(args.output_dir)
# start_dir=Path('/Users/bilalh/Desktop/Experiment/main')
# output = Path('/Users/bilalh/Desktop/Experiment/main') / "_output

if not output.exists():
    output.mkdir()

q_rows = []
for basedir in start_dir.glob('**/results'):
    pbase = basedir.parent

    # Get finished runs
    with sqlite3.connect(str(basedir / "Info.db")) as conn:
        conn.row_factory = sqlite3.Row
        q_rows += [ (pbase, dict(row)) for row in conn.execute("SELECT * FROM everything") ]


param_eprime_results_sql="""
Select * From
(
Select count(paramHash) as paramC, eprimesLeft as eprimeC,
CASE When quality == 1 Then "" Else eprimes END as eprimes,
num_models, quality,
min(ordering) as minOrdering, avg(ordering) as avgOrdering,
avg(P.avgTime) as avgAvgTime,
group_concat(paramHash, ", ") as params
From ParamsData P Join (Select count(*) as num_models from EprimeOrdering)
Group by eprimes
)


Order by paramC Desc, eprimeC, eprimes;
"""


def process_row(basedir_row):
    (basedir, row) = basedir_row
    run_dir = Path(row['output_dir'])

    json_file = basedir / run_dir / "times.json"
    times = json.load(json_file.open())
    row.update(times)

    with sqlite3.connect(str( basedir / run_dir / "results.db")) as conn:
        conn.row_factory = sqlite3.Row
        p_rows = [ dict(row) for row in conn.execute("SELECT * FROM ParamsData") ]
        pe_rows = [ dict(row) for row in conn.execute(param_eprime_results_sql) ]


    for rrow in p_rows:
        for field in ['essence', 'mode', 'method', 'num_models']:
            rrow[field] = row[field]

        for field in ['uuid', 'id', 'gid', 'guuid']:
            rrow['run_' + field] = row[field]

        del rrow['param']

    for rrow in pe_rows:
        for field in ['essence', 'mode', 'method', 'num_models']:
            rrow[field] = row[field]

        for field in ['uuid', 'id', 'gid', 'guuid']:
            rrow['run_' + field] = row[field]

    return (row, p_rows, pe_rows)


pool = Pool(processes=4)
results = (pool.map(process_row, q_rows))
# results = list(map(process_row, q_rows[1:4] ))

[runs, instances, summary] = zip(*results)


def write_dicts_as_csv(fp, rows, keys):
    with ( fp ).open("w") as f:
        keys = list(keys)

        extra_keys=['paramHash', 'eprimes', 'params', 'output_dir', 'uuid', 'guuid', 'gid', 'id','param']
        time_keys = ["cpu_time_formatted", "method_cpu_time_formatted", "method_extra_time_formatted",
        "real_time_formatted", "iterations_done", "iterations_done_including_failed"]
        time_keys2 = ["cpu_time","method_cpu_time", "method_extra_time", "real_time",
        "date_end", "date_start", "time_stamp_start", "time_stamp_end"]

        for e in time_keys + extra_keys + time_keys2:
            if e in keys:
                keys.remove(e)
                keys.append(e)

            e2 = 'run_' + e
            if e2 in keys:
                keys.remove(e2)
                keys.append(e2)

        csv_writer = csv.DictWriter(f, delimiter=',', fieldnames=keys)
        csv_writer.writeheader()
        for r in rows:
            csv_writer.writerow(r)

write_dicts_as_csv(output / "runs.csv",
    runs, runs[0].keys())
write_dicts_as_csv(output / "instances.csv",
    itertools.chain.from_iterable(instances), instances[0][0].keys())
write_dicts_as_csv(output / "summary.csv",
    itertools.chain.from_iterable(summary), summary[0][0].keys())

