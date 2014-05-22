#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse
import csv
import logging
import shutil
import sqlite3
import itertools

from pprint import pprint
from pathlib import Path

from multiprocessing import Pool

logger = logging.getLogger(__name__)
parser = argparse.ArgumentParser()

# parser.add_argument("db", help='')
# parser.add_argument('-f',  action='store_true', dest='delete', help='')
# parser.add_argument('-p','--plays',  dest='selected', action='store_const', const='plays',   help='')

args = parser.parse_args()

basedir=Path('/Users/bilalh/Desktop/Experiment/main/knapsack')
output = basedir / "_output"
if not output.exists():
    output.mkdir()

# Get finished runs
with sqlite3.connect(str(basedir / "results" / "Info.db")) as conn:
    conn.row_factory = sqlite3.Row
    q_rows = [ dict(row) for row in conn.execute("SELECT * FROM everything") ]


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

def process_row(row):
    run_dir = Path(row['output_dir'])

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

        for e in ['paramHash', 'eprimes', 'params', 'output_dir', 'uuid', 'guuid', 'gid', 'id','param']:
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

