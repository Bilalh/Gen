#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain
from pathlib import Path
import sqlite3
import csv
import argparse

import multiprocessing
from multiprocessing import Queue
import math
import itertools as it

from pprint import pprint

param_eprime_results_sql="""
Select * From
(
Select count(param) as paramC, eprimes_count as eprimeC,
CASE When quality == 1 Then "" Else eprimes END as eprimes,
num_models, quality, satisfiable, maxSolutions as max_solutions, min(ordering) as min_ordering,
group_concat(param, ", ") as params
From ParamsData P Join (Select count(*) as num_models from EprimeOrdering)
Group by eprimes
)


Order by paramC Desc, eprimeC, eprimes
"""

every_param_sql ="""
Select * from ParamsData
"""


def collect_data_as_dicts(base_str, num_proc, all_results_dir, start_num=0):

    base = Path(base_str)
    info_db = base / "results" / "Info.db"

    conn = sqlite3.connect(str(info_db))
    conn.row_factory = sqlite3.Row

    num_models = { row['essence']: row['num_models'] for row in conn.execute("Select * from essences")  }

    def worker(rows_in, indexes, out_q):
        indexes = list(indexes)
        rows = []

        def ff(cr, ix):
            d = dict(cr)
            d['index'] = ix
            return d

        for (i, row) in enumerate(rows_in):
            results_db = base / row['output_dir'] / "results.db"

            # if we have a partial result ignore the the result
            if row['method'] != 'smac' and not (base / row['output_dir'] / "info" / "data-points.json").exists():
                continue

            results_conn = sqlite3.connect(str(results_db))
            quality_row = results_conn.execute("SELECT min(quality) FROM  DiscriminatingParams Limit 1")

            quality = list(quality_row)[0][0]
            discriminating_count_row = results_conn.execute("SELECT count(quality) FROM  DiscriminatingParams")
            discriminating_count = list(discriminating_count_row)[0][0]


            param_count_row = results_conn.execute("SELECT count(*) FROM  ParamQuality")
            param_count = list(param_count_row)[0][0]

            row_data = dict(row)
            row_data['best_quality'] = quality
            row_data['max_discriminating'] = discriminating_count
            row_data['param_count'] = param_count
            row_data['num_models'] = num_models[row['essence']]
            row_data['index'] = indexes[i]

            if base.name != 'Experiment':
                row_data['output_dir'] = str(Path(base.name) / row_data['output_dir'])

            if 'use_minion' not in row_data['output_dir']:
                row_data['use_minion'] = int(bool(base.name.endswith('minion')))

            results_conn.row_factory = sqlite3.Row
            param_eprime_info_rows = results_conn.execute(param_eprime_results_sql)
            param_eprime_info_rows = list(param_eprime_info_rows)

            every_param_rows = results_conn.execute(every_param_sql)
            every_param_rows = list(every_param_rows)


            rows.append((
                row_data,
                [ ff(cr, row_data['index']) for cr in param_eprime_info_rows ],
                [ ff(cr, row_data['index']) for cr in every_param_rows ] ))

        out_q.put(rows )

    q_rows = list(conn.execute("SELECT * FROM everything"))
    q_out = Queue()
    chunksize = int(math.ceil(len(q_rows) / num_proc))
    print("num_proc chunksize len(q_rows) for basedir", num_proc, chunksize,  len(q_rows), base_str)
    procs = []

    for i in range(num_proc):
        p = multiprocessing.Process(
                target=worker,
                args=(q_rows[chunksize * i:chunksize * (i + 1)],
                      range(start_num + chunksize * i, start_num + chunksize * (i + 1)),
                      q_out))
        procs.append(p)
        p.start()

    results_rows = list( q_out.get() for i in range(num_proc) )
    # print(len(results_rows))
    # for (i, rs) in enumerate(results_rows):
    #     print(i, len(rs), type(rs))
    #     (a, b) =list(zip(*rs))
    #     print(len(a), len(b))
    #     print("~~~")
    #     print("")

    # too many stars to just join the rows then unzip them

    (results, params_info, all_params) = zip(*[ list(zip(*rs)) for rs in results_rows if rs])

    results = list(it.chain(*results))
    params_info = list(it.chain(*it.chain(*params_info)))
    all_params = list(it.chain(*it.chain(*all_params)))

    return (results, params_info, all_params)


# I don't really need the keys since I can get them from the dict
def write_dicts_as_csv(fp, rows):
    with ( Path(fp) ).open("w") as f:
        keys = sorted(rows[0].keys())

        for e in ['param', 'eprimes', 'params', 'output_dir']:
            if e in keys:
                keys.remove(e)
                keys.append(e)

        csv_writer = csv.DictWriter(f, delimiter=',', fieldnames=keys)
        csv_writer.writeheader()
        for r in rows:
            csv_writer.writerow(r)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("num_proc", type=int)
    parser.add_argument("all_results_fp")
    parser.add_argument("base_dirs", nargs='+')
    args = parser.parse_args()
    pprint(args)
    all_rows = []
    all_param_info = []
    every_param_data =[]
    start_num = 0
    all_results_dir = Path(args.all_results_fp).parent / 'extra_data'

    import os
    os.makedirs(str(all_results_dir), exist_ok=True)

    for base_dir in args.base_dirs:
        (rows, param_info, every_param) = collect_data_as_dicts(base_dir, args.num_proc, all_results_dir, start_num)

        print("____", len(rows), len(param_info) )
        csv_fp = Path(base_dir) / "results" / "info.csv"
        write_dicts_as_csv(csv_fp, rows)
        all_rows += rows

        start_num += len(rows)
        all_param_info += param_info
        every_param_data += every_param

    write_dicts_as_csv(args.all_results_fp, all_rows)
    write_dicts_as_csv(all_results_dir / 'param_eprime_info.csv', all_param_info)
    write_dicts_as_csv(all_results_dir / 'every_param.csv', every_param_data)