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
Select count(param) as 'param#',eprimes_num as 'eprime#',  eprimes, group_concat(param, ", ") as params
From (
    Select P.param, count(D.eprime) as eprimes_num  , group_concat(D.eprime, ", ") as eprimes, quality
    From ParamQuality P
    Join TimingsDomination D on P.param = D.param
    Where P.Quality < 1 and isDominated = 0
    Group by P.param

    Union

    Select Q.param, 0 as eprimes_num, "" as eprimes, 100 as quality
    From ParamQuality Q
    Where Q.param not in (Select D.param From DiscriminatingParams D)

    Order by quality

)

Group by eprimes
Order by eprimes_num,count(param), eprimes;
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

        def ff(cr):
            d = dict(cr)
            d['index'] = row_data['index']
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

            row_data = dict(row)
            row_data['quality'] = quality
            row_data['discriminating'] = discriminating_count
            row_data['num_models'] = num_models[row['essence']]
            row_data['index'] = indexes[i]

            if base.name == 'div_cores':
                row_data['output_dir'] = str(Path("div_cores") / row_data['output_dir'])


            results_conn.row_factory = sqlite3.Row
            param_eprime_info_rows = results_conn.execute(param_eprime_results_sql)
            param_eprime_info_rows = list(param_eprime_info_rows)

            rows.append((row_data, [ ff(cr) for cr in param_eprime_info_rows ]))

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

    # maybe too many stars to  join the rows then unzip them
    results_rows = list( q_out.get() for i in range(num_proc) )
    print(len(results_rows))
    for (i, rs) in enumerate(results_rows):
        print(i, len(rs), type(rs))
        (a, b) =list(zip(*rs))
        print(len(a), len(b))
        print("~~~")
        print("")

    (results, params_info) = zip(*[ list(zip(*rs)) for rs in results_rows ])
    print(len(results), len(params_info))

    results = list(it.chain(*results))
    params_info = list(it.chain(*it.chain(*params_info)))
    print(len(results), len(params_info))


    return (results, params_info)


# I don't really need the keys since I can get them from the dict
def write_dicts_as_csv(fp, rows):
    with ( Path(fp) ).open("w") as f:
        keys = sorted(rows[0].keys())
        csv_writer = csv.DictWriter(f, delimiter=',', fieldnames=sorted(keys))
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
    start_num = 0
    all_results_dir = Path(args.all_results_fp).parent / 'extra_data'

    import os
    os.makedirs(str(all_results_dir), exist_ok=True)

    for base_dir in args.base_dirs:
        (rows, param_info) = collect_data_as_dicts(base_dir, args.num_proc, all_results_dir, start_num)
        pprint(rows[1:4])
        pprint(param_info[1:4])

        print("____", len(rows), len(param_info) )
        csv_fp = Path(base_dir) / "results" / "info.csv"
        write_dicts_as_csv(csv_fp, rows)
        all_rows += rows

        start_num += len(rows)
        all_param_info += param_info

    write_dicts_as_csv(args.all_results_fp, all_rows)
    write_dicts_as_csv(all_results_dir / 'param_eprime_info.csv', all_param_info)

