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

def collect_data_as_dicts(base_str, num_proc):

    base = Path(base_str)
    info_db = base / "results" / "Info.db"

    conn = sqlite3.connect(str(info_db))
    conn.row_factory = sqlite3.Row

    num_models = { row['essence']: row['num_models'] for row in conn.execute("Select * from essences")  }
    
    def worker(rows_in, out_q):
        rows = []
        for row in rows_in:
            results_db = base / row['output_dir'] / "results.db"
            
            # if we have a partial result ignore the the result
            if row['method'] != 'smac' and  not (base / row['output_dir'] / "info" / "data-points.json").exists():
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
            rows.append(row_data)
        out_q.put(rows)
    
    q_rows = list(conn.execute("SELECT * FROM everything"))
    q_out = Queue()
    chunksize = int(math.ceil(len(q_rows) / num_proc))
    print("num_proc chunksize len(q_rows) for basedir", num_proc, chunksize,  len(q_rows), base_str)
    procs = []
    
    for i in range(num_proc):
        p = multiprocessing.Process(
                target=worker,
                args=(q_rows[chunksize * i:chunksize * (i + 1)],
                      q_out))
        procs.append(p)
        p.start()
    
    results_rows = it.chain( *(q_out.get()  for i in range(num_proc)) )

    keys = q_rows[0].keys() + ['quality', 'discriminating', 'num_models']

    return (list(results_rows), keys)


def write_dicts_as_csv(fp, rows, keys):
    with ( Path(fp) ).open("w") as f:
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
    for base_dir in args.base_dirs:
        (rows, keys) = collect_data_as_dicts(base_dir, args.num_proc)
        csv_fp = Path(base_dir) / "results" / "info.csv"
        write_dicts_as_csv(csv_fp, rows, keys)
        all_rows += rows

    write_dicts_as_csv(args.all_results_fp, all_rows, keys)

