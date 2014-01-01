#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain
from pathlib import Path
import sqlite3
import csv
import argparse


def collect_data_as_dicts(base_str):

    base = Path(base_str)
    info_db = base / "results" / "Info.db"

    conn = sqlite3.connect(str(info_db))
    conn.row_factory = sqlite3.Row

    num_models = { row['essence']: row['num_models'] for row in conn.execute("Select * from essences")  }
    rows = []
    for row in conn.execute("SELECT * FROM everything"):
            results_db = base / row['output_dir'] / "results.db"
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

    return rows


def write_dicts_as_csv(fp, rows):
    with ( Path(fp) ).open("w") as f:
        csv_writer = csv.DictWriter(f, delimiter=',', fieldnames=sorted(rows[0].keys()))
        csv_writer.writeheader()
        for r in rows:
            csv_writer.writerow(r)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("base_dir")
    args = parser.parse_args()
    rows = collect_data_as_dicts(args.base_dir)
    csv_fp = Path(args.base_dir) / "results" / "info.csv"
    write_dicts_as_csv(csv_fp, rows)


