#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import csv
from pathlib import Path

parser = argparse.ArgumentParser(prog='didMinionTimeout.py')
parser.add_argument("minionTableout", help='from minion -tableout named {eprime}-{param}')
args = parser.parse_args()

with Path(args.minionTableout).open("r") as f:
    reader = csv.DictReader(f, skipinitialspace=True, delimiter=' ')
    rows = list(reader)[0]
    print(int(rows['TimeOut']))
