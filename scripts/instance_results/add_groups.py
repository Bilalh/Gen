#!/usr/bin/env python3
#-*- coding: utf-8 -*-
import argparse
import csv
import shutil
import sys, os
import itertools

from pathlib import Path
from pprint import pprint, pformat
from collections import defaultdict

import logging
logger = logging.getLogger(__name__)


def setup():
  """ args and logging """
  parser = argparse.ArgumentParser(description="Add the given group ideas")
  parser.add_argument("--all",
                      default="all.csv",
                      metavar="CSV",
                      help="default: %(default)s")
  parser.add_argument("--all_models",
                      default="all_models.csv",
                      metavar="CSV",
                      help="default: %(default)s")
  args = parser.parse_args()

  logger_level = logging.INFO
  logger_format = '%(name)s:%(lineno)d:%(funcName)s: %(message)s'
  logging.basicConfig(format=logger_format, level=logger_level)

  return args


def hash_row(row, *, compare):
  """ Make hashable using the given fields """
  return frozenset((k, v) for (k, v) in sorted(row.items()) if k in compare)


def read_csv(csvfile):
  """ Read the csv and do type conversions """
  with csvfile.open('r') as f:
    r = csv.DictReader(f)

    def fix(v):
      if v == "":
        return None

      try:
        return int(v)
      except ValueError:
        return v

    rows = [{k: fix(v) for (k, v) in row.items()} for row in r]
    return (rows, r.fieldnames)


def write_csv(csvfile, *, headers, rows):
  with csvfile.open('w') as f:
    w = csv.DictWriter(f, headers)
    w.writeheader()
    for d in rows:
      w.writerow(d)


args = setup()
csvfile = Path(args.all)
allModels_csv = Path(args.all_models)

(rows, fieldnames) = read_csv(csvfile)
run_fields = ["essenceClass", "mode", "iterations", "per_model_time_given",
              "use_all_solutions", "influence_radius", "num_models", "race_time_given",
              "paramsUsedHash"]

# The seq where the param was generated
base_group = {row['paramsUsedHash']: row['seq'] for row in rows if row['isGiven'] == 0}
base_group_hashes = {
    hash_row(row,
             compare=run_fields): row['seq']
    for row in rows if row['isGiven'] == 0
}
# for (k, v) in base_group_hashes.items():
#   pprint((v, k))

givens = defaultdict(set)
for row in rows:
  if row['isGiven'] == 1:
    givens[row['paramsUsedHash']].add(row['seq'])

# seq -> paramsUsedHash of base_group if it exists
param_groups = dict(itertools.chain(*[[(seq, base_group[k]) for seq in vs] for k, vs in
                                      givens.items() if k in base_group]))

# str(seq) -> seq
# to make process the large all_models.csv faster
# and defaultdict is not written in older versions of py3
givenRunGroupMap = {}
strParamGroup = {}

for row in rows:
  try:
    row['paramGroup'] = param_groups[row['seq']]
    strParamGroup[str(row['seq'])] = row['paramGroup']
  except KeyError:
    pass
  if row['isGiven'] == 1:
    try:
      row['givenRunGroup'] = base_group_hashes[hash_row(row, compare=run_fields)]
      givenRunGroupMap[str(row['seq'])] = row['givenRunGroup']
    except KeyError:
      pass

tmpPath = csvfile.with_suffix(".tmp.csv")
if tmpPath.exists():
  tmpPath.unlink()

write_csv(tmpPath, headers=fieldnames, rows=rows)
tmpPath.replace(csvfile)

# Stream all_models.csv since it so large
tmpPath2 = allModels_csv.with_suffix(".tmp.csv")
if tmpPath2.exists():
  tmpPath2.unlink()

with allModels_csv.open('r') as fin:
  with tmpPath2.open('w') as fout:
    r = csv.DictReader(fin)
    w = csv.DictWriter(fout, r.fieldnames)
    w.writeheader()
    for row in r:
      try:
        row['paramGroup'] = strParamGroup[row['seq']]
      except KeyError:
        pass
      try:
        row['givenRunGroup'] = givenRunGroupMap[row['seq']]
      except KeyError:
        pass

      w.writerow(row)

tmpPath2.replace(allModels_csv)
