#!/usr/bin/env python3
#-*- coding: utf-8 -*-
import argparse
import csv
import shutil
import sys, os
import itertools
import json

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


args = setup()
csvfile = Path(args.all)
allModels_csv = Path(args.all_models)

ignore_json = Path("ignore.json")
if ignore_json.exists():
  ignore = json.load(ignore_json.open("r"))
  ignore_str = {str(i) for i in ignore}
else:
  ignore = {}
  ignore_str = {}

(rows, fieldnames) = read_csv(csvfile)
if 'group2' not in fieldnames:
  fieldnames.append('group2')

run_fields = ["essenceClass", "mode", "iterations", "per_model_time_given",
              "use_all_solutions", "influence_radius", "num_models", "race_time_given",
              "paramsUsedHash"]

group2_fields = ["essence", 'essence_name', "mode", "iterations", "kindClass",
                 "per_model_time_given", "use_all_solutions", "influence_radius",
                 "num_models", "race_time_given"]

group2_id = 1

# groups2 compares on kindClass instead of kind
groups2 = {}
strSeqGroup2 = {}

for row in rows:
  try:
    id = row['group2']
    strSeqGroup2[str(row['seq'])] = id
    group2_id = max(group2_id, id)
    hr = hash_row(row, compare=group2_fields)
    groups2[hr] = id
  except KeyError as e:
    pass

for row in rows:
  hr = hash_row(row, compare=group2_fields)
  try:
    id = groups2[hr]
  except KeyError:
    id = group2_id
    group2_id += 1
    groups2[hr] = id

  strSeqGroup2[str(row['seq'])] = id
  row['group2'] = id

# The seq where the param was generated
base_group = {row['paramsUsedHash']: row['seq'] for row in rows if row['isGiven'] == 0}
base_group_hashes = {
    hash_row(row,
             compare=run_fields): row['seq']
    for row in rows if row['isGiven'] == 0
}

givens = defaultdict(set)
for row in rows:
  if row['isGiven'] == 1:
    givens[row['paramsUsedHash']].add(row['seq'])

# seq -> paramsUsedHash of base_group if it exists
param_groups = dict(itertools.chain(*[[(seq, base_group[k]) for seq in vs] for k, vs in
                                      givens.items() if k in base_group]))

param_groups.update({v: v for v in param_groups.values()})

# Only add the givenRunGroup if acually have a group of runs
base_givenRunGroup_ids = set()

# str(seq) -> seq
# to make process the large all_models.csv faster
# and defaultdict is not written in C in older versions of py3
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
      givenRunGroupMap[str(row['givenRunGroup'])] = row['givenRunGroup']
      base_givenRunGroup_ids.add(row['givenRunGroup'])
    except KeyError:
      pass

for row in rows:
  if row['seq'] in base_givenRunGroup_ids:
    row['givenRunGroup'] = row['seq']

tmpPath = csvfile.with_suffix(".tmp.csv")
if tmpPath.exists():
  tmpPath.unlink()

with tmpPath.open('w') as f:
  w = csv.DictWriter(f, fieldnames)
  w.writeheader()
  for d in rows:
    if d["seq"] not in ignore:
      w.writerow(d)

tmpPath.replace(csvfile)

# Stream all_models.csv since it so large
tmpPath2 = allModels_csv.with_suffix(".tmp.csv")
if tmpPath2.exists():
  tmpPath2.unlink()

with allModels_csv.open('r') as fin:
  with tmpPath2.open('w') as fout:
    r = csv.DictReader(fin)
    names = r.fieldnames
    if 'group2' not in names:
      names.append('group2')

    w = csv.DictWriter(fout, r.fieldnames + ["group2"])
    w.writeheader()
    for row in r:
      if row['seq'] in ignore_str:
        continue

      try:
        row['paramGroup'] = strParamGroup[row['seq']]
      except KeyError:
        pass
      try:
        row['givenRunGroup'] = givenRunGroupMap[row['seq']]
      except KeyError:
        pass
      try:
        row['group2'] = strSeqGroup2[row['seq']]
      except KeyError:
        pprint("group2 not found for seq {} ".format(row['seq']), stream=sys.stderr)
        pprint(row['seq'])
        pprint(strSeqGroup2)
        pprint(row)
        raise

      w.writerow(row)

tmpPath2.replace(allModels_csv)
