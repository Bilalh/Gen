#!/usr/bin/env python3
#-*- coding: utf-8 -*-
import logging
import argparse
import json

# from pprint import pprint
from pathlib import Path

logger = logging.getLogger(__name__)

parser = argparse.ArgumentParser()
parser.add_argument("eprime")
parser.add_argument('--update',  action='store_true',help='Update old choices')
args = parser.parse_args()

eprime = Path(args.eprime)
with eprime.open("r") as f:
    json_lines=[  line[1:] for line in f  if line[0]=='$' ]

data=json.loads(" ".join(json_lines[1:]))
choices=data['questionAnswered']

def process_inner(cur):
    if isinstance(cur,list):
        return [ process_inner(l) for l in cur ]
    elif isinstance(cur,dict):
        if "HasRepresentation" in cur:
            a=cur['HasRepresentation']
            if isinstance(a,str):
                cur['HasRepresentation'] = { "Name" : a }
            return cur
        else:
            return  { k:process_inner(v) for (k,v) in cur.items()   }

    else:
        return cur


def process_json(cur):
    if not args.update:
        return cur
    if "AnsweredRepr" not in cur:
        return cur

    return process_inner(cur)

new_choices = map(process_json, choices)

with (eprime.with_suffix('.choices-eprime.json')).open('w') as f:
    f.write("[  ")
    f.write( "\n,  ".join(json.dumps(c)  for c in new_choices) )
    f.write("\n]")
