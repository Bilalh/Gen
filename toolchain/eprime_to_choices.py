#!/usr/bin/env python3
#-*- coding: utf-8 -*-
import json
import logging

from pathlib import Path

logger = logging.getLogger(__name__)


def process_inner(cur):
    if isinstance(cur, list):
        return [process_inner(l) for l in cur]
    elif isinstance(cur, dict):
        if "HasRepresentation" in cur:
            a = cur['HasRepresentation']
            if isinstance(a, str):
                cur['HasRepresentation'] = {"Name": a}
            return cur
        else:
            return {k: process_inner(v) for (k, v) in cur.items()}

    else:
        return cur


def process_json(update, cur):
    if not update:
        return cur
    if "AnsweredRepr" not in cur:
        return cur

    return process_inner(cur)


def main(eprime, *, update):
    eprime = Path(eprime)
    with eprime.open("r") as f:
        json_lines = [line[1:] for line in f if line[0] == '$']

    data = json.loads(" ".join(json_lines[1:]))
    choices = data['questionAnswered']

    new_choices = [process_json(update, c) for c in choices]

    with (eprime.with_suffix('.choices-eprime.json')).open('w') as f:
        f.write("[  ")
        f.write("\n,  ".join(json.dumps(c) for c in new_choices))
        f.write("\n]")


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("eprime")
    parser.add_argument('--update', action='store_true', help='Update old choices')
    args = parser.parse_args()
    main(args.eprime, update=args.update)
