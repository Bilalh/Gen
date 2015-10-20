#!/usr/bin/env python3
from pathlib import Path
from pprint import pprint

solutions_path = Path("/Users/bilalh/Desktop/all_solutions/all_sols")
with (solutions_path / "solutions.counts").open() as f:
    parts = [line.strip().split(' ') for line in f.readlines()]
    solutions_count = 0
    for (i, (v, _)) in enumerate(parts):
        parts[i][0] = solutions_count
        solutions_count += int(v)

    parts.append([solutions_count, parts[-1][1]])
    pprint(parts)
