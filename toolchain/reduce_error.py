#!/usr/bin/env python3
# Reduce a spec create by `gen`

# -*- coding: utf-8 -*-
import json
import sys
import shlex
import subprocess
# from toolchain_recheck import data_convert_back

import logging
import argparse
from pathlib import Path
logger = logging.getLogger(__name__)

parser = argparse.ArgumentParser()
parser.add_argument("essence", help='essence file')
parser.add_argument('-o',  dest='output',          help='output base dir ', required=True)
parser.add_argument('-p',  dest='total_time',      help='time per run', required=True)
parser.add_argument('-bin_dir',  dest='bin_dir',   help='--bin-dir' )
parser.add_argument('-c',  dest='cores',           help='cores to use, default 1' )


args = parser.parse_args()

essence = Path(args.essence)
essence_dir = essence.parent

if (essence_dir / "solve_eprime.json").exists():
	data = json.load(open(essence_dir / "solve_eprime.json"))
	raise NotImplementedError("solve_eprime.json not done yet")

elif (essence_dir / "refine_essence.json").exists():
	with (essence_dir / "refine_essence.json").open() as f:
	    data=json.load(f)


	for (names, vals) in data['data_'].items():
		kind   = vals['kind_']
		status = vals['status_']
		if status == "ErrorUnknown_":
			status = "StatusAny_"

		cmd_str="""
			gen reduce {essence_dir} -o '{output}' -p {total_time} -N --kind {kind} --status {status} -D
		""".format(essence_dir=essence_dir, kind=kind, status=status,
			       total_time=args.total_time, output=args.output)

		if 'choices_json' in vals['vals']:
			cs=Path(vals['vals']['choices_json']).name
			cmd_str += " --choices {}".format(essence_dir / cs)

		if args.bin_dir:
			cmd_str += " --bin-dir {}".format(args.bin_dir)

		if args.cores:
			cmd_str += " --cores {}".format(args.cores)


		cmd_arr=shlex.split(cmd_str)
		try:
			subprocess.check_call(cmd_arr)
		except subprocess.CalledProcessError as e:
			print("error")
			sys.exit(e.returncode)


else:
	print("no solve_eprime.json or refine_essence.json file found")
	sys.exit(3)

