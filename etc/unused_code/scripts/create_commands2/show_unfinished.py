#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import logging
import argparse
import shutil

from pprint import pprint
from pathlib import Path

logger = logging.getLogger(__name__)

parser = argparse.ArgumentParser()
parser.add_argument('-f',  action='store_true', dest='delete', help="Delete unfinished")
args = parser.parse_args()


for out_dir in Path("results").glob("*/out_*"):
	if not (out_dir / "times.json").exists():
		print("unfinished: {}".format(out_dir))
		if args.delete:
			print("deleting: {}".format(out_dir))
			shutil.rmtree(str(out_dir))