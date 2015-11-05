#!/usr/bin/env python3
#-*- coding: utf-8 -*-
import logging
import argparse
import json
import sqlite3
import os

from pprint import pprint
from pathlib import Path

logger = logging.getLogger(__name__)

parser = argparse.ArgumentParser(description="Output the no-chan models' ids")
parser.add_argument("db", help='')
parser.add_argument("json", help='')
args = parser.parse_args()

data = json.load(open(os.path.expanduser(args.json)))

query = 'Select eprimeId from Eprimes where eprime in ({})'.format(', '.join('?' for _ in data['no_chan']))
with sqlite3.connect(os.path.expanduser(args.db)) as conn:
	res = conn.execute(query, [ Path(ep).stem for ep in data['no_chan'] ] )

print("no-chan is {}".format(", ".join( str(x[0]) for x in res)))