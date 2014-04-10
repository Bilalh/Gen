#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# does hitting_set_gent.txt

from pprint import pprint
import argparse
import sqlite3

import logging
logger = logging.getLogger(__name__)
logging.basicConfig(format='%(lineno)d:%(funcName)s: %(message)s', level=logging.ERROR)


parser = argparse.ArgumentParser(prog='gent_idea')
parser.add_argument("db", help='results.db')
parser.add_argument("hitting_set", help='form {1,2,4,5}')

args = parser.parse_args()
logger.info(args)

str_set = args.hitting_set
if str_set == '{}':
	str_set='set()'

hset = eval(str_set)

insection_mapping = {}

with sqlite3.connect(args.db) as conn:
	for e in hset:
		logging.info("processing %s", e)
		sets_with_e =[ set(eprimes.split(", ")) for (eprimes,) in
			conn.execute("Select eprimes From ParamsData Where eprimes like '%{:04d}%' ".format(e))]

		assert len(sets_with_e) > 0
		logger.info(len(sets_with_e))
		intersection = sets_with_e[0]

		if len(sets_with_e) > 1:
			for set_e in sets_with_e[1:]:
				intersection &= set_e

		logger.info("intersection: %s", intersection)
		insection_mapping[e] = intersection

for intersection in insection_mapping.values():
	print(intersection)