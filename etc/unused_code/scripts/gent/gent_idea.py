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

highest_ordering_needed=-1

with sqlite3.connect(args.db) as conn:
	for e in hset:
		logging.info("processing %s", e)
		sets_with_e =[ (set(eprimes.split(", ")), ordering) for (eprimes, ordering) in
			conn.execute("Select eprimesIds, ordering From ParamsData Where eprimesIds like '%{:04d}%' Order By ordering".format(e))]

		logger.info(len(sets_with_e))
		assert len(sets_with_e) > 0
		intersection = sets_with_e[0][0]
		if sets_with_e[0][1] > highest_ordering_needed:
			highest_ordering_needed = sets_with_e[0][1]
		logger.info("ordering start %s", highest_ordering_needed)


		if len(sets_with_e) > 1:
			for (set_e, ordering) in sets_with_e[1:]:
				new_set = intersection & set_e
				logger.info("new_set %s intersection %s, ordering %s", len(new_set), len(intersection), ordering)
				if len(new_set) < len(intersection):
					highest_ordering_needed = ordering
				intersection = new_set

		logger.info("intersection: %s", intersection)
		insection_mapping[e] = intersection

conn.close()

logger.info("ordering end %s", highest_ordering_needed)

for intersection in insection_mapping.values():
	print(len(intersection), sorted(intersection))


print("highest_ordering_needed: {}".format(highest_ordering_needed))
