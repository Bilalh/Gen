#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# does hitting_set_gent.txt

from pprint import pprint, pformat
import argparse
import sqlite3

import logging
logger = logging.getLogger(__name__)
logging.basicConfig(format='%(lineno)d: %(message)s', level=logging.ERROR)

parser = argparse.ArgumentParser(prog='gent_idea')
parser.add_argument("db", help='results.db')
parser.add_argument("hitting_set", help='form {1,2,4,5}')

args = parser.parse_args()
logger.info(args)

str_set = args.hitting_set
if str_set == '{}':
    str_set = 'set()'

hset = eval(str_set)

insection_mapping = {}

highest_ordering_needed = -1

# Since the ids are not zero padded append a @ at the end and the start
query = """
    Select * From
    (Select group_concat(D.id, ", ") as eprimesIds, ordering, count(D.eprimeId) as len

    From ParamQuality P
    Join (Select *, "@" || eprimeId || "@" as id from TimingsDomination ) D
    ON P.paramHash = D.paramHash

    Where D.isDominated = 0 AND quality < 1

    Group by P.paramHash
    Order by P.quality
    ) Where eprimesIds like '%@{}@%'
"""

with sqlite3.connect(args.db) as conn:
    for e in hset:
        logging.info("processing %s", e)
        sets_with_e = [
            (set(e[1:-1] for e in eprimes.split(", ")), ordering)
            for (eprimes, ordering, numEprimes) in conn.execute(query.format(e))
        ]

        logger.info("len(sets_with_e) %d", len(sets_with_e))
        logger.debug("sets_with_e %s", (sets_with_e))
        assert len(sets_with_e) > 0
        intersection = sets_with_e[0][0]
        if sets_with_e[0][1] > highest_ordering_needed:
            highest_ordering_needed = sets_with_e[0][1]
        logger.info("ordering start %s", highest_ordering_needed)

        if len(sets_with_e) > 1:
            for (set_e, ordering) in sets_with_e[1:]:
                new_set = intersection & set_e
                logger.info("new_set %s intersection %s, ordering %s", len(new_set),
                            len(intersection), ordering)
                logger.debug("new_set %s intersection %s, ordering %s", new_set,
                             intersection, ordering)
                if len(new_set) < len(intersection):
                    highest_ordering_needed = ordering
                intersection = new_set

        logger.info("intersection: %s", intersection)
        insection_mapping[e] = intersection

conn.close()

logger.info("ordering end %s", highest_ordering_needed)

for intersection in insection_mapping.values():
    print(len(intersection), sorted(map(int, intersection)))

print("highest_ordering_needed: {}".format(highest_ordering_needed))
