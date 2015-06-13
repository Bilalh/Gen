#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse
import logging
import sqlite3
import sys

logger = logging.getLogger(__name__)
parser = argparse.ArgumentParser()

parser.add_argument("db", help='info.sqlite')
parser.add_argument("num", help='group id', type=int)
args = parser.parse_args()

get_query = "SELECT * FROM ToolchainGroup WHERE groupId = ?;"
with sqlite3.connect(args.db) as conn:
    conn.row_factory = sqlite3.Row
    res = list(conn.execute(get_query, (args.num,) ))
    if not res:
        print("Group does not exist : {} ".format(args.num))
        sys.exit(1)
    else:
        row = res[0]
        print("SR_REV={savilerow} MINION_REV={minion} CONJURE_REV={conjureNew} ~/server-settings/install_repos.sh ".format(**row))

