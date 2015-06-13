#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse
import csv
import logging
import shutil
import sqlite3
import sys

from collections import namedtuple
from pathlib import Path
from pprint import pprint

logger = logging.getLogger(__name__)
parser = argparse.ArgumentParser()

parser.add_argument("db", help='info.sqlite')
parser.add_argument("csv", help='versions*.csv')
parser.add_argument("filePath", help='relative file path e.g. `date/2015-02-24_1424776251/eno`')
args = parser.parse_args()

exists_query = "SELECT ? in (Select filePath from Groups)"
with sqlite3.connect(args.db) as conn:
	if list(conn.execute(exists_query, (args.filePath,) ))[0][0] == 1:
		print("Already added: {} ".format(args.filePath))
		sys.exit(0)
	


versions_query = 'INSERT OR IGNORE  INTO Versions ( name, hash, commitDate, scm, id) VALUES(?,?,?,?, (SELECT IFNULL(MAX(id), 0) + 1 FROM Versions))'
versions_values = []

hosts_query = """INSERT OR IGNORE INTO
Hosts ( hostType, binId, id) VALUES(
	?,
	(SELECT id FROM Versions WHERE name = ? and hash=?),
	(SELECT IFNULL(MAX(id), 0) + 1 FROM Hosts)
)
"""
hosts_values = []


group_hostType = None
group_query = "INSERT INTO Groups(hostType,filePath) Values(?,?)"

groupItems_query = """INSERT INTO
GroupItems ( groupId, binId, id) VALUES(
	?,
	(SELECT id FROM Versions WHERE name = ? and hash=?),
	(SELECT IFNULL(MAX(id), 0) + 1 FROM GroupItems)
)
"""

groupItems_parts = []

def emptyToNull(s):
	if s == "":
		return None
	else:
		return s

with Path(args.csv).open("r") as f:
	reader = csv.DictReader(f, skipinitialspace=True, delimiter=',')

	host_type=None
	for row in reader:
		versions_values.append(
			[ row['name'], row['hash'], emptyToNull(row["ver_date"]),  row['scm']])

		hosts_values.append(
			[ row['host_type'], row['name'], row['hash']])

		groupItems_parts.append(
			[ row['name'], row['hash']])

		group_hostType = row['host_type']


# pprint(versions_values)
# pprint(hosts_values)

with sqlite3.connect(args.db) as conn:
	conn.executemany(versions_query, versions_values)
	conn.executemany(hosts_query, hosts_values)

	cur = conn.cursor()
	cur.execute(group_query, (group_hostType, args.filePath) )
	group_id = list(conn.execute("Select id from Groups where rowId = ?", (cur.lastrowid,)))[0][0]
	cur.executemany(groupItems_query, ([group_id] + vals for vals in groupItems_parts) )

	conn.commit()


