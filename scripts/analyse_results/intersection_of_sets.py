#!/usr/local/bin/python3
# -*- coding: utf-8 -*-
# given lines of the form  {1,2,3,4} output the intersection

import fileinput

# to get rid of leading zeroes
# sed -E 's/[[:<:]]00*([1-9][0-9]*)/\1/g'

instances = None
for line in fileinput.input():
	eprimes = eval(line.strip())
	if instances is None:
		instances = eprimes
	else:
		instances &= eprimes
	# print(len(eprimes), instances)

if instances == set():
	print("{}")
else:
	print(instances)