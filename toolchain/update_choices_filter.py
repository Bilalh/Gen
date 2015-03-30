#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import fileinput
import re

fixer = re.compile(r'{"HasRepresentation":"(.*?)"}')

for line in fileinput.input():
	print(fixer.sub(r'{"HasRepresentation":{"Name":"\1"}}', line), end="")
