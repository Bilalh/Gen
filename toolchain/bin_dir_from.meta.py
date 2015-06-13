#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse
import json
import logging
import os
import socket
import sys

from pathlib import Path

logger = logging.getLogger(__name__)
parser = argparse.ArgumentParser()
parser.add_argument("meta", help='meta.json file')
parser.add_argument('-s',  action='store_true', dest='output_with_var', help='Output with $SAVED_BINARIES')

args = parser.parse_args()

sb = os.getenv("SAVED_BINARIES")
if not sb:
	print("$SAVED_BINARIES not defined")
	sys.exit(1)

data = json.load(open(args.meta))

our_host_type = os.getenv("HOST_TYPE")
if not our_host_type:
	our_host_type=socket.gethostname()



if data['host_type'] != our_host_type:
	p = Path(sb) / data['bin_dir'] / our_host_type
	if p.exists():
		host_type = our_host_type
	else:
		print("Host type don't match {} != {} ".format(data['host_type'], our_host_type ) )
		print("Also no version was complied in $SAVED_BINARIES/{} for {}".format( data['bin_dir'], our_host_type   ) )
		sys.exit(2)
else:
	host_type = data['host_type']

if args.output_with_var: 
    print("$SAVED_BINARIES/{}/{}".format(data['bin_dir'], host_type ))
else:
    print("{}/{}/{}".format(sb,data['bin_dir'], host_type ))

