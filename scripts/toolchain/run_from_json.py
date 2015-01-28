#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import logging
import argparse
import shutil
import json

from pprint import pprint
from pathlib import Path

logger = logging.getLogger(__name__)
parser = argparse.ArgumentParser()

parser.add_argument("json", help='')
parser.add_argument("arg", help='')
parser.add_argument("host", help='')
parser.add_argument('--bin_base', default='../')
parser.add_argument('--out_dir', default='outt')
parser.add_argument('--timeout', default=30)
parser.add_argument('--cores', default=4)


args = parser.parse_args()

with open(args.json, "r") as f:
	d  = json.loads(f.read())

data = d[args.arg]
data.update(args.__dict__)

print("\n###")
print("$PARAM_GEN_SCRIPTS/toolchain/toolchain.py {spec_dir}/spec.essence \
--outdir={out_dir} \
--timeout={timeout} \
--num_cores={cores} \
--bin_dir={bin_base}/{bin_dir}/{host} \
--new_conjure \
	###toolchain".format(**data) )


print("\n###")
print("$PARAM_GEN_SCRIPTS/toolchain/toolchain_recheck.py {spec_dir} \
--outdir={out_dir} \
--num_cores={cores} \
--bin_dir={bin_base}/{bin_dir}/{host} \
--new_conjure \
###toolchain_rerun".format(**data) )

# print("\n")
# print("testReduce {spec_dir} \
# -o {out_dir} \
# -p {timeout}  \
# -cores {cores} \
# --new-conjure \
# --kind KindAny_ \
# --status StatusAny_ \
# ###reduce_same".format(**data) )


print("\n")
print("testReduce {spec_dir} \
-o {out_dir} \
-p {timeout}  \
-cores {cores} \
--new-conjure \
--kind KindAny_ \
--status StatusAny_ \
###reduce_any".format(**data) )

