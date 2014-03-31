#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse

parser = argparse.ArgumentParser(prog='warehouse_pcs')
parser.add_argument("n_upper", type=int)
parser.add_argument("n_stores", type=int)
parser.add_argument("n_warehouses", type=int)

args = parser.parse_args()

for e in ["n_upper", "n_stores", "n_warehouses"]:
	print("{:20s} [1,{}] [{}]i".format(e, args.__dict__[e], args.__dict__[e] // 2) )

print()
print()

for e in ["capacity", "opencost"]:
	for i in range(1, args.n_warehouses + 1):
		print("{}%FT%{:03d}  [{}, {}] [{}]i".format(e, i, 1,  args.n_upper, args.n_upper //2))
	print()
	print()

for i in range(1, (args.n_warehouses * args.n_stores) + 1 ):
	print("cost%FT_T{}%{:03d}  [{}, {}] [{}]i".format(1, i, 1, args.n_upper, args.n_stores // 2))
	print("cost%FT_T{}%{:03d}  [{}, {}] [{}]i".format(2, i, 1, args.n_upper, args.n_warehouses // 2))
	print("cost%FT_T{}%{:03d}  [{}, {}] [{}]i".format("V", i, 1, args.n_upper, args.n_upper // 2))
	print()