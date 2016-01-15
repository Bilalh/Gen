#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse

parser = argparse.ArgumentParser(prog='Meb_pcs')
# args = parser.parse_args()
args = argparse.Namespace(numNodes=20, maxPower=1000, initialNode=20)

for e in ["numNodes", "maxPower", "initialNode"]:
  print("{:11} [1,{:>4d}] [{}]i".format(e, getattr(args, e), getattr(args, e) // 2))

print()
print()

for i in range(1, args.numNodes + 1):
  for j in range(1, args.numNodes + 1):
    print("linkCosts%matrix2d%{:02d}%{:02d} [1,{}] [{}]i".format(i, j, args.maxPower,
                                                                 args.maxPower // 2))
  print()
