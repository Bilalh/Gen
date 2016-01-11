#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse

parser = argparse.ArgumentParser(prog='Diagnosis_pcs')
# parser.add_argument("numGates", type=int)
# args = parser.parse_args()
args = argparse.Namespace(numGates=3)

for e in ["numGates"]:
  print("{} [1,{}] [{}]i".format(e, getattr(args, e), getattr(args, e) // 2))

print()
print()

for i in range(1, args.numGates + 1):
  print("{}%enumerated%{:02d} [{}, {}] [{}]i".format("gateFuncs", i, 0, 80, 80 // 2))

print()
print()

gateInpts_mid = args.numGates - ((args.numGates + 1) // 2)
for i in range(1, args.numGates + 1):
  for j in range(1, 2 + 1):
    print("{}%FT_int_matrix%{:02d}%{:03d}  [{}, {}] [{}]i".format(
        "gateInpts", i, j, -1, args.numGates, gateInpts_mid))
  print()

print()
print()

for i in range(1, args.numGates + 1):
  print("{}%P_int_bool%{:02d}  [{}, {}] [{}]i".format("finalOutputs", i, 0, 1, 0))
  print("{}%P_int_bool%{:02d}%picked  [{}, {}] [{}]i".format("finalOutputs", i, 0, 1, 0))
