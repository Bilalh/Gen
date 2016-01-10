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

gateInpts_mid = args.numGates - ((args.numGates + 1) // 2)

for i in range(1, args.numGates + 1):
  for j in range(1, 2 + 1):
    print("{}%FT_int_matrix%{:02d}%{:03d}  [{}, {}] [{}]i".format(
        "gateInpts", i, j, -1, args.numGates, gateInpts_mid))
  print()

print()
print()

print("{}%FT_int_bool%count  [{}, {}] [{}]i".format("finalOutputs", 0, 3, 1))
for i in range(1, args.numGates + 1):
  print("{}%F_int_bool%{:02d}  [{}, {}] [{}]i".format("finalOutputs", i, 0, 1, 0))

print()
print()

for i in range(1, args.numGates + 1):
  print("{}%FT_int_complex_bool%{:02d}%count  [{}, {}] [{}]i".format(
      "gateFuncs", i, 0, 81, 81 // 2))
  for j in range(1, 81 + 1):
    print("{}%FT_int_complex_bool%{:02d}%{:03d}  [{}, {}] [{}]i".format(
        "gateFuncs", i, j, 0, 1, 0))
  print()

print()
print()
