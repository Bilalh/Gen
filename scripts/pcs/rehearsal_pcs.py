#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse

parser = argparse.ArgumentParser(prog='Rehearsal_pcs')
# args = parser.parse_args()
args = argparse.Namespace(n_pieces=4, n_players=4)

#  Note need to use linux for n_pieces|n_players > ~40

for e in ["n_pieces", "n_players"]:
  print("{:11} [1,{}] [{}]i".format(e, getattr(args, e), getattr(args, e) // 2))

print()
print()

for i in range(1, args.n_pieces + 1):
  print("duration%FT%{:03d}     [1,100] [50]i".format(i))

print()
print()

for i in range(1, args.n_players * args.n_pieces + 1):
  print("plays_in%rel%{:03d}   [0,1] [0]i".format(i))
  print("plays_in%rel%{:03d}%{:01d} [1,{}] [{}]i".format(i, 1, args.n_players,
                                                         args.n_players // 2))
  print("plays_in%rel%{:03d}%{:01d} [1,{}] [{}]i".format(i, 2, args.n_pieces,
                                                         args.n_pieces // 2))
  print()
