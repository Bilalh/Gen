#!/bin/bash
set -o nounset
parallel --keep-order --colsep=, --header=, "echo '*{seq}*'" :::: all.csv > synced.txt
