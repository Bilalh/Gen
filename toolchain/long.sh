#!/bin/bash
set -o nounset

for i in $(seq 1 5 ); do
	echo "$i"
	echo hello
	sleep $(( i * 2 ))
done
