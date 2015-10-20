#!/bin/bash
set -o nounset
set -e

echo "NUM_JOBS is ${NUM_JOBS}"

dir="$1"
cd "$dir"

function process(){
	[ "$1" = "" ] && return 0

	if (sw_vers); then
		cmd="gsplit"
	else
		cmd="split"

	"$cmd" -d -a10 -l 1000000 "$1" "$1."
	tar -c "$1" | pigz -c -p"${NUM_JOBS}" > "${1}".tar.gz
	rm "$1"
}
export -f process

parallel -j1 "process" ::: "$(find . -name '*minion-solution')"

echo "Finished"
