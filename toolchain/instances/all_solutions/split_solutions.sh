#!/bin/bash
set -o nounset
set -e

echo "NUM_JOBS is ${NUM_JOBS}"

dir="$1"
cd "$dir"

if [ "$(uname)" == "Darwin" ]; then
	export cmd="gsplit"
else
	export cmd="split"
fi

hash "$cmd" || echo "$cmd not found" >&2


function process(){
	[ "$1" = "" ] && return 0
	"$cmd" -d -a10 -l 1000000 "$1" "$1." && rm "$1"
}
export -f process

parallel -j1 "process" ::: "$(find . -name '*minion-solution')"

echo "Finished"
