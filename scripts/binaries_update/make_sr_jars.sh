#!/bin/bash
# Compiles SR using the folder names as the hashes
set -o nounset
export OUR="$( cd "$( dirname "$0" )" && pwd )";


export SR_DIR="$1"
base="$2"

function process(){
	if [ "$1" == "sr" ]; then
		return 0
	fi

	pushd "${SR_DIR}"
	hg update --clean --rev "${1}"
	./compile.sh
	mv savilerow.jar "${2}"
	popd
}

export -f process
parallel -j1 --tagstring "{/}" "process {/} {}"  \
	::: "$(find "${base}" -maxdepth 1 -type d  )"
