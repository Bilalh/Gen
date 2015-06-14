#!/bin/bash
set -o nounset
export OUR="$( cd "$( dirname "$0" )" && pwd )";


function process(){
	if [ "$1" == "." ]; then
		return 0
	fi

	dir="$1"
	pushd "${dir}"

	#shellcheck disable=SC2016
	sed -e 's!"${DIR}/savilerow" "$@"!exec "${DIR}/savilerow2.sh" "$@"!' \
	  < savilerow > savilerow2.sh

	chmod +x savilerow2.sh

	popd
}

export -f process
parallel -j1 --tagstring "{}" "process {//}"  \
	::: "$(find "$1" -type f -name savilerow)"

