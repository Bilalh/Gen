#!/bin/bash
set -o nounset
cores=${CORES:-"$(parallel --number-of-cores)"};

function process(){
	out="$1"
	[[ -z "${NO_CACHE:-}" && -d "${out}/summary" ]] || gen instance-summary -o "${out}"
}
export -f process

parallel -j"${cores}" --line-buffer --tagstring '{#} {/.}' \
	'process {//}' \
	:::: <(find . -type d -name 'fastest*')
