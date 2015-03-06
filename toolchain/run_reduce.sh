#!/bin/bash
set -o nounset

# Run testReduce on specs in _errror  and places them in _reduced

base="$1"

if [ ! -d "$base/_errors" ]; then
	echo "No _errors dir... exiting"
	exit 1
fi

function process (){
	path="$1"
	dir="${2}"
	out_dir="${2/_errors/_reduced}"
	num="${3}"

	status="$( basename "$(dirname "${dir}")")"
	kind="$(basename "$(dirname "$(dirname "${dir}")")" )"

	status=${status/ErrorUnknown_/StatusAny_}

	mkdir -p "$out_dir"
	testReduce "$dir" -o "${out_dir}" -p"${TIME_OUT:-10}" -c"${CORES}" \
		--seed "$((SEED_BASE + num))" --new-conjure \
		--kind="${kind}" --status="${status}"
}

export -f process

export CORES=${CORES:-$(parallel --number-of-cores)}
export SEED_BASE=${SEED_BASE:-${RANDOM}}

mkdir -p "${base}/_reduced"

parallel -j1 --keep-order --linebuffer --halt 1  \
	--rpl '{@} $Global::use{"File::Basename"} ||= eval "use File::Basename; 1;"; $_ = basename(dirname($_));' \
	--tagstring '{@}' \
	"process {} {//} {#}" ::: \
	"$(find "$base/_errors/" -type f -name 'spec.specE' ! -path '*zall*' | sort )" \
	2>&1 | tee "$base/_reduced/_all.logged"

