#!/bin/bash
# Bilal Syed Hussain

set -o nounset

Dir="$( cd "$( dirname "$0" )" && pwd )";
Tools="$Dir/../tools"

if [ ! -f "$Tools/timeout5" ]; then
	echo "Compile 'timeout5' in $Tools/"
	exit  1	
fi


# Where to put the database
export REPOSITORY_BASE="${REPOSITORY_BASE:-`pwd`}"

echo "REPOSITORY_BASE   is ${REPOSITORY_BASE}"
# Where the paramgen scripts live
echo "PARAM_GEN_SCRIPTS is ${PARAM_GEN_SCRIPTS}"

export MINION_TIMEOUT=${2:-${MINION_TIMEOUT:-600}}
export TOTAL_TIMEOUT=${3:-${TOTAL_TIMEOUT:-800}}

 

echo "MINION_TIMEOUT is ${MINION_TIMEOUT}"
echo "TOTAL_TIMEOUT  is ${TOTAL_TIMEOUT}"

BASE="`basename ${REPOSITORY_BASE}`"

time (
	( conjure --mode generateParams              \
		    --in-eprime-dir "$1"     \
		    --in-essence ${BASE}.essence \
		    --output-directory params            \
			--method uniform-biased                  \
			--seed 100
	) 2>&1 | tee  "${BASE}.genparams.output"
) 2> >(tee "${BASE}.time.genparams" >&2 )
