#!/bin/bash
# Bilal Syed Hussain
set -o nounset

Dir="$( cd "$( dirname "$0" )" && pwd )";
Tools="$Dir/../tools"

if [ ! -f "$Tools/timeout5" ]; then
	echo "Compile 'timeout5' in $Tools/"
	exit  1	
fi

echo "PARAM_GEN_SCRIPTS is ${PARAM_GEN_SCRIPTS}"


export TOTAL_TIMEOUT=10
export NUM_JOBS=5

conjure --mode generateParams \
		--method uniform-biased \
	    --in-essence prob024-Langford.essence \
	    --in-eprime-dir prob024-Langford-df \
		--output-directory out \
		--seed 32 