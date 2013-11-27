#!/bin/bash
# Bilal Syed Hussain
set -o nounset
Dir="$( cd "$( dirname "$0" )" && pwd )";
Tools="$Dir/../tools"
Mode="${USE_MODE:-df}";


if [ ! -f "$Tools/timeout5" ]; then
	echo "Compile 'timeout5' in $Tools/"
	exit  1	
fi

set -x
# Put the results in a different directory by default
export GENERATED_OUTPUT_DIR=${GENERATED_OUTPUT_DIR:-"${OUT_BASE_DIR:-.}/results-`basename $(pwd)`-${Mode}"}
#"
export STATS_OUTPUT_DIR=${STATS_OUTPUT_DIR:-"${OUT_BASE_DIR:-.}/stats-`basename $(pwd)`-${Mode}"}
#"
export FASTEST_OUTPUT_DIR=${FASTEST_OUTPUT_DIR:-"${OUT_BASE_DIR:-.}/fastest-`basename $(pwd)`-${Mode}"}
#"
set +x

parallel "test -n {}  && mkdir {}" ::: \
	"${STATS_OUTPUT_DIR}" "${GENERATED_OUTPUT_DIR}" "${FASTEST_OUTPUT_DIR}"


export Date="${USE_DATE}"
echo "DATE: ${Date}"
( 
	time ${Dir}/runModel.sh "$@" \
	| tee "${STATS_OUTPUT_DIR}/${Date}.output"
) \
	3>&1 1>&2 2>&3  \
	|  tee "${STATS_OUTPUT_DIR}/${Date}.time"

