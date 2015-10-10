#!/bin/bash
# Bilal Syed Hussain
set -o nounset
OUR="$( cd "$( dirname "$0" )" && pwd )";
Mode="${USE_MODE}";
cputimeout="${OUR}/../../cputimeout/cputimeout"

if [ ! -f "$cputimeout" ]; then
	echo "Compile 'cputimeout' in $cputimeout using make"
	exit  1
fi

set -x
# Put the results in a different directory by default
export GENERATED_OUTPUT_DIR="${OUT_BASE_DIR}/results_${Mode}/"
export STATS_OUTPUT_DIR="${OUT_BASE_DIR}/stats_${Mode}/"
export FASTEST_OUTPUT_DIR="${OUT_BASE_DIR}/fastest_${Mode}/"
set +x

parallel "test -n {}  && mkdir {}" ::: \
	"${STATS_OUTPUT_DIR}" "${GENERATED_OUTPUT_DIR}" "${FASTEST_OUTPUT_DIR}"


export Date="${USE_DATE}"
echo "DATE: ${Date}"
(
(
	time "${OUR}/perModel.sh" "$@" 2>&1 \
	| tee "${STATS_OUTPUT_DIR}/${Date}.output"
) \
	3>&1 1>&2 2>&3  \
	|  tee "${STATS_OUTPUT_DIR}/${Date}.time"

) 3>&2 2>&1 1>&3
