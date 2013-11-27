#!/bin/bash
set -o nounset

Count_essence=$(ls -1 *.essence 2> /dev/null | wc -l)

if (( $Count_essence != 1 )); then
	WD="$(pwd)"
	echo "ERROR: Only 1 *.essence file should be in: $WD"
	exit 1
fi

Mode="${1:-${USE_MODE:-df}}";


Essence=$(ls *.essence | head -n 1)

echo "Running conjure ${Mode} on ${Essence} with ${EXTRA_ARGS:-}"
set -x 
conjure --mode "${Mode}" \
	--in "${Essence}" \
	${EXTRA_ARGS:-} \
	+RTS -M15G -s \
	2> >(tee "${Essence%.essence}.${Mode}.stats" >&2)
set +x