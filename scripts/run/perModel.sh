#!/bin/bash
# Bilal Syed Hussain
set -o nounset

Dir="$( cd "$( dirname "$0" )" && pwd )";
TIMEOUT5="$Dir/../tools/timeout5"

MINION_TIMEOUT=${1:-${MINION_TIMEOUT:-600}}
TOTAL_TIMEOUT=${2:-${TOTAL_TIMEOUT:-800}}

echo "MINION_TIMEOUT is ${MINION_TIMEOUT}"
echo "TOTAL_TIMEOUT  is ${TOTAL_TIMEOUT}"

Base="`pwd`";
Name="`basename ${Base}`";
Essence="${Name}.essence";

Mode="${3:-${USE_MODE:-df}}";

Eprime_dir="${Name}-${Mode}";
Param_dir="params";

Output_dir=${GENERATED_OUTPUT_DIR:-}

TIMEOUT5_FILE=${TIMEOUT5_FILE:-${Output_dir}/timeout5-$Name-$Mode}
echo "TIMEOUT5_FILE is ${TIMEOUT5_FILE}"
echo "Deleting ${TIMEOUT5_FILE}"
rm -f $TIMEOUT5_FILE

Stats_output=${STATS_OUTPUT_DIR:-}
if [ ! "${Stats_output}" == "" ]; then
	Stats_output="${Stats_output}/"
fi



if [ ! "${Output_dir}" == "" ]; then
	Output_dir="${Output_dir}/";
else
	Output_dir="${Eprime_dir}";
fi

[ -z "${FASTEST_OUTPUT_DIR}" ] && echo '$FASTEST_OUTPUT_DIR Not defined'  && exit 1

Params="${PARAMS_TO_USE:-`ls -1 $Param_dir/*.param`}"
echo "${Params}" > "${Stats_output}${Date}.params-used";

Eprimes="${MODELS_TO_USE:-`ls -1 $Eprime_dir/*.eprime`}"
echo "${Eprimes}" > "${Stats_output}${Date}.models-used";

echo "${MINION_TIMEOUT} ${TOTAL_TIMEOUT}" > "${Stats_output}${Date}.timeout-used";

echo "`pwd`";
echo " --- ${Essence} --- ";

function update_timeout(){
tf="$1.time.all"
fin="$1.zfinished"
shift

echo "<update_timeout> ${tf}"
echo "<update_timeout> ${fin}"
echo "<update_timeout> $TIMEOUT5_FILE"

if [ ! -f "${fin}" ]; then
	echo "<update_timeout> no ${fin}"
	return
fi

set -x
if [ ! -f "$TIMEOUT5_FILE" ]; then
	if ( grep -q "real" "$tf" ); then
		(( time_taken  = `grep "real" $tf | tail -n1 | sed -Ee 's/.*m([0-9]+).*/\1/'` ))
		(( new_timeout = $time_taken  * ${DOMINATION_MULTIPLIER:-2} ))
		if [[ $new_timeout -lt $TOTAL_TIMEOUT ]]; then
			echo $time_taken >   "$FASTEST_OUTPUT_DIR/$2.fastest"
			echo $1          >>  "$FASTEST_OUTPUT_DIR/$2.fastest"
			echo "{$@}   Changing timeout to $new_timeout from $TOTAL_TIMEOUT"
			echo $new_timeout > $TIMEOUT5_FILE
		fi
	fi
fi
set +x
# FIXME if found better timeout
}

Command=$( cat <<EOF
echo -e '\n***  {1} {2/} ***';
(
time $TIMEOUT5 --timeout-file $TIMEOUT5_FILE --interval 5  -k15 $TOTAL_TIMEOUT \
	bash "${Dir}/perModelPerParam.sh"  ${Essence} {1} {2} ${MINION_TIMEOUT} ${TOTAL_TIMEOUT} ${Mode}  \
) 3>&1 1>&2 2>&3  | tee "${Output_dir}/{1/.}-{2/.}.time.all";
update_timeout  "${Output_dir}/{1/.}-{2/.}" {1/} {2/}
EOF
)

export -f update_timeout
export TOTAL_TIMEOUT
export TIMEOUT5_FILE

parallel --tagstring "{1/.} {2/.}" -j${NUM_JOBS:-6}  $Command ::: ${Eprimes} ::: ${Params};
