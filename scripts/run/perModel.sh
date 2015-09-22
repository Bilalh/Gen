#!/bin/bash
# Bilal Syed Hussain
set -o nounset

Dir="$( cd "$( dirname "$0" )" && pwd )";
Time="/usr/bin/time -p"

MINION_TIMEOUT=${1:-${MINION_TIMEOUT}}
TOTAL_TIMEOUT=${2:-${TOTAL_TIMEOUT}}

echo "MINION_TIMEOUT is ${MINION_TIMEOUT}"
echo "TOTAL_TIMEOUT  is ${TOTAL_TIMEOUT}"

Base="`pwd`";
Name="`basename ${Base}`";
Essence="${Name}.essence";

Mode="${3:-${USE_MODE:-df}}";

Eprime_dir="${Name}_${Mode}";
Param_dir="params";

Output_dir=${GENERATED_OUTPUT_DIR:-}

TIMEOUT5_FILE_BASE=${Output_dir}/__timeout5-${Name}_${Mode}
echo "TIMEOUT5_FILE_BASE is ${TIMEOUT5_FILE_BASE}"

timing_method=${TIMING_METHOD:-cpu}
echo "timing_method: ${timing_method}"
export  timing_method

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
trap "excode=$?; echo '<update_timeout>  removing ${LOCKDIR} from trap for $1'; rmdir "${LOCKDIR}"; trap - EXIT; echo $excode" EXIT SIGHUP SIGINT SIGTERM

sr_time="$1.sr-time"
minion_time="$1.minion-time"
fin="$1.zfinished"
shift

echo "<update_timeout>  LOCKDIR is ${LOCKDIR}"
if mkdir "${LOCKDIR}"; then
    echo "<update_timeout> successfully acquired lock: ${LOCKDIR} for $1"
else
	total=0
	while [ -d "${LOCKDIR}" ]; do
		sleep 1
		(( total++ ))
		# This has never happen yet
		if [[ $total -gt 80 ]]; then
			echo "<update_timeout>  ignoring lock after ${total} seconds for $1"
			break
		fi
	done
    echo "<update_timeout> successfully acquired lock: ${LOCKDIR} for $1 after ${total} seconds"
    mkdir ${LOCKDIR};
fi

echo "<update_timeout> fin ${fin}"
echo "<update_timeout> tf $TIMEOUT5_FILE"

if [ ! -f "${fin}" ]; then
	echo "<update_timeout> no ${fin}"
	echo "<update_timeout>  removing ${LOCKDIR} no ${fin} for $1"
	rmdir "${LOCKDIR}"
	return
fi

if [ ! -f "$TIMEOUT5_FILE" ]; then
	set -x
	echo "<update_timeout> if"
	if ( grep -q "${timing_method}" "$sr_time" && grep -q "${timing_method}"  "${minion_time}" ); then

		(( time_taken  = `grep -h "${timing_method}" ${sr_time} ${minion_time} |  ruby -e 'print $stdin.readlines.map{|n|  n[4..-1].to_f}.reduce(:+).ceil' ` ))
		(( new_timeout = $time_taken  * ${DOMINATION_MULTIPLIER:-2} ))



		echo "<update_timeout> if grep time ${time_taken} ${new_timeout}"
		if [[ $new_timeout -lt $TOTAL_TIMEOUT ]]; then
			echo $time_taken >   "$FASTEST_OUTPUT_DIR/$2.fastest"
			echo $1          >>  "$FASTEST_OUTPUT_DIR/$2.fastest"
			echo "<update_timeout> $1 Changing timeout to $new_timeout from $TOTAL_TIMEOUT"
			echo ""
			echo $new_timeout > "$TIMEOUT5_FILE"
		fi
	fi
	set +x
else
	set -x
	echo "<update_timeout> else"

	if ( grep -q "${timing_method}" "$sr_time" && grep -q "${timing_method}"  "${minion_time}" ); then
		(( time_taken  = `grep -h "${timing_method}" ${sr_time} ${minion_time} |  ruby -e 'print $stdin.readlines.map{|n|  n[4..-1].to_f}.reduce(:+).ceil' ` ))
		(( new_timeout = $time_taken  * ${DOMINATION_MULTIPLIER:-2} ))

		echo "<update_timeout> else grep time ${time_taken} ${new_timeout}"
		current_fastest="`head -n1 ${TIMEOUT5_FILE}`"
		if [[ $new_timeout -lt $current_fastest ]]; then
			echo "<update_timeout> if grep time ${time_taken} ${new_timeout}"
			echo $time_taken >   "$FASTEST_OUTPUT_DIR/$2.fastest"
			echo $1          >>  "$FASTEST_OUTPUT_DIR/$2.fastest"
			echo "<update_timeout> $1 Changing timeout to $new_timeout from ${current_fastest}  (TOTAL_TIMEOUT: $TOTAL_TIMEOUT)"
			echo $new_timeout > "$TIMEOUT5_FILE"
		fi

	fi
	set +x
fi

echo "<update_timeout> end removing ${LOCKDIR} at end  for $1"
rmdir "${LOCKDIR}"
}

function check_errors(){
	out_dir=$1
	param=$2
	if  ( ls "${out_dir}/p-${2}.errors" &>/dev/null ); then
		echo "Found errors for ${param}"
		return 1
	else
		echo "No errors for ${param}"
		return 0
	fi
}

Command=$( cat <<EOF
TIMEOUT5_FILE="${TIMEOUT5_FILE_BASE}-{2/.}";
echo "tfff \${TIMEOUT5_FILE}";
export TIMEOUT5_FILE;
LOCKDIR="$Output_dir/_lockdir_{2/.}";
echo "lddd \${LOCKDIR}";
export LOCKDIR;
(
$Time \
	bash "${Dir}/perModelPerParamCpuTime.sh"  ${Essence} {1} {2} ${MINION_TIMEOUT} ${TOTAL_TIMEOUT} ${Mode}  \
) 3>&1 1>&2 2>&3  | tee "${Output_dir}/{1/.}-{2/.}.stderr";
echo "";
update_timeout  "${Output_dir}/{1/.}-{2/.}" {1/} {2/};
check_errors ${Output_dir} {2/.};
EOF
)

export -f update_timeout
export -f check_errors
export TOTAL_TIMEOUT
export TIMEOUT5_FILE_BASE

parallel --tagstring "{1/.} {2/.}" --halt 1 -j${NUM_JOBS:-6}  $Command ::: ${Eprimes} ::: ${Params};

