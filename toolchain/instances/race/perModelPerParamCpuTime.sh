#!/bin/bash
set -o nounset

OUR="$( cd "$( dirname "$0" )" && pwd )";
Time="/usr/bin/time -p"

# limit swap space used on my laptop
if [ "$(uname)" = "Darwin" ] ; then
    ulimit -Sv 2000000
fi

ESSENCE="$1"
EPRIME="$2"
PARAM="$3"
# MINION_TIMEOUT=${4}  # not used anymore
TOTAL_TIMEOUT=${5}

EPRIMEBASE=${EPRIME%.eprime}

OUTPUT_BASE=${GENERATED_OUTPUT_DIR:-}
[ ! "${OUTPUT_BASE}" == "" ] && EPRIMEBASE="${OUTPUT_BASE}/`basename ${EPRIMEBASE}`"


PARAM_NAME="`basename ${PARAM}`"
PARAMBASE=${PARAM_NAME%.param}


START_FILE="${EPRIMEBASE}-${PARAMBASE}.zstarted"
END_FILE="${EPRIMEBASE}-${PARAMBASE}.zfinished"

EPRIME_PARAM="${EPRIMEBASE}-${PARAMBASE}.eprime-param"
# EPRIME_SOLUTION="${EPRIMEBASE}-${PARAMBASE}.eprime-solution"
# ESSENCE_SOLUTION="${EPRIMEBASE}-${PARAMBASE}.solution"
SR_OUTPUT="${EPRIMEBASE}-${PARAMBASE}.sr-output"


MINION="${EPRIMEBASE}-${PARAMBASE}.minion"
MINION_SOLUTION="${EPRIMEBASE}-${PARAMBASE}.minion-solution"
# MINION_STATS="${EPRIMEBASE}-${PARAMBASE}.minion-stats"
MINION_TABLE="${EPRIMEBASE}-${PARAMBASE}.minion-table"
MINION_TIME="${EPRIMEBASE}-${PARAMBASE}.minion-time"
MINION_OUTPUT="${EPRIMEBASE}-${PARAMBASE}.minion-output"

REFINE_TIME="${EPRIMEBASE}-${PARAMBASE}.param-time"
SAVILEROW_TIME="${EPRIMEBASE}-${PARAMBASE}.sr-time"
touch $START_FILE


MSG_TEMPLATE="$ESSENCE `basename ${EPRIME}` $PARAM_NAME"

FAIL_FILE="${EPRIMEBASE}.fails"
PARAM_ERROR_FILE="`dirname ${EPRIMEBASE}`/p-${PARAMBASE}.errors"
echo "PARAM_ERROR_FILE is $PARAM_ERROR_FILE"

echo "TIMEOUT5_FILE is ${TIMEOUT5_FILE}"

CPUTIMEOUT_ARR=("${OUR}/../../cputimeout/cputimeout")
CPUTIMEOUT_ARR+=(--timeout-file "$TIMEOUT5_FILE" --interval 1 -f -k1 --preserve-status)
CPUTIMEOUT="${CPUTIMEOUT_ARR[*]}"



PREVIOUS_USED=0

echo ""
echo "*** `basename $ESSENCE` - `basename $EPRIME` - `basename $PARAM` ***"
#"

RESULTOF_REFINEPARAM=0
MSG_REFINEPARAM="{refineParam}        $MSG_TEMPLATE"
echo "$MSG_REFINEPARAM"

# print the command the run it
function echoer(){
    echo "$@" >&2
    $Time "$@"
}

function update_total_time(){
    cur=$1
    fp=$2
    taken="`grep cpu $fp | egrep -o '[0-9]+.[0-9]+'`"
    set -x
    # we are using floor which will give up to a second more time.
    TOTAL_TIMEOUT="$(echo "(${cur}-${taken})/1" | bc)"
    set +x
    taken="$(echo "${taken}/1" | bc )"
    (( PREVIOUS_USED += taken ))
}

date +'StartP %a %d %b %Y %k:%M:%S %z%nStartP(timestamp) %s' >&2

echoer \
${CPUTIMEOUT} --write-time ${REFINE_TIME} --previous-used $PREVIOUS_USED $TOTAL_TIMEOUT  \
conjure refine-param                                                            \
    --eprime  $EPRIME                                           \
    --essence-param $PARAM                                      \
    --eprime-param $EPRIME_PARAM;

RESULTOF_REFINEPARAM=$?
echo "~~~ RESULTOF_REFINEPARAM ${RESULTOF_REFINEPARAM}"

date +'finP %a %d %b %Y %k:%M:%S %z%nfinP(timestamp) %s' >&2

# if result > 128, it was killed by sig (result - 128)
if (( RESULTOF_REFINEPARAM != 0 )) ; then
    echo "$MSG_REFINEPARAM" >> "$FAIL_FILE"

    # exit code 2 seem to be stack overflow
    if (( RESULTOF_REFINEPARAM != 137 &&  RESULTOF_REFINEPARAM != 124  && RESULTOF_REFINEPARAM != 2  )); then
        echo "$MSG_REFINEPARAM" >> "$PARAM_ERROR_FILE"
    fi

    exit 2
fi

if [ ! -f $EPRIME_PARAM ]; then
    echo "$MSG_REFINEPARAM" >> "$FAIL_FILE"
    exit 1
fi

update_total_time ${TOTAL_TIMEOUT} ${REFINE_TIME}

if (( TOTAL_TIMEOUT <= 0 )) ; then
    echo "$MSG_REFINEPARAM" >> "$FAIL_FILE"
    exit 1
fi

echo "TOTAL_TIMEOUT is $TOTAL_TIMEOUT now"
echo "PREVIOUS_USED is $PREVIOUS_USED now"


RESULTOF_SAVILEROW=0
MSG_SAVILEROW="{savilerow}          $MSG_TEMPLATE"
echo "$MSG_SAVILEROW"



date +'StartSR %a %d %b %Y %k:%M:%S %z%nStartSR(timestamp) %s' >&2

sr_dir="$(dirname "$(which savilerow)")"
scmd=("${CPUTIMEOUT_ARR[@]}")
scmd+=(--write-time "${SAVILEROW_TIME}" --previous-used "$PREVIOUS_USED" "$TOTAL_TIMEOUT")
scmd+=(java -XX:ParallelGCThreads=1 -Xmx${JAVA_MEMORY:-4G} -server -ea -jar "$sr_dir/savilerow.jar")
scmd+=(-in-eprime "$EPRIME" -in-param "$EPRIME_PARAM" -out-minion "$MINION")

echo "${scmd[@]}"
echo "${scmd[@]}" >&2

# Using just tee loses the exit code
(
set -o pipefail
(
	/usr/bin/time -p  "${scmd[@]}" 2>&1 \
	| tee "${SR_OUTPUT}"
)
)


RESULTOF_SAVILEROW=$?
echo "~~~ RESULTOF_SAVILEROW ${RESULTOF_SAVILEROW}"


date +'finSR %a %d %b %Y %k:%M:%S %z%nfinSR(timestamp) %s' >&2


if (( RESULTOF_SAVILEROW != 0 )) ; then
	echo "$MSG_SAVILEROW" >> "$FAIL_FILE"
	if (( RESULTOF_SAVILEROW != 137 &&  RESULTOF_SAVILEROW != 124  )) ; then

		if ( grep -qc 'Sub-process exited with error code:137' "${SR_OUTPUT}"); then
			echo "savilerow's sub-process was killed with code 137"
			echo "savilerow's sub-process was killed with code 137" >&2
			exit 1
		elif ( grep -qc 'Sub-process exited with error code:124' "${SR_OUTPUT}"); then
			echo "savilerow's sub-process was killed with code 124"
			echo "savilerow's sub-process was killed with code 127" >&2
			exit 1
		elif ( grep -qc 'Error occurred during initialization of VM' "${SR_OUTPUT}"); then
			if ( grep -qc 'java.lang.OutOfMemoryError: unable to create new native thread' "${SR_OUTPUT}"); then
				echo "$MSG_SAVILEROW ~ Error occurred during initialization of VM, java.lang.OutOfMemoryError: unable to create new native thread" >> "$PARAM_ERROR_FILE"
				exit 3
			else
				echo "$MSG_SAVILEROW ~ Error occurred during initialization of VM" >> "$PARAM_ERROR_FILE"
				exit 4
			fi
		else
			echo "$MSG_SAVILEROW" >> "$PARAM_ERROR_FILE"
			exit 5
		fi
	else
		exit 1
	fi

fi

if [ ! -f $MINION ]; then
    echo "$MSG_SAVILEROW" >> "$FAIL_FILE"
    exit 1
fi

update_total_time ${TOTAL_TIMEOUT} ${SAVILEROW_TIME}

if (( $TOTAL_TIMEOUT <= 0 )) ; then
    echo "$MSG_SAVILEROW" >> "$FAIL_FILE"
    exit 1
fi

echo "TOTAL_TIMEOUT is $TOTAL_TIMEOUT now"
echo "PREVIOUS_USED is $PREVIOUS_USED now"


RESULTOF_MINION=0
MSG_MINION="{minion}             $MSG_TEMPLATE"
echo "$MSG_MINION"


minion_cpu=$((TOTAL_TIMEOUT - PREVIOUS_USED - 2))
if [ ${minion_cpu} -lt 0 ]; then
    minion_cpu=0
fi

date +'StartMINION %a %d %b %Y %k:%M:%S %z%nStartMINION(timestamp) %s' >&2

mcmd=("${CPUTIMEOUT_ARR[@]}")
mcmd+=(--write-time "$MINION_TIME" --previous-used "$PREVIOUS_USED" "$TOTAL_TIMEOUT")
mcmd+=(minion "$MINION" -noprintsols -preprocess SACBounds )
mcmd+=(-tableout "$MINION_TABLE" -solsout "$MINION_SOLUTION")
mcmd+=(-cpulimit ${minion_cpu})

echo "${mcmd[@]}"
echo "${mcmd[@]}" >&2

# Using just tee loses the exit code
(
set -o pipefail
(
	/usr/bin/time -p  "${mcmd[@]}" 2>&1 \
	| tee "${MINION_OUTPUT}"
)
)

RESULTOF_MINION=$?
echo "~~~ RESULTOF_MINION ${RESULTOF_MINION}"
date +'finMINION %a %d %b %Y %k:%M:%S %z%nfinMINION(timestamp) %s' >&2


# For optimisation problem if we may have a non-optimal solution
# We at the moment consider this a failure but not an error

#  SIGKILL and SIGTEM
if (( $RESULTOF_MINION == 137 ||  $RESULTOF_MINION == 124  )) ; then
    echo "$MSG_MINION" >> "$FAIL_FILE"
    exit 1
fi

if (( $RESULTOF_MINION != 0 )) ; then
    echo "$MSG_MINION" >> "$FAIL_FILE"
    echo "$MSG_MINION" >> "$PARAM_ERROR_FILE"
    exit 6
fi


# When the timeout of CPUTIMEOUT was reduced, and minion was SIGKILL'd
if [[  ! -s ${MINION_TABLE} ]]; then
    echo "$MSG_MINION" >> "$FAIL_FILE"
    exit 1
fi


timeout="$( "${OUR}/didMinionTimeout.py" "${MINION_TABLE}" )"
if [ $? -ne 0 ]; then
    echo "$MSG_MINION" >> "$FAIL_FILE"
    echo "$MSG_MINION  didMinionTimeout.py failed with output ${timeout}" >> "$PARAM_ERROR_FILE"
    exit 7
fi

if [ "${timeout}" -ne 0 ]; then
    echo "$MSG_MINION" >> "$FAIL_FILE"
    exit 1
fi


echo "YFINISHED ${EPRIMEBASE}-${PARAMBASE}"
touch $END_FILE
