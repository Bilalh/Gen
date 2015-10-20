#!/bin/bash
set -o nounset

OUR="$( cd "$( dirname "$0" )" && pwd )";


SOL_NAME="$1"
LINE="$2"
EPRIME="$3"
PARAM="$4"
TOTAL_TIMEOUT="$5"

Time="/usr/bin/time -p"

# limit swap space used on my laptop
if [ "$(uname)" = "Darwin" ] ; then
    ulimit -Sv 2000000
fi


OUTPUT_BASE=${GENERATED_OUTPUT_DIR}
SOLS_BASE=${GENERATED_SOLUTIONS_DIR}

PARAM_NAME="$(basename "${PARAM}")"
PARAMBASE=${PARAM_NAME%.param}


START_FILE="${OUTPUT_BASE}/${PARAMBASE}.zstarted"
END_FILE="${OUTPUT_BASE}/${PARAMBASE}.zfinished"

AUX="${SOLS_BASE}/${PARAMBASE}.eprime-param.aux"
EPRIME_SOLUTION="${SOLS_BASE}/${PARAMBASE}.eprime-solution"
ESSENCE_SOLUTION="${OUTPUT_BASE}/${PARAMBASE}.solution"

MINION="${SOLS_BASE}/${PARAMBASE}.minion"
MINION_SOLUTION="${OUTPUT_BASE}/${SOL_NAME}-${LINE}.minion-solution"

SOL_PATH="${SOLS_BASE}/${SOL_NAME}"


SAVILEROW_TIME_UP="${OUTPUT_BASE}/${PARAMBASE}.sr2-time"
TRANSLATESOLN_TIME="${OUTPUT_BASE}/${PARAMBASE}.up-time"

MSG_TEMPLATE="${SOL_NAME} $LINE "

FAIL_FILE="${OUTPUT_BASE}/${PARAMBASE}.fails"
PREVIOUS_USED=0


echo "TIMEOUT5_FILE is ${TIMEOUT5_FILE}"
CPUTIMEOUT="${OUR}/../../cputimeout/cputimeout --timeout-file $TIMEOUT5_FILE --interval 1 -k1"

# print the command the run it
function echoer(){
    echo "$@"
    $Time "$@"
}

function update_total_time(){
    cur=$1
    fp=$2
    taken="`grep cpu $fp | egrep -o '[0-9]+.[0-9]+'`"
    set -x
    # we are using floor which will up give up a second more time.
    TOTAL_TIMEOUT="$(echo "(${cur}-${taken})/1" | bc)"
    set +x
    taken="$(echo "${taken}/1" | bc )"
    (( PREVIOUS_USED += taken ))
}

touch "$START_FILE"


sed "${LINE}q;d" "${SOL_PATH}" > "${MINION_SOLUTION}"

function savilerow(){
    timeout=$1
    timefile=$2
    shift
    shift
    sr_dir="`dirname $(which savilerow)`"
    echo "savilerow $@"
    echo "java -XX:ParallelGCThreads=1 -Xmx${JAVA_MEMORY:-2G}  -server -ea -jar $sr_dir/savilerow.jar $@"

    echoer \
    ${CPUTIMEOUT} --write-time ${timefile} --previous-used $PREVIOUS_USED $timeout \
    java -XX:ParallelGCThreads=1 -Xmx${JAVA_MEMORY:-4G}  -server -ea -jar $sr_dir/savilerow.jar $@
}


date +'StartSR2 %a %d %b %Y %k:%M:%S %z%nStartSR2(timestamp) %s' >&2
savilerow  $TOTAL_TIMEOUT ${SAVILEROW_TIME_UP}  \
    -mode ReadSolution \
    -out-minion      $MINION    \
    -minion-sol-file $MINION_SOLUTION \
    -out-solution    $EPRIME_SOLUTION \
    -out-aux         $AUX  1>/dev/null
date +'finSR2 %a %d %b %Y %k:%M:%S %z%nfinSR2(timestamp) %s' >&2


RESULTOF_SAVILEROW2=0
MSG_SAVILEROW2="{savilerow2}         $MSG_TEMPLATE"
echo "$MSG_SAVILEROW2"

RESULTOF_SAVILEROW2=$?

if (( RESULTOF_SAVILEROW2 != 0 )) ; then
    echo "$MSG_SAVILEROW2" >> "$FAIL_FILE"
    exit 1
fi

update_total_time "${TOTAL_TIMEOUT}" "${SAVILEROW_TIME_UP}"
echo "TOTAL_TIMEOUT is $TOTAL_TIMEOUT now"
echo "PREVIOUS_USED is $PREVIOUS_USED now"


MSG_SOLUTION_MISSING="{noSolution?}        $MSG_TEMPLATE"
if ! [ -f "$EPRIME_SOLUTION" ] ; then
    echo "${MSG_SOLUTION_MISSING}" >> "$FAIL_FILE"
    exit 1
fi



RESULTOF_TRANSLATESOLN=0
MSG_TRANSLATESOLN="{translateSolution}  $MSG_TEMPLATE"
echo "$MSG_TRANSLATESOLN"

date +'StartUP %a %d %b %Y %k:%M:%S %z%nStartUP(timestamp) %s' >&2
echoer \
${CPUTIMEOUT} --write-time ${TRANSLATESOLN_TIME} --previous-used $PREVIOUS_USED $TOTAL_TIMEOUT  \
conjure translate-solution                                          \
    --eprime              $EPRIME                                   \
    --essence-param       $PARAM                                    \
    --eprime-solution    $EPRIME_SOLUTION                           \
    --essence-solution  $ESSENCE_SOLUTION
date +'finUP %a %d %b %Y %k:%M:%S %z%nfinUP(timestamp) %s' >&2


RESULTOF_TRANSLATESOLN=$?
if (( RESULTOF_TRANSLATESOLN != 0 )) ; then
    echo "$MSG_TRANSLATESOLN" >> "$FAIL_FILE"
    exit 1
fi

# work around
grep -v 'new type of' $ESSENCE_SOLUTION > $ESSENCE_SOLUTION.tmp
mv $ESSENCE_SOLUTION.tmp $ESSENCE_SOLUTION


cat ${OUTPUT_BASE}/*.*-time \
    | grep cpu \
    | ruby -e 'p $stdin.readlines.map{|n| n[4..-1].to_f }.reduce(:+)' \
    > ${OUTPUT_BASE}/total.time


echo "TOTAL CPU TIME `cat ${OUTPUT_BASE}/total.time`"
touch $END_FILE
