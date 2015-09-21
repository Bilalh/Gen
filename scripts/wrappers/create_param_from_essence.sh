#!/bin/bash
set -o nounset

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )";
Time="/usr/bin/time -p"

# limit swap space used on my laptop
if [ "$(uname)" = "Darwin" ] ; then
    ulimit -Sv 2000000
fi

ESSENCE="$1"
EPRIME="$2"
PARAM="$3"
MINION_TIMEOUT=${4}  # not used anymore
TOTAL_TIMEOUT=${5}
RANDOM_SEED=${6}

DIR=$(dirname $ESSENCE)
EPRIMEBASE=${EPRIME%.eprime}

OUTPUT_BASE=${GENERATED_OUTPUT_DIR}
[ ! "${OUTPUT_BASE}" == "" ] && EPRIMEBASE="${OUTPUT_BASE}/`basename ${EPRIMEBASE}`"


PARAM_NAME="`basename ${PARAM}`"
PARAMBASE=${PARAM_NAME%.param}


START_FILE="${EPRIMEBASE}-${PARAMBASE}.zstarted"
END_FILE="${EPRIMEBASE}-${PARAMBASE}.zfinished"

EPRIME_PARAM="${EPRIMEBASE}-${PARAMBASE}.eprime-param"
EPRIME_SOLUTION="${EPRIMEBASE}-${PARAMBASE}.eprime-solution"
ESSENCE_SOLUTION="${EPRIMEBASE}-${PARAMBASE}.solution"

MINION="${EPRIMEBASE}-${PARAMBASE}.minion"
MINION_SOLUTION="${EPRIMEBASE}-${PARAMBASE}.minion-solution"
MINION_STATS="${EPRIMEBASE}-${PARAMBASE}.minion-stats"
MINION_TABLE="${EPRIMEBASE}-${PARAMBASE}.minion-table"
MINION_TIME="${EPRIMEBASE}-${PARAMBASE}.minion-time"

REFINE_TIME="${EPRIMEBASE}-${PARAMBASE}.param-time"
SAVILEROW_TIME="${EPRIMEBASE}-${PARAMBASE}.sr-time"

SAVILEROW_TIME_UP="${EPRIMEBASE}-${PARAMBASE}.sr2-time"
TRANSLATESOLN_TIME="${EPRIMEBASE}-${PARAMBASE}.up-time"


touch $START_FILE


MSG_TEMPLATE="$ESSENCE `basename ${EPRIME}` $PARAM_NAME"
TIME_TEMPLATE="${EPRIMEBASE}-${PARAMBASE}.time"

FAIL_FILE="${EPRIMEBASE}.fails"

echo "TIMEOUT5_FILE is ${TIMEOUT5_FILE}"
CPUTIMEOUT="${SCRIPT_DIR}/../tools/cputimeout/cputimeout --timeout-file $TIMEOUT5_FILE --interval 1 -k1"

PREVIOUS_USED=0

echo ""
echo "*** `basename $ESSENCE` - `basename $EPRIME` - `basename $PARAM` ***"
#"

RESULTOF_REFINEPARAM=0
MSG_REFINEPARAM="{refineParam}        $MSG_TEMPLATE"
echo "$MSG_REFINEPARAM"

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

date +'StartP %a %d %b %Y %k:%M:%S %z%nStartP(timestamp) %s' >&2

echoer \
${CPUTIMEOUT} --write-time ${REFINE_TIME} --previous-used $PREVIOUS_USED $TOTAL_TIMEOUT  \
conjure refine-param                                                            \
    --eprime  $EPRIME                                           \
    --essence-param $PARAM                                      \
    --eprime-param $EPRIME_PARAM;

RESULTOF_REFINEPARAM=$?

date +'finP %a %d %b %Y %k:%M:%S %z%nfinP(timestamp) %s' >&2

if [ ! -f $EPRIME_PARAM ]; then
    echo "$MSG_REFINEPARAM" >> "$FAIL_FILE"
    exit 1
fi

update_total_time ${TOTAL_TIMEOUT} ${REFINE_TIME}

if (( $RESULTOF_REFINEPARAM != 0 || $TOTAL_TIMEOUT <= 0 )) ; then
    echo "$MSG_REFINEPARAM" >> "$FAIL_FILE"
    exit 1
fi

echo "TOTAL_TIMEOUT is $TOTAL_TIMEOUT now"
echo "PREVIOUS_USED is $PREVIOUS_USED now"


RESULTOF_SAVILEROW=0
MSG_SAVILEROW="{savilerow}          $MSG_TEMPLATE"
echo "$MSG_SAVILEROW"


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

date +'StartSR %a %d %b %Y %k:%M:%S %z%nStartSR(timestamp) %s' >&2
savilerow $TOTAL_TIMEOUT ${SAVILEROW_TIME} -mode Normal \
    -in-eprime    $EPRIME       \
    -in-param     $EPRIME_PARAM \
    -out-minion   $MINION       \

date +'finSR %a %d %b %Y %k:%M:%S %z%nfinSR(timestamp) %s' >&2


RESULTOF_SAVILEROW=$?

if [ ! -f $MINION ]; then
    echo "$MSG_SAVILEROW" >> "$FAIL_FILE"
    exit 1
fi

update_total_time ${TOTAL_TIMEOUT} ${SAVILEROW_TIME}

if (( $RESULTOF_SAVILEROW != 0 || $TOTAL_TIMEOUT <= 0 )) ; then
    echo "$MSG_SAVILEROW" >> "$FAIL_FILE"
    exit 1
fi

echo "TOTAL_TIMEOUT is $TOTAL_TIMEOUT now"
echo "PREVIOUS_USED is $PREVIOUS_USED now"


RESULTOF_MINION=0
MSG_MINION="{minion}             $MSG_TEMPLATE"
echo "$MSG_MINION"


date +'StartMINION %a %d %b %Y %k:%M:%S %z%nStartMINION(timestamp) %s' >&2
echoer \
${CPUTIMEOUT} --write-time $MINION_TIME --previous-used $PREVIOUS_USED $TOTAL_TIMEOUT \
minion $MINION  \
    -noprintsols \
    -preprocess SACBounds \
    -tableout $MINION_TABLE \
    -solsout  $MINION_SOLUTION \
    -randomiseorder \
    -randomseed ${RANDOM_SEED}

date +'finMINION %a %d %b %Y %k:%M:%S %z%nfinMINION(timestamp) %s' >&2


RESULTOF_MINION=$?
if (( $RESULTOF_MINION != 0 )) ; then
    echo "$MSG_MINION" >> "$FAIL_FILE"
    exit 1
fi

if [  ! -f ${MINION_TABLE} ]; then
    echo "$MSG_MINION" >> "$FAIL_FILE"
    exit 1
fi

update_total_time ${TOTAL_TIMEOUT} ${MINION_TIME}
echo "TOTAL_TIMEOUT is $TOTAL_TIMEOUT now"
echo "PREVIOUS_USED is $PREVIOUS_USED now"

date +'StartSR2 %a %d %b %Y %k:%M:%S %z%nStartSR2(timestamp) %s' >&2
savilerow  $TOTAL_TIMEOUT ${SAVILEROW_TIME_UP}  \
    -mode ReadSolution \
    -out-minion      $MINION    \
    -minion-sol-file $MINION_SOLUTION \
    -out-solution    $EPRIME_SOLUTION 1>/dev/null
date +'finSR2 %a %d %b %Y %k:%M:%S %z%nfinSR2(timestamp) %s' >&2


RESULTOF_SAVILEROW2=0
MSG_SAVILEROW2="{savilerow2}         $MSG_TEMPLATE"
echo "$MSG_SAVILEROW2"

RESULTOF_SAVILEROW2=$?

if (( $RESULTOF_SAVILEROW2 != 0 )) ; then
    echo "$MSG_SAVILEROW2" >> "$FAIL_FILE"
    exit 1
fi

update_total_time ${TOTAL_TIMEOUT} ${SAVILEROW_TIME_UP}
echo "TOTAL_TIMEOUT is $TOTAL_TIMEOUT now"
echo "PREVIOUS_USED is $PREVIOUS_USED now"


MSG_SOLUTION_MISSING="{noSolution?}        $MSG_TEMPLATE"
if ! [ -f $EPRIME_SOLUTION ] ; then
    echo "${MSG_SOLUTION_MISSING}" >> "$FAIL_FILE"
    exit 1
fi



RESULTOF_TRANSLATESOLN=0
MSG_TRANSLATESOLN="{translateSolution}  $MSG_TEMPLATE"
echo "$MSG_TRANSLATESOLN"

date +'StartUP %a %d %b %Y %k:%M:%S %z%nStartUP(timestamp) %s' >&2
echoer \
${CPUTIMEOUT} --write-time ${TRANSLATESOLN_TIME} --previous-used $PREVIOUS_USED $TOTAL_TIMEOUT  \
conjure translateSolution                                           \
    --eprime              $EPRIME                                   \
    --essence-param       $PARAM                                    \
    --eprime-solution    $EPRIME_SOLUTION                           \
    --essence-solution  $ESSENCE_SOLUTION
date +'finUP %a %d %b %Y %k:%M:%S %z%nfinUP(timestamp) %s' >&2


RESULTOF_TRANSLATESOLN=$?
if (( $RESULTOF_TRANSLATESOLN != 0 )) ; then
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
