#!/bin/bash
set -o nounset

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )";
Time="/usr/bin/time -p"

# limit swap space used on my laptop
if [ "$(uname)" = "Darwin" ] ; then
    ulimit -Sv 2000000
fi

TOTAL_TIMEOUT=${1}
ESSENCE="$2"
EPRIME="$3"
PARAM="$4"

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


REFINE_TIME="${EPRIMEBASE}-${PARAMBASE}.param-time"
SAVILEROW_TIME="${EPRIMEBASE}-${PARAMBASE}.sr-time"


touch $START_FILE


MSG_TEMPLATE="$ESSENCE `basename ${EPRIME}` $PARAM_NAME"
TIME_TEMPLATE="${EPRIMEBASE}-${PARAMBASE}.time"

FAIL_FILE="${EPRIMEBASE}.fails"

CPUTIMEOUT="${SCRIPT_DIR}/../tools/cputimeout/cputimeout --interval 1 -k1"

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
conjure                                                            \
    --mode       refineParam                                       \
    --in-essence $ESSENCE                                          \
    --in-eprime  $EPRIME                                           \
    --in-essence-param $PARAM                                      \
    --out-eprime-param $EPRIME_PARAM;

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
    java -XX:ParallelGCThreads=1 -Xmx${JAVA_MEMORY:-4G}  -server -ea -jar $sr_dir/savilerow.jar "$@"
}

date +'StartSR %a %d %b %Y %k:%M:%S %z%nStartSR(timestamp) %s' >&2
savilerow $TOTAL_TIMEOUT ${SAVILEROW_TIME} -mode Normal \
    -in-eprime    $EPRIME       \
    -in-param     $EPRIME_PARAM \
    -run-minion minion \
    -out-minion   $MINION       \
    -out-solution $EPRIME_SOLUTION \
    -all-solutions \
    -boundvars;

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


cmd="
sol=\$( echo '{/}' | sed -e 's/eprime-solution/solution/g'  );
conjure
    --mode translateSolution
    --in-eprime $EPRIME
    --in-eprime-param $EPRIME_PARAM
    --in-essence $ESSENCE
    --in-eprime-solution {} \
    --in-essence-param $PARAM
    --out-solution ${GENERATED_SOLUTIONS_DIR}/\${sol}"
echo $cmd
parallel --tagstring "{#}" --keep-order -j${NUM_JOBS_ALL_SOLS:-1} $cmd ::: ${EPRIME_SOLUTION}.*


touch $END_FILE
