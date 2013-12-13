#/bin/bash
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

DIR=$(dirname $ESSENCE)
EPRIMEBASE=${EPRIME%.eprime}

OUTPUT_BASE=${GENERATED_OUTPUT_DIR:-}
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
touch $START_FILE


MSG_TEMPLATE="$ESSENCE $EPRIME $PARAM"
TIME_TEMPLATE="${EPRIMEBASE}-${PARAMBASE}.time"

FAIL_FILE="${EPRIMEBASE}.fails"
SUCCESS_FILE="${EPRIMEBASE}.success"

CPUTIMEOUT="${SCRIPT_DIR}/../tools/cputimeout/cputimeout --timeout-file $TIMEOUT5_FILE --interval 1 -k5"

PREVIOUS_USED=0

echo ""
echo "*** `basename $ESSENCE` - `basename $EPRIME` - `basename $PARAM` ***"
#"

RESULTOF_REFINEPARAM=0
MSG_REFINEPARAM="{refineParam}       $MSG_TEMPLATE"
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
    TOTAL_TIMEOUT="$(echo "(${cur}-${taken})/1" | bc)"
    set +x
    taken="$(echo "${taken}/1" | bc )"
    (( PREVIOUS_USED += taken ))
}

echoer \
${CPUTIMEOUT} --write-time ${REFINE_TIME} --previous-used $PREVIOUS_USED $TOTAL_TIMEOUT  \
conjure                                                            \
    --mode       refineParam                                       \
    --in-essence $ESSENCE                                          \
    --in-eprime  $EPRIME                                           \
    --in-essence-param $PARAM                                      \
    --out-eprime-param $EPRIME_PARAM;

RESULTOF_REFINEPARAM=$?
if (( $RESULTOF_REFINEPARAM != 0 || $TOTAL_TIMEOUT <= 0 )) ; then
    echo "$MSG_REFINEPARAM" >> "$FAIL_FILE"
    exit 1
fi

if [ ! -f $EPRIME_PARAM ]; then
    echo "$MSG_REFINEPARAM" >> "$FAIL_FILE"
    exit 1
fi

update_total_time ${TOTAL_TIMEOUT} ${REFINE_TIME}
echo "TOTAL_TIMEOUT is $TOTAL_TIMEOUT now"
echo "PREVIOUS_USED is $PREVIOUS_USED now"


RESULTOF_SAVILEROW=0
MSG_SAVILEROW="{savilerow}         $MSG_TEMPLATE"
echo "$MSG_SAVILEROW"


function savilerow(){
    timeout=$1
    shift
    sr_dir="`dirname $(which savilerow)`"
    echo "savilerow $@"
    echo "java -XX:ParallelGCThreads=1 -Xmx${JAVA_MEMORY:-2G}  -server -ea -jar $sr_dir/savilerow.jar $@"

    $Time ${CPUTIMEOUT} --write-time ${SAVILEROW_TIME} --previous-used $PREVIOUS_USED $timeout \
    java -XX:ParallelGCThreads=1 -Xmx${JAVA_MEMORY:-2G}  -server -ea -jar $sr_dir/savilerow.jar $@
}

savilerow $TOTAL_TIMEOUT -mode Normal \
    -in-eprime    $EPRIME       \
    -in-param     $EPRIME_PARAM \
    -out-minion   $MINION       \
    -boundvars;

RESULTOF_SAVILEROW=$?

if (( $RESULTOF_SAVILEROW != 0 || $TOTAL_TIMEOUT <= 0 )) ; then
    echo "$MSG_SAVILEROW" >> "$FAIL_FILE"
    exit 1
fi

if [ ! -f $MINION ]; then
    echo "$MSG_SAVILEROW" >> "$FAIL_FILE"
    exit 1
fi

update_total_time ${TOTAL_TIMEOUT} ${SAVILEROW_TIME}
echo "TOTAL_TIMEOUT is $TOTAL_TIMEOUT now"
echo "PREVIOUS_USED is $PREVIOUS_USED now"


RESULTOF_MINION=0
MSG_MINION="{minion}         $MSG_TEMPLATE"
echo "$MSG_MINION"


echoer \
${CPUTIMEOUT} --write-time $MINION_TIME $TOTAL_TIMEOUT --previous-used $PREVIOUS_USED  \
minion $MINION  \
    -printsolsonly \
    -preprocess SACBounds \
    -tableout $MINION_TABLE \
    -solsout  $MINION_SOLUTION

RESULTOF_MINION=$?

if (( $RESULTOF_MINION != 0 )) ; then
    echo "$MSG_MINION" >> "$FAIL_FILE"
    exit 1
fi


