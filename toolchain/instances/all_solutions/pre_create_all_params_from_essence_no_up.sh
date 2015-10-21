#!/bin/bash
set -o nounset

# limit swap space used on my laptop
if [ "$(uname)" = "Darwin" ] ; then
    ulimit -Sv 2000000
fi

TOTAL_TIMEOUT=${1}
ESSENCE="$2"
EPRIME="$3"
PARAM="$4"

BASE="${GENERATED_OUTPUT_DIR}"
PARAM_NAME="$(basename "${PARAM}")"
PARAMBASE=${PARAM_NAME%.param}

EPRIME_PARAM="${BASE}/${PARAMBASE}.eprime-param"
MINION="${BASE}/${PARAMBASE}.minion"
MINION_SOLUTION="${BASE}/${PARAMBASE}.minion-solution"
MINION_TABLE="${BASE}/${PARAMBASE}.minion-table"

START_FILE="${BASE}/${PARAMBASE}.zstarted"
END_FILE="${BASE}/${PARAMBASE}.zfinished"
FAIL_FILE="${BASE}.fails"
MSG_TEMPLATE="$ESSENCE $(basename "${EPRIME}") $PARAM_NAME"

# print the command the run it
function echoer(){
    echo "$@"
    time "$@"
}

if [ -f "${END_FILE}" ]; then
    echo "NOT RUNNING $(basename "$END_FILE") exists *** $(basename "$ESSENCE") - $(basename "$EPRIME") - $(basename "$PARAM") ***"
    exit 0
fi

touch "$START_FILE"

RESULTOF_REFINEPARAM=0
MSG_REFINEPARAM="{refineParam}        $MSG_TEMPLATE"

echoer                            \
conjure refine-param              \
    --eprime  "$EPRIME"           \
    --essence-param "$PARAM"      \
    --eprime-param "$EPRIME_PARAM";

RESULTOF_REFINEPARAM=$?
echo "~~~ RESULTOF_REFINEPARAM ${RESULTOF_REFINEPARAM}"

if (( RESULTOF_REFINEPARAM != 0  )) ; then
    echo "$MSG_REFINEPARAM" >> "$FAIL_FILE"
    echo "error $MSG_REFINEPARAM"  >&2
    exit 2
fi

if [ ! -f "$EPRIME_PARAM" ]; then
    echo "$MSG_REFINEPARAM" >> "$FAIL_FILE"
    echo "error  $MSG_REFINEPARAM" >&2
    exit 3
fi


RESULTOF_SAVILEROW=0
MSG_SAVILEROW="{savilerow}          $MSG_TEMPLATE"

sr_dir="$(dirname "$(which savilerow)")"
echoer \
java -XX:ParallelGCThreads=1 -Xmx"${JAVA_MEMORY:-4G}" -server -ea -jar "$sr_dir/savilerow.jar" \
     -in-eprime "$EPRIME" -in-param "$EPRIME_PARAM" -out-minion "$MINION"


RESULTOF_SAVILEROW=$?
echo "~~~ RESULTOF_SAVILEROW ${RESULTOF_SAVILEROW}"

if (( RESULTOF_SAVILEROW != 0 )) ; then
    echo "$MSG_SAVILEROW" >> "$FAIL_FILE"
    echo "error $MSG_SAVILEROW" >&2
    exit 5
fi

if [ ! -f "$MINION" ]; then
    echo "$MSG_SAVILEROW" >> "$FAIL_FILE"
    echo "error $MSG_SAVILEROW" >&2
    exit 4
fi


RESULTOF_MINION=0
MSG_MINION="{minion}             $MSG_TEMPLATE"

echoer \
minion "$MINION"  \
    -noprintsols \
    -findallsols \
    -preprocess SACBounds \
    -tableout "$MINION_TABLE" \
    -solsout  "$MINION_SOLUTION" \
    -cpulimit "${TOTAL_TIMEOUT}"

RESULTOF_MINION=$?
if (( RESULTOF_MINION != 0 )) ; then
    echo "$MSG_MINION" >> "$FAIL_FILE"
    echo "error $MSG_MINION" >&2
    exit 1
fi

if [  ! -f "${MINION_TABLE}" ]; then
    echo "$MSG_MINION" >> "$FAIL_FILE"
    echo "error $MSG_MINION" >&2
    exit 1
fi

touch "$END_FILE"
echo "Finished successfully"
