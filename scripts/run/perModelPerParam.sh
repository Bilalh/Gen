#/bin/bash
set -o nounset

# limit swap space used on my laptop
if [ "$(uname)" = "Darwin" ] ; then
    ulimit -Sv 2000000
fi

ESSENCE="$1"
EPRIME="$2"
PARAM="$3"
MINION_TIMEOUT=${4:-600}
TOTAL_TIMEOUT=${5:-800}

DIR=$(dirname $ESSENCE)
EPRIMEBASE=${EPRIME%.eprime}

OUTPUT_BASE=${GENERATED_OUTPUT_DIR:-}
[ ! "${OUTPUT_BASE}" == "" ] && EPRIMEBASE="${OUTPUT_BASE}/`basename ${EPRIMEBASE}`"


PARAM_NAME="`basename ${PARAM}`"
PARAMBASE=${PARAM_NAME%.param}



EPRIME_PARAM="${EPRIMEBASE}-${PARAMBASE}.eprime-param"
MINION="${EPRIMEBASE}-${PARAMBASE}.minion"
MINION_STATS="${EPRIMEBASE}-${PARAMBASE}.minion-stats"
EPRIME_SOLUTION="${EPRIMEBASE}-${PARAMBASE}.eprime-solution"
ESSENCE_SOLUTION="${EPRIMEBASE}-${PARAMBASE}.solution"
END_FILE="${EPRIMEBASE}-${PARAMBASE}.zfinished"
START_FILE="${EPRIMEBASE}-${PARAMBASE}.zstarted"
touch $START_FILE


MSG_TEMPLATE="$ESSENCE $EPRIME $PARAM"
TIME_TEMPLATE="${EPRIMEBASE}-${PARAMBASE}.time"

FAIL_FILE="${EPRIMEBASE}.fails"
SUCCESS_FILE="${EPRIMEBASE}.success"


timeout_file=${TIMEOUT5_FILE:-"${OUT_BASE_DIR:-.}/timeout5PerModelPerParam"}
#"

RESULTOF_REFINEPARAM=0
MSG_REFINEPARAM="{refineParam}       $MSG_TEMPLATE"
echo "$MSG_REFINEPARAM"

set -x
{
time conjure                                                                 \
    --mode       refineParam                                            \
    --in-essence $ESSENCE                                               \
    --in-eprime  $EPRIME                                                \
    --in-essence-param $PARAM                                           \
    --out-eprime-param $EPRIME_PARAM;
}
set +x

RESULTOF_REFINEPARAM=$?
if (( $RESULTOF_REFINEPARAM != 0 )) ; then
    echo "$MSG_REFINEPARAM" >> "$FAIL_FILE"
    exit 1
fi


RESULTOF_SAVILEROW=0
MSG_SAVILEROW="{savilerow}         $MSG_TEMPLATE"
echo "$MSG_SAVILEROW"


if [ -z "${NO_TIMERS:-}" ]; then
	timer="${PARAM_GEN_SCRIPTS}/tools/timeout5 --timeout-file $timeout_file ${TOTAL_TIMEOUT}"
else
	timer=""
fi

set -x
{
time \
${timer} \
savilerow                                                               \
    -in-eprime    $EPRIME                                               \
    -in-param     $EPRIME_PARAM                                         \
    -out-minion   $MINION                                               \
    -out-solution $EPRIME_SOLUTION                                      \
    -m minion                                                           \
    -boundvars                                                          \
    -minion-options "-timelimit ${MINION_TIMEOUT}";
}
set +x

RESULTOF_SAVILEROW=$?

touch $END_FILE

RESULTOF_MINIONSTATS=0
if [ !  -n "${NO_MINION_STATS:-}" ]; then
    MSG_MINIONSTATS="{minionStats}       $MSG_TEMPLATE"
    echo "$MSG_MINIONSTATS"
    ${timer} minion -instancestats $MINION > $MINION_STATS
    RESULTOF_MINIONSTATS=$?
fi

if (( $RESULTOF_SAVILEROW != 0 )) ; then
	echo "$MSG_SAVILEROW" >> "$FAIL_FILE"
	exit 1
fi

fail=0
if (( $RESULTOF_MINIONSTATS != 0 )) ; then
    fail=1
fi

MSG_SOLUTION_MISSING="{noSolution?}        $MSG_TEMPLATE"
if ! [ -f $EPRIME_SOLUTION ] ; then
	echo "${MSG_SOLUTION_MISSING}" >> "$FAIL_FILE"
	exit 1
fi


if [ !  -n "${NO_TRANSLATE:-}" ]; then

RESULTOF_TRANSLATESOLN=0
MSG_TRANSLATESOLN="{translateSolution} $MSG_TEMPLATE"
echo "$MSG_TRANSLATESOLN"

set -x
{ time \
${timer} \
conjure                                                                 \
    --mode translateSolution                                            \
    --in-essence            $ESSENCE                                    \
    --in-essence-param      $PARAM                                      \
    --in-eprime             $EPRIME                                     \
    --in-eprime-param       $EPRIME_PARAM                               \
    --in-eprime-solution    $EPRIME_SOLUTION                            \
    --out-essence-solution  $ESSENCE_SOLUTION
}
set +x

RESULTOF_TRANSLATESOLN=$?
if (( $RESULTOF_TRANSLATESOLN != 0 )) ; then
	echo "$MSG_TRANSLATESOLN" >> "$FAIL_FILE"
	exit 1
else
	echo "${MSG_TEMPLATE}" >> "$SUCCESS_FILE"
fi

fi

if [ !  -n "${NO_VALIDATE:-}" ]; then
RESULTOF_VALIDATESOLN=0
MSG_VALIDATESOLN="{validateSolution} $MSG_TEMPLATE"
echo "$MSG_VALIDATESOLN"


set -x
{ time \
${timer} \
conjure                                                                 \
    --mode validateSolution                                             \
    --in-essence  $ESSENCE                                              \
    --in-param    $PARAM                                                \
    --in-solution $ESSENCE_SOLUTION
}
set +x

RESULTOF_VALIDATESOLN=$?
if (( $RESULTOF_VALIDATESOLN != 0 )) ; then
	echo "$MSG_VALIDATESOLN" >> "$FAIL_FILE"
	exit 1
else
	echo "$MSG_VALIDATESOLN" >> "$SUCCESS_FILE"
fi

fi

if (( $fail == 1)); then
	exit 1
fi

