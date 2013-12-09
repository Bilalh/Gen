#/bin/bash
set -o nounset

# limit swap space used on my laptop
if [ "$(uname)" = "Darwin" ] ; then
    ulimit -Sv 2000000
fi

ESSENCE="$1"
EPRIME="$2"
PARAM="$3"
MINION_TIMEOUT=${4}
TOTAL_TIMEOUT=${5}

DIR=$(dirname $ESSENCE)
EPRIMEBASE=${EPRIME%.eprime}

OUTPUT_BASE=${GENERATED_OUTPUT_DIR:-}
[ ! "${OUTPUT_BASE}" == "" ] && EPRIMEBASE="${OUTPUT_BASE}/`basename ${EPRIMEBASE}`"


PARAM_NAME="`basename ${PARAM}`"
PARAMBASE=${PARAM_NAME%.param}



EPRIME_PARAM="${EPRIMEBASE}-${PARAMBASE}.eprime-param"
MINION="${EPRIMEBASE}-${PARAMBASE}.minion"
MINION_SOLUTION="${EPRIMEBASE}-${PARAMBASE}.minion-solution"
MINION_TABLE="${EPRIMEBASE}-${PARAMBASE}.minion-table"
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

echo ""
echo "*** `basename $ESSENCE` - `basename $EPRIME` - `basename $PARAM` ***"
#"

RESULTOF_REFINEPARAM=0
MSG_REFINEPARAM="{refineParam}       $MSG_TEMPLATE"
echo "$MSG_REFINEPARAM"

# print the command the run it
function echoer(){
    echo "$@"
    time "$@"
}

echoer \
conjure                                                            \
    --mode       refineParam                                       \
    --in-essence $ESSENCE                                          \
    --in-eprime  $EPRIME                                           \
    --in-essence-param $PARAM                                      \
    --out-eprime-param $EPRIME_PARAM;

RESULTOF_REFINEPARAM=$?
if (( $RESULTOF_REFINEPARAM != 0 )) ; then
    echo "$MSG_REFINEPARAM" >> "$FAIL_FILE"
    exit 1
fi


RESULTOF_SAVILEROW=0
MSG_SAVILEROW="{savilerow}         $MSG_TEMPLATE"
echo "$MSG_SAVILEROW"



echoer \
savilerow -mode Normal \
    -in-eprime    $EPRIME       \
    -in-param     $EPRIME_PARAM \
    -out-minion   $MINION       \
    -boundvars;

RESULTOF_SAVILEROW=$?

if (( $RESULTOF_SAVILEROW != 0 )) ; then
    echo "$MSG_SAVILEROW" >> "$FAIL_FILE"
    exit 1
fi

RESULTOF_MINION=0
MSG_MINION="{minion}         $MSG_TEMPLATE"
echo "$MSG_MINION"


echoer \
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

echoer \
savilerow -mode ReadSolution \
    -out-minion      $MINION    \
    -minion-sol-file $MINION_SOLUTION \
    -out-solution    $EPRIME_SOLUTION


RESULTOF_SAVILEROW2=0
MSG_SAVILEROW2="{savilerow2}         $MSG_TEMPLATE"
echo "$MSG_SAVILEROW2"

RESULTOF_SAVILEROW2=$?

if (( $RESULTOF_SAVILEROW2 != 0 )) ; then
    echo "$MSG_SAVILEROW2" >> "$FAIL_FILE"
    exit 1
fi

touch $END_FILE


MSG_SOLUTION_MISSING="{noSolution?}        $MSG_TEMPLATE"
if ! [ -f $EPRIME_SOLUTION ] ; then
	echo "${MSG_SOLUTION_MISSING}" >> "$FAIL_FILE"
	exit 1
fi


if [ !  -n "${NO_TRANSLATE:-}" ]; then

RESULTOF_TRANSLATESOLN=0
MSG_TRANSLATESOLN="{translateSolution} $MSG_TEMPLATE"
echo "$MSG_TRANSLATESOLN"

echoer   \
conjure                                                                 \
    --mode translateSolution                                            \
    --in-essence            $ESSENCE                                    \
    --in-essence-param      $PARAM                                      \
    --in-eprime             $EPRIME                                     \
    --in-eprime-param       $EPRIME_PARAM                               \
    --in-eprime-solution    $EPRIME_SOLUTION                            \
    --out-essence-solution  $ESSENCE_SOLUTION


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


echoer   \
conjure                                                                 \
    --mode validateSolution                                             \
    --in-essence  $ESSENCE                                              \
    --in-param    $PARAM                                                \
    --in-solution $ESSENCE_SOLUTION


RESULTOF_VALIDATESOLN=$?
if (( $RESULTOF_VALIDATESOLN != 0 )) ; then
	echo "$MSG_VALIDATESOLN" >> "$FAIL_FILE"
	exit 1
else
	echo "$MSG_VALIDATESOLN" >> "$SUCCESS_FILE"
fi

fi


