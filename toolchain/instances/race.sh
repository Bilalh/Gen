#!/bin/bash
# Bilal Syed Hussain
set -o nounset
OUR="$( cd "$( dirname "$0" )" && pwd )";


set -x
export USE_DATE="$1";
export PARAMS_TO_USE="$2";
cutoff_time="${3}"

BASE="${4}"
DIR="$(basename "$BASE")"
cd "$BASE" || exit

mkdir -p "$OUT_BASE_DIR"

echo "$USE_MODE"
set +x


export NO_MINION_STATS=true;
export NO_TIMERS=true;
export NO_VALIDATE=true;
export NO_TRANSLATE=true
echo "PWD: $PWD"

if [ "${LIMIT_MODELS:-}" ]; then
	models="$( echo "${MODELS_TO_USE:-$(ls -1 "$BASE/${DIR}_${USE_MODE}/"*.eprime)}"  | head -n "${LIMIT_MODELS}" )"
    echo "LIMITING to running ${LIMIT_MODELS} models"
else
	models=${MODELS_TO_USE:-$(ls -1 "$BASE/${DIR}_${USE_MODE}/"*.eprime)}
fi

export MODELS_TO_USE="${models}";
if [ "$(uname)" = "Darwin" ] ; then
    export NUM_JOBS=${NUM_JOBS:-4}
else
    export NUM_JOBS=${NUM_JOBS:-32}
fi


"${OUR}/race/timeModel.sh" "$cutoff_time" "$cutoff_time"
date
