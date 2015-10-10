#!/bin/bash
# Bilal Syed Hussain
set -o nounset
OUR="$( cd "$( dirname "$0" )" && pwd )";

set -x
PARAM_BASE_NAME=$1
export PARAM_BASE_NAME

BASE="$2"
cd "$BASE"
DIR=`basename $BASE`

echo "$USE_MODE"
echo "${OUT_BASE_DIR}";

export NO_MINION_STATS=true;
export REPOSITORY_BASE="$OUT_BASE_DIR"
echo "$USE_DATE"
set +x

stats_dir="${OUT_BASE_DIR}/stats_${USE_MODE}"
"${OUR}/db/gather_data_separate.sh" 2>&1 | tee "${stats_dir}/${USE_DATE}.output-gather"

