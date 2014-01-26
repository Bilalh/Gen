#!/bin/bash
# Bilal Syed Hussain

set -o nounset

set -x
PARAM_BASE_NAME=$1
export PARAM_BASE_NAME

BASE=${2:-`pwd`}
cd $BASE
DIR=`basename $BASE`

echo $USE_MODE
export OUT_BASE_DIR=${OUT_BASE_DIR:-$BASE/out};

export NO_MINION_STATS=true;
export REPOSITORY_BASE="$OUT_BASE_DIR"
echo $USE_DATE
set +x

$PARAM_GEN_SCRIPTS/db/gather_data_separate.sh