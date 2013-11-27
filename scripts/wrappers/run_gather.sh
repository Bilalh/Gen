#!/bin/bash
# Bilal Syed Hussain

set -o nounset

PARAM_BASE_NAME=$1
export PARAM_BASE_NAME

BASE=${2:-`pwd`}
cd $BASE
MODE='df'
DIR=`basename $BASE`

export OUT_BASE_DIR=${OUT_BASE_DIR:-$BASE/out};
export USE_MODE="$MODE"

export NO_MINION_STATS=true;
export REPOSITORY_BASE="$OUT_BASE_DIR"
echo $USE_DATE

$PARAM_GEN_SCRIPTS/db/gather_data.sh