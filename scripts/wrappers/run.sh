#!/bin/bash
# Bilal Syed Hussain
set -o nounset


BASE=${4:-`pwd`}
DIR=`basename $BASE`
cd $BASE

set -x
cutoff_time=$3
export USE_DATE="$1";


export OUT_BASE_DIR=${OUT_BASE_DIR:-$BASE/out};
mkdir -p $OUT_BASE_DIR

export PARAMS_TO_USE="$2";
export USE_MODE='df'
set +x


export NO_MINION_STATS=true;
export NO_TIMERS=true;
export NO_VALIDATE=true;
export NO_TRANSLATE=true

# 6 SR/minon on my mac, 31 on servers
if [ "$(uname)" = "Darwin" ] ; then
    export NUM_JOBS=${NUM_JOBS:-6}
    export MODELS_TO_USE=`ls -1 $BASE/$DIR-$USE_MODE/*.eprime | head -n 5`;
else
    export NUM_JOBS=${NUM_JOBS:-31}
    export MODELS_TO_USE=`ls -1 $BASE/$DIR-$USE_MODE/*.eprime`;
fi


$PARAM_GEN_SCRIPTS/run/timeModel.sh $cutoff_time $cutoff_time
set -x
echo "`date`"
set +x