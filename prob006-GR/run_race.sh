#!/bin/bash
set -o nounset
export OUR="$( cd "$( dirname "$0" )" && pwd )";

pushd ..
export PARAM_GEN_SCRIPTS="$PWD/scripts/"
popd

export NUM_JOBS=3
export USE_MODE=df
export OUT_BASE_DIR="$PWD/out"

# TODO specify the model ordering
# export MODELS_TO_USE=


now="$(date +%s)"
param_path="$PWD/./params/6e0c6b668.param"
time_per_model=60
working_dir="$PWD"

rm -rf out
mkdir out

"$PARAM_GEN_SCRIPTS/wrappers/run.sh" \
	"$now"  "${param_path}" "${time_per_model}" "${working_dir}"

export USE_DATE="${now}"
export TOTAL_TIMEOUT="${time_per_model}"
# Called param_hash in old script but seem to be the name of param file
# which happen to the hash
param_hash="6e0c6b668"

# Need to do this first globally
# cabal install split

"$PARAM_GEN_SCRIPTS/wrappers/run_gather.sh" \
	"${param_hash}" "${working_dir}"

