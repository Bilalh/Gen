#!/bin/bash
set -o nounset
export OUR="$( cd "$( dirname "$0" )" && pwd )";

set -x
pushd ..
export PARAM_GEN_SCRIPTS="$PWD/scripts/"
popd

export CORES=2
export USE_MODE=df
export OUT_BASE_DIR="$PWD/out"

# export MODELS_TO_USE=

now="$(date +%s)"
param_path="$PWD/./params/1.param"
time_per_model=10
working_dir="$PWD"

rm -rf out
mkdir out

"$PARAM_GEN_SCRIPTS/wrappers/run.sh" \
	"$now"  "${param_path}" "${time_per_model}" "${working_dir}"

set +x

