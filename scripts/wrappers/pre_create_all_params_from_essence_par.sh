#!/bin/bash
set -o nounset

OUR_DIR="$( cd "$( dirname "$0" )" && pwd )";

TOTAL_TIMEOUT="${1}"
ESSENCE="${2}"
EPRIME="${3}"

parallel -j1  "${OUR_DIR}/pre_create_all_params_from_essence.sh ${TOTAL_TIMEOUT} ${ESSENCE} ${EPRIME} {}" ::: ${PARAMS_DIR}/1-1.param