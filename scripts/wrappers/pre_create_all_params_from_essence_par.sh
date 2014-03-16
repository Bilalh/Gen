#!/bin/bash
set -o nounset

OUR_DIR="$( cd "$( dirname "$0" )" && pwd )";

TOTAL_TIMEOUT="${1}"
ESSENCE="${2}"
EPRIME="${3}"

# parallel -j${NUM_JOBS} "${OUR_DIR}/pre_create_all_params_from_essence.sh ${TOTAL_TIMEOUT} ${ESSENCE} ${EPRIME} {}" \
#     ::: ${PARAMS_DIR}/*.param

function record_time(){
    cat ${GENERATED_OUTPUT_DIR}/*.*-time \
        | grep cpu \
        | ruby -e 'p $stdin.readlines.map{|n| n[4..-1].to_f }.reduce(:+)' \
        > ${GENERATED_OUTPUT_DIR}/total.time
}

record_time
echo "TOTAL CPU TIME `cat ${GENERATED_OUTPUT_DIR}/total.time`"

pushd ${GENERATED_SOLUTIONS_DIR}
find . -name '*.solution.*' | parallel -j${NUM_JOBS} --keep-order  "mv {}  solution.param.{#}"
popd

find . -name '*.param.*'  | wc -l > ${GENERATED_SOLUTIONS_DIR}/solutions.count