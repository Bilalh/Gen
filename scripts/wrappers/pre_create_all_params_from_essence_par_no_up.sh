#!/bin/bash
set -o nounset

OUR_DIR="$( cd "$( dirname "$0" )" && pwd )";

TOTAL_TIMEOUT="${1}"
ESSENCE="${2}"
EPRIME="${3}"

parallel -j${NUM_JOBS} "${OUR_DIR}/pre_create_all_params_from_essence_no_up.sh ${TOTAL_TIMEOUT} ${ESSENCE} ${EPRIME} {}" \
    ::: ${PARAMS_DIR}/*.param

function record_time(){
    cat ${GENERATED_OUTPUT_DIR}/*.*-time \
        | grep cpu \
        | ruby -e 'p $stdin.readlines.map{|n| n[4..-1].to_f }.reduce(:+)' \
        > ${GENERATED_OUTPUT_DIR}/total.time
}

record_time
echo "TOTAL CPU TIME `cat ${GENERATED_OUTPUT_DIR}/total.time`"




mv ${GENERATED_OUTPUT_DIR}/*.minion-solution \
	 ${GENERATED_OUTPUT_DIR}/*.minion.aux \
	 ${GENERATED_OUTPUT_DIR}/*.eprime-param \
${GENERATED_SOLUTIONS_DIR}

mv 	 ${GENERATED_OUTPUT_DIR}/total.time ${GENERATED_OUTPUT_DIR}/../total.time

pushd ${GENERATED_OUTPUT_DIR}/..
tar -c "all_sols_data" | pigz -c -p"${NUM_JOBS}" > all_sols_data.tar.gz
rm -rf all_sols_data
popd

pushd ${GENERATED_SOLUTIONS_DIR}
wc -l *.minion-solution | sed '$ d' > solutions.counts
popd