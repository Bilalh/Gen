#!/bin/bash
set -o nounset
set -e

OUR_DIR="$( cd "$( dirname "$0" )" && pwd )";

TOTAL_TIMEOUT="${1}"
ESSENCE="${2}"
EPRIME="${3}"

[ -f "${ESSENCE}" ] || (echo "${ESSENCE} missing" exit 1)
[ -f "${EPRIME}" ]  || (echo "${EPRIME} missing" exit 1)

function check_variables(){
    echo ""
    echo "                *** Variables ***"
    var_err=0
    for var in "${vars_needed[@]}"; do
        val="${!var:-MISSING}"
        printf "%-24s: %s\n" "$var" "${val}"
        if [ "$val" = "MISSING" ]; then
            var_err=1
        fi
    done
    return $var_err
}

export vars_needed=(GENERATED_OUTPUT_DIR GENERATED_SOLUTIONS_DIR PARAMS_DIR NUM_JOBS TOTAL_TIMEOUT ESSENCE EPRIME )
if ( ! check_variables ) then
    echo ""
    echo "Error: missing variables" >&2
    exit 1
fi

mkdir -p "${GENERATED_OUTPUT_DIR}"
mkdir -p "${GENERATED_SOLUTIONS_DIR}"

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
	 ${GENERATED_OUTPUT_DIR}/*.eprime-param.aux \
	 ${GENERATED_OUTPUT_DIR}/*.eprime-param \
${GENERATED_SOLUTIONS_DIR}

mv 	 ${GENERATED_OUTPUT_DIR}/total.time ${GENERATED_OUTPUT_DIR}/../total.time

pushd ${GENERATED_OUTPUT_DIR}/..
base_out="$(basename "${GENERATED_OUTPUT_DIR}")"
tar -c "${base_out}" | pigz -c -p"${NUM_JOBS}" > "${base_out}".tar.gz
rm -rf "${base_out}"
popd

pushd ${GENERATED_SOLUTIONS_DIR}
wc -l *.minion-solution | sed '$ d' > solutions.counts
popd
