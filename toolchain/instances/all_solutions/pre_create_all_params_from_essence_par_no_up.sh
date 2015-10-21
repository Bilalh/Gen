#!/bin/bash
set -o nounset
set -e

OUR_DIR="$( cd "$( dirname "$0" )" && pwd )";

TOTAL_TIMEOUT="${1}"
ESSENCE="${2}"
EPRIME="${3}"
out_base="${4}"

function check_variables(){
    # check that the variables specified by the array vars_needed are set.
    # Return return 0 only if they are >0 if there they are not

    echo ""
    echo "                *** Variables ***"
    var_err=0
    for var in "${vars_needed[@]}"; do
        val="${!var:-MISSING}"
        printf "%-24s: %s\n" "$var" "${val}"
        if [ "$val" = "MISSING" ]; then
            var_err+=1
        fi
    done
    return $var_err
}

export GENERATED_OUTPUT_DIR="${out_base}/solve_data"
export GENERATED_SOLUTIONS_DIR="${out_base}/all_sols"
export PARAMS_DIR="${out_base}/_params"

export vars_needed=(GENERATED_OUTPUT_DIR GENERATED_SOLUTIONS_DIR PARAMS_DIR NUM_JOBS TOTAL_TIMEOUT ESSENCE EPRIME )
if ( ! check_variables ) then
    echo ""
    echo "Error: missing variables" >&2
    exit 1
fi

[ -f "${ESSENCE}" ] || (echo "${ESSENCE} missing" exit 1)
[ -f "${EPRIME}" ]  || (echo "${EPRIME} missing" exit 1)

mkdir -p "${GENERATED_OUTPUT_DIR}"
mkdir -p "${GENERATED_SOLUTIONS_DIR}"

# Do the refinement in parallel
parallel --tagstring "{/.}"  \
	-j"${NUM_JOBS}" "${OUR_DIR}/pre_create_all_params_from_essence_no_up.sh \
	${TOTAL_TIMEOUT} ${ESSENCE} ${EPRIME} {}" \
	:::: <(find  "${PARAMS_DIR}" -name '*.param') 2>&1 | tee "output.log"

# Save the results
mv "${GENERATED_OUTPUT_DIR}/"*.minion-solution \
	 "${GENERATED_OUTPUT_DIR}/"*.eprime-param.aux \
	 "${GENERATED_OUTPUT_DIR}/"*.eprime-param \
"${GENERATED_SOLUTIONS_DIR}"


pushd "${GENERATED_OUTPUT_DIR}/.."
base_out="$(basename "${GENERATED_OUTPUT_DIR}")"
tar -c "${base_out}" | pigz -c -p"${NUM_JOBS}" > "${base_out}".tar.gz
rm -rf "${base_out}"
popd


pushd "${GENERATED_SOLUTIONS_DIR}"
function minions(){
    newlines="$(head -n5 "$1" | egrep -cv '\S')"
    all=$( head -n5 "$1" | wc -l "$1" | egrep -o '^ *[0-9]+ ' | egrep -o '[0-9]+')

    if [ "$all" -eq "${newlines}" ]; then
        echo "Deleting $1 it's empty "
        find . -name "$2*" -delete
        echo "$2" >> "empty.log"
    fi
}
export -f minions

parallel -j1 --tag "minions {} {/.}" ::: "$(find . -name '*.minion-solution')"


# # mimics wc -l *.minion-solution but a lot faster (seconds instead of hours for a 1tb file)
"${OUR_DIR}/count_lines.sh"

# slow wc -l
# wc -l *.minion-solution | sed '$ d'  > solutions.counts


popd
