#!/bin/bash
set -o nounset
set -o errexit

set -x

spec_dir="$1"
bin_dir="$2"
out_dir="$3"

pushd "${bin_dir}"
export PATH=$PWD:$PATH
popd

cores=${NUM_CORES:-$(parallel --number-of-cores)}

"${PARAM_GEN_SCRIPTS}/toolchain/toolchain_recheck.py" "${spec_dir}" "${out_dir}"\
	--new_conjure "${cores}"


set +x
