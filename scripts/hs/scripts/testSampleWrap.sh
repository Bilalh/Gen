#/bin/bash
set -o nounset

base=$1;
shift;

echo "testSample before ${testSample:-}"
export testSample=${testSample:-testSample}

mkdir -p "$base"
dat="$(date +%F_%H-%M_%s)"
mkdir "${base}/${dat}"

# "$PARAM_GEN_SCRIPTS/toolchain/save_version.sh" "${base}/${dat}/zver@"
cores=${CORES:-"$(parallel --number-of-cores)"}


${testSample}  -b "${base}/${dat}" --cores "$cores" "$@" 2>&1 | tee "${base}/${dat}/_all.logged" ;
