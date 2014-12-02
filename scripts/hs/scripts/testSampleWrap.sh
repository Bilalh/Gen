#/bin/bash
set -o nounset

base=$1;
shift;

echo "testSample before ${testSample:-testSample}"
export testSample=${testSample:-testSample}

mkdir -p "$base"
"$PARAM_GEN_SCRIPTS/toolchain/save_version.sh" "${base}/"
cores=${CORES:-"$(parallel --number-of-cores)"}



${testSample}  -b "${base}/$(date +%F_%H-%M_%s)" --cores "$cores" "$@" ;
