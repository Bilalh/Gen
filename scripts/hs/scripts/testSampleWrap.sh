#/bin/bash
set -o nounset

base=$1;
shift;

mkdir -p "$base"
$PARAM_GEN_SCRIPTS/toolchain/save_version.sh "${base}/"

testSample  -b "${base}/$(date +%F_%H-%M_%s)" "$@" ;