#/bin/bash
set -o nounset

base=$1;
shift;

mkdir -p "$base"
dat="$(date +%F_%H-%M_%s)"
mkdir "${base}/${dat}"

cores=${CORES:-"$(parallel --number-of-cores)"}

gen essence "${base}/${dat}" --cores "$cores" "$@" 2>&1 | tee "${base}/${dat}/_all.logged" ;

"$PARAM_GEN_SCRIPTS/toolchain/save_version.sh" "${base}/${dat}/zver@"
