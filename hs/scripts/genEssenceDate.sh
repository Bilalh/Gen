#/bin/bash
set -o nounset

if ( ! gen --toolchain-path >/dev/null ); then
    exit 1
fi

base=$1;
shift;

mkdir -p "$base"
dat="$(date +%F_%H-%M_%s)"
mkdir "${base}/${dat}"

cores=${CORES:-"$(parallel --number-of-cores)"}

gen essence "${base}/${dat}" --cores "$cores" "$@" 2>&1 | tee "${base}/${dat}/_all.logged" ;

"$(gen --toolchain-path)/save_version.sh" "${base}/${dat}/zver@"
