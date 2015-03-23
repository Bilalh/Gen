#/bin/bash
# gen essence on all cores, logged and datestamped
set -o nounset

if ( ! gen --toolchain-path >/dev/null ); then
    exit 1;
fi

if [ $# -lt 2 ]; then
	echo "$0 <base>  <gen-args>"
	exit 1
fi

base="$1";
cmd="$2";
shift;
shift;

mkdir -p "$base";
dat="$(date +%F_%H-%M_%s)";
mkdir "${base}/${dat}";

cores=${CORES:-"$(parallel --number-of-cores)"};

[ -n "${TERM}" ] && export COLUMNS="$(tput cols)"

gen "${cmd}" -o "${base}/${dat}" --cores "$cores" "$@" 2>&1 | tee "${base}/${dat}/_all.logged" ;
