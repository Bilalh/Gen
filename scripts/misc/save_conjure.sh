#!/bin/bash
set -o nounset

# Save conjureNew

if [ -z "${1:-}" ]; then
	echo "Usage:"
	echo "$0 basedir"
	exit 1
fi

base="$1"

host_index=(
	"eno_ferry"
	"babbage_lovelace"
	"azure"
	"bh_laptop"
)

function host_selector() {
	case "$1" in
		eno*)            return 0;;
		ferry*)          return 0;;
		eno*)            return 0;;
		lovelace*)       return 1;;
		babbage*)        return 1;;
		'b.home')        return 3;;
		instancegen1*)   return 2;;

		vpn*.cs.st-andrews.ac.uk)
			return 3;;
		*)
			export "HOST_TYPE=<some_name>"
			exit 33
	esac
}

set -x
if [ -z "${HOST_TYPE:-}" ]; then
	host_selector "$(hostname)"
	host_type="${host_index[$?]}"
else
	host_type="${HOST_TYPE}"
fi

set -o errexit

cbase="${base}/conjureNew/${host_type}/"
mkdir -p "${cbase}"

conjureNewPath="$(which conjureNew)"

conjureNew_version="$(conjureNew --version | egrep -o 'Version: \w+' | egrep -o ': \w+' | egrep -o '\w+')"

conjureNew_date="$(conjureNew --version | egrep -o '201[0-9]-[0-9][0-9]-[0-9][0-9] [0-9]+:[0-9]+ \+[0-9]+')"

newDstDir="${cbase}/hash/${conjureNew_version}"
mkdir -p "${newDstDir}"

cp "${conjureNewPath}" "${newDstDir}/conjure"

pushd "${newDstDir}"
ln -sf conjure conjureNew
popd

dateDir="${cbase}/date/${conjureNew_date}"
mkdir -p "${dateDir}"

pushd "${dateDir}"
ln -sf "../../hash/${conjureNew_version}/conjure" conjure
ln -sf "../../hash/${conjureNew_version}/conjure" conjureNew
popd

set +x
