#!/bin/bash
# save a csv of the tools used
set -o nounset


if [ -z "${1:-}" ]; then
	echo "Usage:"
	echo "$0 basedir [filename]"
	echo "export HOST_TYPE=<some_name> if not on a compute server for better data"
	exit 1
fi

base="$1"
csv_name="${2:-versions.csv}"

declare -a host_index
host_index=(
	"eno"
	"ferry"
	"babbage"
	"lovelace"
	"azure"
	"bh_laptop"
	"$(whoami)_$( hostname | awk -F. '{print $1}' )"
)


function host_selector() {
	case "$1" in
		eno*)            return 0;;
		ferry*)          return 1;;
		babbage*)        return 2;;
		lovelace*)       return 3;;
		instancegen1*)   return 4;;
		'b.home')        return 5;;
		*)               return 6;;
	esac
}

if [ -z "${HOST_TYPE:-}" ]; then
	host_selector "$(hostname)"
	host_type="${host_index[$?]}"
else
	host_type="${HOST_TYPE}"
fi


mkdir -p "${base}"
echo "name,scm,hash,ver_date,uname,whoami,host_type,hostname" > "${base}/${csv_name}"
rest_line="$(uname),$(whoami),${host_type},$(hostname)"



## Conjure
conjureNew_version="$(conjure --version | egrep -o 'Version: \w+' | egrep -o ': \w+' | egrep -o '\w+')"

vd="$(conjure --version | egrep -o '201[0-9]-[0-9][0-9]-[0-9][0-9] [0-9]+:[0-9]+ \+[0-9]+')"

if (sw_vers &>/dev/null); then
	conjureNew_date="$(date -jf '%Y-%m-%e %H:%M %z' "${vd}" '+%F_%s')"
else
	conjureNew_date="$(date --date="${vd}" '+%F_%s')"
fi

echo "conjureNew,hg,${conjureNew_version},${conjureNew_date},${rest_line}" >> "${base}/${csv_name}"


## conjureOld
if ( which conjureOld &> /dev/null ); then
	name=conjureOld

	version="$(conjureOld  2>&1 | egrep -o 'Version: \w+' | egrep -o ': \w+' | egrep -o '\w+')"
	vd="$(conjureOld 2>&1 | egrep -o '201[0-9]-[0-9][0-9]-[0-9][0-9] [0-9]+:[0-9]+ \+[0-9]+')"

	if (sw_vers &>/dev/null); then
		version_date="$(date -jf '%Y-%m-%e %H:%M %z' "${vd}" '+%F_%s')"
	else
		version_date="$(date --date="${vd}" '+%F_%s')"
	fi

	echo "${name},hg,${version},${version_date},${rest_line}" >> "${base}/${csv_name}"
fi



## savilerow
name=savilerow

version="$(savilerow | egrep -o 'Version: \w+' | egrep -o ': \w+' | egrep -o '\w+')"
vd="$(savilerow  | egrep -o '201[0-9]-[0-9][0-9]-[0-9][0-9] [0-9]+:[0-9]+ \+[0-9]+')"

if (sw_vers &>/dev/null); then
	version_date="$(date -jf '%Y-%m-%e %H:%M %z' "${vd}" '+%F_%s')"
else
	version_date="$(date --date="${vd}" '+%F_%s')"
fi

echo "${name},hg,${version},${version_date},${rest_line}" >> "${base}/${csv_name}"

#Minion
name=minion

version="$(minion | grep 'HG version:' | egrep -o '"\w+' | egrep -o '\w+')"

set +o errexit
set -x
version_date_="$(hg log --template "{date(date, '%F_%s')}\n" --cwd "$(dirname "$(which minion)")" -r"${version}" 2>&1)"
set +x

if [[ $? -eq 0  ]]; then
	if ( echo "${version_date}_" | egrep -q "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]_"  ); then
		version_date="${version_date_}"
	else
		version_date=""
	fi
else
	version_date=""
fi

echo "${name},hg,${version},${version_date},${rest_line}" >> "${base}/${csv_name}"

set -o errexit

#gen
if ( which gen &> /dev/null ); then
	name="gen"
	version="$("${name}" --version | grep 'Git version' | egrep -o "'\w+" | egrep -o '\w+')"

	vd="$("${name}" --version | egrep 'Git version' | egrep -o '\w+,.*[0-9]' )"

	if (sw_vers &>/dev/null); then
		version_date="$(date -jf '%a, %d %b %Y %T %z' "${vd}" '+%F_%s')"
	else
		version_date="$(date --date="${vd}" '+%F_%s')"
	fi

	echo "${name},git,${version},${version_date},${rest_line}" >> "${base}/${csv_name}"
fi

