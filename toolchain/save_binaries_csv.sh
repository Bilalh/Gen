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
	"gen_azure"
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
		gen-a*)          return 5;;
		'b.home')        return 6;;
		*)               return 7;;
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



conjureNew_version="$(conjureNew --version | egrep -o 'Repository version [0-9a-z]+' \
	| sed 's/Repository version //')"

if [ -n "${conjureNew_version}" ]; then
	vd="$(conjureNew --version | egrep -o '20[1-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9]+:[0-9]+:[0-9]+ [+-][0-9]+')"
	if (sw_vers &>/dev/null); then
		conjureNew_date="$(date -jf '%Y-%m-%e %H:%M:%S %z' "${vd}" '+%F_%s')"
	else
		conjureNew_date="$(date --date="${vd}" '+%F_%s')"
	fi
	echo "conjureNew,git,${conjureNew_version},${conjureNew_date},${rest_line}" >> "${base}/${csv_name}"
else
	# Conjure `new` pre-git, the date format was changed on 2016-04-29 on 4c172ad
	# Commit before that was c3baac0 2016-04-29
	# https://bitbucket.org/stacs_cp/conjure-private/commits/c3baac01e538334a93f5f0dec4c878a12e88ac8d
	# Problem what do if we use an old version from git, I not sure that would even work?
	conjureNew_version="$(conjure --version | egrep -o 'Version: \w+' | egrep -o ': \w+' | egrep -o '\w+')"

	vd="$(conjure --version | egrep -o '201[0-9]-[0-9][0-9]-[0-9][0-9] [0-9]+:[0-9]+ [+-][0-9]+')"

	if (sw_vers &>/dev/null); then
		conjureNew_date="$(date -jf '%Y-%m-%e %H:%M %z' "${vd}" '+%F_%s')"
	else
		conjureNew_date="$(date --date="${vd}" '+%F_%s')"
	fi

	echo "conjureNew,hg,${conjureNew_version},${conjureNew_date},${rest_line}" >> "${base}/${csv_name}"

fi


## conjureOld
if ( which conjureOld &> /dev/null ); then
	name=conjureOld

	version="$(conjureOld  2>&1 | egrep -o 'Version: \w+' | egrep -o ': \w+' | egrep -o '\w+')"
	vd="$(conjureOld 2>&1 | egrep -o '201[0-9]-[0-9][0-9]-[0-9][0-9] [0-9]+:[0-9]+ [+-][0-9]+')"

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
vd="$(savilerow  | egrep -o '201[0-9]-[0-9][0-9]-[0-9][0-9] [0-9]+:[0-9]+ [+-][0-9]+')"

if (sw_vers &>/dev/null); then
	version_date="$(date -jf '%Y-%m-%e %H:%M %z' "${vd}" '+%F_%s')"
else
	version_date="$(date --date="${vd}" '+%F_%s')"
fi

echo "${name},hg,${version},${version_date},${rest_line}" >> "${base}/${csv_name}"

name=minion
if ( which "${name}" &> /dev/null ); then

	version="$("${name}" | grep 'HG version:' | egrep -o '"\w+' | egrep -o '\w+')"

	set +o errexit
	# Stupid hg only added date functions recently
	#version_date_="$(hg log --template "{date(date, '%F_%s')}\n" --cwd "$(dirname "$(which "${name}")")" -r"${version}" 2>&1)"
	version_date_="$(hg log --template "{date}\n" --cwd "$(dirname "$(which "${name}")")" -r"${version}" 2>&1)"

	if [[ $? -eq 0  ]]; then
		if ( echo "_${version_date_}" | egrep -q "^_[0-9]+\.[0-9]+"  ); then
			version_date_="${version_date_:0:10}"
			if (sw_vers &>/dev/null); then
				version_date="$(date -jf '%s' "${version_date_}" '+%F_%s')"
			else
				version_date="$(date --date="@${version_date_}" '+%F_%s')"
			fi
		else
			version_date=""
		fi
	else
		version_date=""
	fi

	echo "${name},hg,${version},${version_date},${rest_line}" >> "${base}/${csv_name}"

	set -o errexit

fi


name=minion-wdeg
if ( which "${name}" &> /dev/null ); then

	version="$("${name}" | grep 'HG version:' | egrep -o '"\w+' | egrep -o '\w+')"

	set +o errexit
	# Stupid hg only added date functions recently
	#version_date_="$(hg log --template "{date(date, '%F_%s')}\n" --cwd "$(dirname "$(which "${name}")")" -r"${version}" 2>&1)"
	version_date_="$(hg log --template "{date}\n" --cwd "$(dirname "$(which "${name}")")" -r"${version}" 2>&1)"

	if [[ $? -eq 0  ]]; then
		if ( echo "_${version_date_}" | egrep -q "^_[0-9]+\.[0-9]+"  ); then
			version_date_="${version_date_:0:10}"
			if (sw_vers &>/dev/null); then
				version_date="$(date -jf '%s' "${version_date_}" '+%F_%s')"
			else
				version_date="$(date --date="@${version_date_}" '+%F_%s')"
			fi
		else
			version_date=""
		fi
	else
		version_date=""
	fi

	echo "${name},hg,${version},${version_date},${rest_line}" >> "${base}/${csv_name}"

	set -o errexit

fi



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
