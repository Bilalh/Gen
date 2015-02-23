#!/bin/bash
set -o nounset

# Save binaries of
# conjureNew (conjure)
# savilerow
# minion
# testReduce
# testSample
# ${testSampleName}
#
# by date and hash so that compatible versions of the binaries can be rerun.


if [ -z "${1:-}" ]; then
	echo "Usage:"
	echo "$0 basedir"
	exit 1
fi

base="$1"

declare -a host_index
host_index=(
	"eno"
	"ferry"
	"babbage"
	"lovelace"
	"azure"
	"bh_laptop"
)


function host_selector() {
	case "$1" in
		eno*)            return 0;;
		ferry*)          return 1;;
		babbage*)        return 2;;
		lovelace*)       return 3;;
		'b.home')        return 5;;
		instancegen1*)   return 4;;

		vpn*.cs.st-andrews.ac.uk)
			return 5;;
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
now="${DATE_PREFIX:-}$(date +'%F_%s')"

tbase_="date/${now}/${host_type}/"
tbase="${base}/date/${now}/${host_type}/"
mkdir -p "${tbase}"
echo "name,scm,hash,ver_date,uname,whoami,host_type,hostname" > "${tbase}/data.csv"
rest_line="$(uname),$(whoami),${host_type},$(hostname)"

declare -a cmds
cmds=(python python3 pip sqlite3 git hg perl parallel ruby ghc cabal gcc clang pigz pip3 )
for prog in "${cmds[@]}"; do
	val=$($prog --version  2>&1 | cat | head -n3)
	printf "***${prog}\n%s\n" "${val}" >> "${tbase}/data_other.txt"
	echo "+++" >> "${tbase}/data_other.txt"
done

# Java just had too be different
prog=java
val="$(java -version  2>&1 | cat)"
printf "***${prog}\n%s\n" "${val}" >> "${tbase}/data_other.txt"
echo "+++" >> "${tbase}/data_other.txt"


## Conjure
cbase="${base}/versions/conjureNew/"
mkdir -p "${cbase}"

conjureNewPath="$(which conjureNew)"

conjureNew_version="$(conjureNew --version | egrep -o 'Version: \w+' | egrep -o ': \w+' | egrep -o '\w+')"

vd="$(conjureNew --version | egrep -o '201[0-9]-[0-9][0-9]-[0-9][0-9] [0-9]+:[0-9]+ \+[0-9]+')"

if (sw_vers &>/dev/null); then
	conjureNew_date="$(date -jf '%Y-%m-%e %H:%M %z' "${vd}" '+%F_%s')"
else
	conjureNew_date="$(date --date="${vd}" '+%F_%s')"
fi

newDstDir="${cbase}/hash/${conjureNew_version}/${host_type}"
mkdir -p "${newDstDir}"

cp "${conjureNewPath}" "${newDstDir}/conjure"

pushd "${newDstDir}"
ln -sf conjure conjureNew

ln -fs "../../../../../${tbase_}" date
echo  "../../../../../${tbase_}" >> dates

popd

dateDir="${cbase}/date/${conjureNew_date}/${host_type}"
mkdir -p "${dateDir}"
pushd "${dateDir}"
ln -sf "../../../hash/${conjureNew_version}/${host_type}/conjure" conjure
ln -sf "../../../hash/${conjureNew_version}/${host_type}/conjure" conjureNew
popd

pushd "${tbase}"
ln -sf "../../../versions/conjureNew/hash/${conjureNew_version}/${host_type}/conjure" conjure
ln -sf "../../../versions/conjureNew/hash/${conjureNew_version}/${host_type}/conjure" conjureNew
echo "conjureNew,git,${conjureNew_version},${conjureNew_date},${rest_line}" >> data.csv

popd

## conjureOld
if ( which conjureOld &> /dev/null ); then
	name=conjureOld
	cbase="${base}/versions/${name}/"
	mkdir -p "${cbase}"

	binPath="$(which ${name})"

	version="$(conjureOld  2>&1 | egrep -o 'Version: \w+' | egrep -o ': \w+' | egrep -o '\w+')"
	vd="$(conjureOld 2>&1 | egrep -o '201[0-9]-[0-9][0-9]-[0-9][0-9] [0-9]+:[0-9]+ \+[0-9]+')"

	if (sw_vers &>/dev/null); then
		version_date="$(date -jf '%Y-%m-%e %H:%M %z' "${vd}" '+%F_%s')"
	else
		version_date="$(date --date="${vd}" '+%F_%s')"
	fi

	newDstDir="${cbase}/hash/${version}/${host_type}"
	mkdir -p "${newDstDir}"

	cp "${binPath}" "${newDstDir}/${name}"


	pushd "${newDstDir}"
	ln -fs "../../../../../${tbase_}" date
	echo  "../../../../../${tbase_}" >> dates
	popd

	dateDir="${cbase}/date/${version_date}/${host_type}"
	mkdir -p "${dateDir}"

	pushd "${dateDir}"
	ln -sf "../../../hash/${version}/${host_type}/" "${name}"
	popd

	pushd "${tbase}"
	echo "${name},hg,${version},${version_date},${rest_line}" >> data.csv
	ln -sf "../../../versions/${name}/hash/${version}/${host_type}/${name}" "${name}"
	popd
fi



## savilerow
name=savilerow
cbase="${base}/versions/${name}/"
mkdir -p "${cbase}"

binPath="$(which ${name})"

version="$(savilerow | egrep -o 'Version: \w+' | egrep -o ': \w+' | egrep -o '\w+')"
vd="$(savilerow  | egrep -o '201[0-9]-[0-9][0-9]-[0-9][0-9] [0-9]+:[0-9]+ \+[0-9]+')"

if (sw_vers &>/dev/null); then
	version_date="$(date -jf '%Y-%m-%e %H:%M %z' "${vd}" '+%F_%s')"
else
	version_date="$(date --date="${vd}" '+%F_%s')"
fi

newDstDir="${cbase}/hash/${version}/${host_type}"
mkdir -p "${newDstDir}"

cp "${binPath}" "${newDstDir}/${name}"
cp "$(dirname "${binPath}")/savilerow.jar" "${newDstDir}/${name}.jar"


pushd "${newDstDir}"
ln -fs "../../../../../${tbase_}" date
echo  "../../../../../${tbase_}" >> dates
popd

dateDir="${cbase}/date/${version_date}/${host_type}"
mkdir -p "${dateDir}"

pushd "${dateDir}"

ln -sf "../../../hash/${version}/${host_type}/savilerow.jar" "${name}.jar"
cat << EOF > savilerow
#!/bin/bash
DIR="\$( cd "\$( dirname "\$0" )" && pwd )"
DIR="\${DIR}/../../../hash/${version}/${host_type}"
cd "\$DIR"
./savilerow "\$@"
EOF

chmod +x "./savilerow"
popd

pushd "${tbase}"
echo "${name},hg,${version},${version_date},${rest_line}" >> data.csv

ln -sf "../../../versions/${name}/hash/${version}/${host_type}/${name}" "${name}.jar"
cat << EOF > savilerow
#!/bin/bash
DIR="\$( cd "\$( dirname "\$0" )" && pwd )"
DIR="\${DIR}/../../../versions/${name}/hash/${version}/${host_type}/"
cd "\$DIR"
./savilerow "\$@"
EOF

chmod +x "./savilerow"
popd

#Minion
name=minion
cbase="${base}/versions/${name}/"
mkdir -p "${cbase}"

binPath="$(which ${name})"
version="$(minion | grep 'HG version:' | egrep -o '"\w+' | egrep -o '\w+')"

newDstDir="${cbase}/hash/${version}/${host_type}"
mkdir -p "${newDstDir}"

cp "${binPath}" "${newDstDir}/${name}"

pushd "${newDstDir}"
ln -fs "../../../../../${tbase_}" date
echo  "../../../../../${tbase_}" >> dates
popd

pushd "${tbase}"
echo "${name},hg,${version},,${rest_line}" >> data.csv
ln -sf "../../../versions/${name}/hash/${version}/${host_type}/${name}" "${name}"
popd


#testReduce etc..

function mine(){
	name="$1"
	cbase="${base}/versions/${name}/"
	mkdir -p "${cbase}"

	binPath="$(which "${name}")"
	version="$("${name}" --version | grep 'Git version' | egrep -o "'\w+" | egrep -o '\w+')"

	vd="$("${name}" --version | egrep 'Git version' | egrep -o '\w+,.*[0-9]' )"

	if (sw_vers &>/dev/null); then
		version_date="$(date -jf '%a, %d %b %Y %T %z' "${vd}" '+%F_%s')"
	else
		version_date="$(date --date="${vd}" '+%F_%s')"
	fi

	newDstDir="${cbase}/hash/${version}/${host_type}"
	mkdir -p "${newDstDir}"

	cp "${binPath}" "${newDstDir}/${name}"

	pushd "${newDstDir}"
	ln -fs "../../../../../${tbase_}" date
	echo  "../../../../../${tbase_}" >> dates
	popd

	dateDir="${cbase}/date/${version_date}/${host_type}"
	mkdir -p "${dateDir}"

	pushd "${dateDir}"
	ln -sf "../../../hash/${version}/${host_type}/${name}" "${name}"
	popd

	pushd "${tbase}"
	echo "${name},git,${version},${version_date},${rest_line}" >> data.csv

	ln -sf "../../../versions/${name}/hash/${version}/${host_type}/${name}" "${name}"
	popd
}

mine testReduce
mine testSample

if [ ! -z "${testSampleName:-}" ]; then
	mine "${testSampleName}"
fi

set +x
