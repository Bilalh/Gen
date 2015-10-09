#!/bin/bash
set -o nounset

# Save binaries of
# conjure (conjureNew)
# conjureOld
# savilerow
# minion
# gen
# by date and hash so that compatible versions of the binaries can be rerun.


if [ -z "${1:-}" ]; then
	echo "Usage:"
	echo "$0 basedir"
	exit 1
fi

base="$1"
export BASE_DIR="${base}"

declare -a host_index
host_index=(
	"eno"
	"ferry"
	"babbage"
	"lovelace"
	"azure"
	"gen_azure"
	"bh_laptop"
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
		*)
			echo export "HOST_TYPE=<some_name>"
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
export host_type

set -o errexit
now="${DATE_VAL:-"$(date +'%F_%s')"}"

tbase_="date/${now}/${host_type}/"
tbase="${base}/date/${now}/${host_type}/"
mkdir -p "${tbase}"
echo "name,scm,hash,ver_date,uname,whoami,host_type,hostname" > "${tbase}/data.csv"
rest_line="$(uname),$(whoami),${host_type},$(hostname)"

cat << EOF > "${tbase}/meta.json"
{
	 "bin_dir"   : "date/${now}"
	,"host_type" : "${host_type}"
}
EOF


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

function relative_to(){
	rel="$(python3 -c "import sys; from pathlib import Path;  print( Path(sys.argv[1]).resolve().relative_to(Path('${2}').resolve()) )" "${1}")"
	echo "$rel"
}

function store_latest(){
	name="$1"
	host="$2"
	date="$3"
	loc="$4"

	rel="$(relative_to "${loc}" "${BASE_DIR}")"
	pushd "${BASE_DIR}"

	dir="latest/${name}"
	mkdir -p "${dir}"
	if [[  ( ! -d "${dir}/${host}" ) || ( ! -f  "${dir}/${host}_date" )  || (  "$(cat "${dir}/${host}_date" )" -lt "${date:11}" )  ]]; then
		[ -L "${dir}/${host}" ] &&  rm "${dir}/${host}"
		ln -s "../../${rel}" "${dir}/${host}"
		printf "%s" "${date:11}" > "$dir/${host}_date"
		echo "${date}"
	fi
	popd

}

## Conjure
cbase="${base}/versions/conjureNew/"
mkdir -p "${cbase}"

conjureNewPath="$(which conjure)"

conjureNew_version="$(conjure --version | egrep -o 'Version: \w+' | egrep -o ': \w+' | egrep -o '\w+')"

vd="$(conjure --version | egrep -o '201[0-9]-[0-9][0-9]-[0-9][0-9] [0-9]+:[0-9]+ [+-][0-9]+')"

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
echo "conjureNew,hg,${conjureNew_version},${conjureNew_date},${rest_line}" >> data.csv

popd

store_latest "conjure" "${host_type}" "${conjureNew_date}" "${newDstDir}"
pushd "${BASE_DIR}/latest"
[ ! -e "conjureNew" ] &&   ln -s conjure conjureNew
popd


## conjureOld
if ( which conjureOld &> /dev/null ); then
	name=conjureOld
	cbase="${base}/versions/${name}/"
	mkdir -p "${cbase}"

	binPath="$(which ${name})"

	version="$(conjureOld  2>&1 | egrep -o 'Version: \w+' | egrep -o ': \w+' | egrep -o '\w+')"
	vd="$(conjureOld 2>&1 | egrep -o '201[0-9]-[0-9][0-9]-[0-9][0-9] [0-9]+:[0-9]+ [+-][0-9]+')"

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
vd="$(savilerow  | egrep -o '201[0-9]-[0-9][0-9]-[0-9][0-9] [0-9]+:[0-9]+ [+-][0-9]+')"

if (sw_vers &>/dev/null); then
	version_date="$(date -jf '%Y-%m-%e %H:%M %z' "${vd}" '+%F_%s')"
else
	version_date="$(date --date="${vd}" '+%F_%s')"
fi

newDstDir="${cbase}/hash/${version}/${host_type}"
mkdir -p "${newDstDir}"

cp "${binPath}" "${newDstDir}/${name}"
cp "$(dirname "${binPath}")/savilerow.jar" "${newDstDir}/${name}.jar"

cat << EOF >  "${newDstDir}/savilerow2.sh"
#!/bin/bash
set -x
SR_DIR="\$( cd "\$( dirname "\$0" )" && pwd )"

java -ea -XX:ParallelGCThreads=1 \\
	-Xmx"\${JAVA_MEM:-16G}"       \\
	-jar "\$SR_DIR/savilerow.jar" \\
	"\$@"                         \\
	\${SR_ARGS}

set +x
EOF

chmod +x "${newDstDir}/savilerow2.sh"


pushd "${newDstDir}"
ln -fs "../../../../../${tbase_}" date
echo  "../../../../../${tbase_}" >> dates
popd

dateDir="${cbase}/date/${version_date}/${host_type}"
mkdir -p "${dateDir}"

pushd "${dateDir}"

cat << EOF > savilerow
#!/bin/bash
DIR="\$( cd "\$( dirname "\$0" )" && pwd )"
DIR="\${DIR}/../../../hash/${version}/${host_type}"
"\${DIR}/savilerow" "\$@"
EOF

cat << EOF > savilerow2.sh
#!/bin/bash
DIR="\$( cd "\$( dirname "\$0" )" && pwd )"
DIR="\${DIR}/../../../hash/${version}/${host_type}"
exec "\${DIR}/savilerow2.sh" "\$@"
EOF


chmod +x "./savilerow"
chmod +x "./savilerow2.sh"
popd

pushd "${tbase}"
echo "${name},hg,${version},${version_date},${rest_line}" >> data.csv

cat << EOF > savilerow
#!/bin/bash
DIR="\$( cd "\$( dirname "\$0" )" && pwd )"
DIR="\${DIR}/../../../versions/${name}/hash/${version}/${host_type}/"
"\${DIR}/savilerow" "\$@"
EOF

cat << EOF > savilerow2.sh
#!/bin/bash
DIR="\$( cd "\$( dirname "\$0" )" && pwd )"
DIR="\${DIR}/../../../versions/${name}/hash/${version}/${host_type}/"
exec "\${DIR}/savilerow2.sh" "\$@"
EOF


chmod +x "./savilerow"
chmod +x "./savilerow2.sh"
popd

store_latest "${name}" "${host_type}" "${version_date}" "${newDstDir}"


#Minion
name=minion
cbase="${base}/versions/${name}/"
mkdir -p "${cbase}"



binPath="$(which ${name})"
version="$(minion | grep 'HG version:' | egrep -o '"\w+' | egrep -o '\w+')"

# Stupid hg only added date functions recently
#version_date_="$(hg log --template "{date(date, '%F_%s')}\n" --cwd "$(dirname "$(which minion)")" -r"${version}" 2>&1)"
version_date_="$(hg log --template "{date}\n" --cwd "$(dirname "$(which minion)")" -r"${version}" 2>&1)"

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


newDstDir="${cbase}/hash/${version}/${host_type}"
mkdir -p "${newDstDir}"

cp "${binPath}" "${newDstDir}/${name}"

pushd "${newDstDir}"
ln -fs "../../../../../${tbase_}" date
echo  "../../../../../${tbase_}" >> dates
popd

pushd "${tbase}"
echo "${name},hg,${version},${version_date},${rest_line}" >> data.csv
ln -sf "../../../versions/${name}/hash/${version}/${host_type}/${name}" "${name}"
popd


#gen etc..

function gen_save(){
	name="gen"
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
	cp -r "$(gen --toolchain-path)/" toolchain
	ln -fs "../../../../../${tbase_}" date
	echo  "../../../../../${tbase_}" >> dates
	popd

	dateDir="${cbase}/date/${version_date}/${host_type}"
	mkdir -p "${dateDir}"

	pushd "${dateDir}"
	ln -sf "../../../hash/${version}/${host_type}/${name}" "${name}"
	ln -nsf "../../../hash/${version}/${host_type}/toolchain" "toolchain"
	popd

	pushd "${tbase}"
	echo "${name},git,${version},${version_date},${rest_line}" >> data.csv

	ln -sf "../../../versions/${name}/hash/${version}/${host_type}/${name}" "${name}"
	ln -nsf "../../../versions/${name}/hash/${version}/${host_type}/toolchain" "toolchain"
	popd
	store_latest "${name}" "${host_type}" "${version_date}" "${newDstDir}"
}

gen_save

set +x
