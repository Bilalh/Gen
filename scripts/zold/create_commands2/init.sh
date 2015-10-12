#!/bin/bash

# some error checking for command we needed

export __commands_needed="ghc python3 pip sqlite3 git hg perl parallel pigz ruby"
__our_progs="minion conjure savilerow essenceGivensToFinds essenceGivensToJson2 essenceLettingsToJson"

for prog in $__commands_needed java $__our_progs; do
    if ! hash "$prog" 2>/dev/null; then
    	printf "\e[01;31m Need $prog to be in the PATH \e[0m\n"  >&2
        exit 1
    fi
done

__progs_to_compile="$PARAM_GEN_SCRIPTS/tools/cputimeout/cputimeout"
__progs_to_compile2="essenceGivensToFinds essenceGivensToJson2 essenceLettingsToJson"


for prog in $__progs_to_compile; do
	if [ !  -f "$prog" ]; then
		printf "\e[01;31m Compile $prog \e[0m\n"   >&2
		exit 2
	fi
done

# for prog in $__progs_to_compile2; do
# 	if [ !  -f "$PARAM_GEN_SCRIPTS/hs/bin/$prog" ]; then
# 		printf "\e[01;31m Compile $PARAM_GEN_SCRIPTS/hs/bin/$prog \e[0m\n"   >&2
# 		exit 2
# 	fi
# done


python3 <<EOF
import sys
assert sys.version_info.major == 3
assert sys.version_info.minor >= 3

import sqlite3
from pathlib import Path
import docopt
EOF

if [ $?  != 0 ]; then
	printf "\e[01;31m Need to install python modules \e[0m\n"   >&2
	exit 3
fi



function record(){
	if [[ -z "${1}" || -z "${2}" ]]; then
		echo "$0 name command"
	else
		local name="$1"
		shift
		if [ "`basename \"${name}\"`" != "${name}" ]; then
			mkdir -p "`dirname \"${name}\"`"
		fi
		local fp="${name}-`date +%F_%H-%M_%s`"
		echo "pwd:`pwd`"> "${fp}.cmds"
		echo "$@"  >> "${fp}.cmds"
		"$@" 2>&1 | tee "${fp}.log"
	fi
}


function record_cp(){
	if [[ -z "${1}" || -z "${2}" ]]; then
		echo "$0 name command"
	else
		local name="$1"
		shift
		if [ "`basename \"${name}\"`" != "${name}" ]; then
			mkdir -p "`dirname \"${name}\"`"
		fi

		local _fp="${name}-`date +%F_%H-%M_%s`"
		fp="`python -c 'import os,sys; print(os.path.realpath(sys.argv[1]))'  ${_fp}`"
		echo "pwd:`pwd`"> "${fp}.cmds"


		if ( git rev-parse --is-inside-work-tree ); then
			echo "repo(git) version" >> "${fp}.cmds"
			git log -1 --format="%H" >> "${fp}.cmds"
			git describe --always    >> "${fp}.cmds"
		fi

		echo "conjure">> "${fp}.cmds"
		conjure 2>&1 | grep Version						  >> "${fp}.cmds"
		minion 2>&1	 | egrep 'HG version|Minion Version' >> "${fp}.cmds"
		savilerow	 | head -n 2 | tail -n 1			  >> "${fp}.cmds"

		local sr="$(dirname `which savilerow`)"
		if [ -d "${sr}/.hg" ]; then
			pushd "${sr}"
			hg log -r . --template "{latesttag}-{latesttagdistance}-{node|short}" >> "${fp}.cmds"
			echo "" >> "${fp}.cmds"
			popd
		fi

		if [ -d "../instancegen/.git" ]; then
			pushd "../instancegen/"
			echo "instancegen(git) version" >> "${fp}.cmds"
			git log -1 --format="%H" >> "${fp}.cmds"
			git describe --always >> "${fp}.cmds"
			popd
		fi

		echo "" >> "${fp}.cmds"
		echo "" >> "${fp}.cmds"
		echo "##VERSIONS##"  >> "${fp}.cmds"
		echo "" >> "${fp}.cmds"
		for prog in $__commands_needed python; do
			echo "$prog version:" >> "${fp}.cmds"
			$prog --version  2>&1 | cat >> "${fp}.cmds"
			echo "" >> "${fp}.cmds"
		done

		# Java just had to be different
		echo "java version:" >> "${fp}.cmds"
		java -version  2>&1 | cat >> "${fp}.cmds"

		echo "###" >> "${fp}.cmds"
		echo "" >> "${fp}.cmds"
		echo "" >> "${fp}.cmds"

		echo "Command:"  >> "${fp}.cmds"
		echo "$@"        >> "${fp}.cmds"
		echo
		echo "<RUNNING> $@"
		echo
		"$@" 2>&1 | tee "${fp}.log"
		echo
		echo "<FINISHED> $@"
		echo
	fi
}

# only "True" is true
function to_bool(){
	if [ "$1" == "True" ]; then
		echo 1
	else
		echo 0
	fi
}

export -f record
export -f record_cp
export -f to_bool
