#!/bin/bash
set -o nounset

# Tar logs

place=${1}

if [  -d "${place}" ]; then
	pushd "${place}"
else
	echo "${place} does not exist from $PWD"
	exit 2
fi


# vim can read compressed text files
find  . -path '*/logs/*' -name 'log-*.log' | parallel  -j${NUM_JOBS} --tagstring "{/.}" "tar -cvzf  {}.tar.gz -C {//} {/}" 
find  . -path '*/logs/*' -name 'log-*.log' -delete 
popd
