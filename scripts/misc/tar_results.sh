#!/bin/bash
set -o nounset

# Tar files so that I don't have too many files
# and for convenience since they grouped by param

place=${1}
mode=${2:-${MODE}}

if [  -d "${place}/results-${mode}" ]; then
	pushd "${place}/results-${mode}"
else
	echo "${place}/results-${mode} does not exist from $PWD"
	exit 2
fi


mkdir -p _stats

Command=$( cat <<EOF
echo "taring stats" &&
tar czf _stats/{/.}.tar.gz \
	\$(ls *{/.}* | egrep -v "minion$|minion.aux$")  
EOF
)

Minions=$( cat <<EOF
echo "taring minions" &&
tar czf _stats/{/.}.minions.tar.gz \
	\$(ls *{/.}* | egrep "minion$|minion.aux$") 
EOF
)

Remove=$( cat <<EOF
echo "removing" &&
	rm \$(ls *{/.}* | egrep -v "^p-*{/.}")  
EOF
)


parallel --tagstring "finding finished {/.}" 'ls *{/.}*.zfinished > p-{/.}.finished' ::: ../params/*
find . -name 'p-*.finished' -empty -delete


parallel --tagstring "{/.}" $Command ::: ../params/*

parallel --tagstring "{/.}" $Minions ::: ../params/*

# rm ? seems to not delete some files in parallel
parallel -j1 --tagstring "{/.}" $Remove ::: ../params/*

popd