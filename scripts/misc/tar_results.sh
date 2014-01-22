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


Tar_dir="_tar"
mkdir -p ${Tar_dir}

Params=$( cat <<EOF
echo "taring eprime-param" &&
tar czf ${Tar_dir}/{/.}.params.tar.gz \
	\$(ls *{/.}* | egrep  "eprime-param$")  
EOF
)

Stats=$( cat <<EOF
echo "taring stats" &&
tar czf ${Tar_dir}/{/.}.stats.tar.gz \
	\$(ls *{/.}* | egrep -v "minion$|minion.aux$|eprime-param$")  
EOF
)

Minions=$( cat <<EOF
echo "taring minions" &&
tar czf ${Tar_dir}/{/.}.minions.tar.gz \
	\$(ls *{/.}* | egrep "minion$|minion.aux$") 
EOF
)

Remove=$( cat <<EOF
echo "removing" &&
	rm \$(ls *{/.}* | egrep -v "^p-*{/.}")  
EOF
)



parallel -j${NUM_JOBS} --tagstring "finding finished {/.}" 'ls *{/.}*.zfinished > p-{/.}.finished' ::: ../params/*
find . -name 'p-*.finished' -empty -delete


parallel -j${NUM_JOBS} --tagstring "{/.}" $Params ::: ../params/*

parallel -j${NUM_JOBS} --tagstring "{/.}" $Stats ::: ../params/*

parallel -j${NUM_JOBS} --tagstring "{/.}" $Minions ::: ../params/*

# rm ? seems to not delete some files in parallel
parallel -j1 --tagstring "{/.}" $Remove ::: ../params/*

popd