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

NUM_JOBS_M=$((${NUM_JOBS}/2))
export NUM_JOBS_M

Params=$( cat <<EOF
echo "taring eprime-param" &&
tar -czf ${Tar_dir}/{/.}.params.tar.gz \
	\$(ls *{/.}* | egrep  "eprime-param$")
EOF
)

Stats=$( cat <<EOF
echo "taring stats" &&
tar -czf ${Tar_dir}/{/.}.stats.tar.gz \
	\$(ls *{/.}* | egrep -v "minion$|minion.aux$|eprime-param$")
EOF
)

Minions=$( cat <<EOF
echo "taring minions" &&
tar -c \$(ls *{/.}* | egrep "minion$|minion.aux$")  | pigz -c -p${NUM_JOBS_M} > ${Tar_dir}/{/.}.minions.tar.gz
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

parallel -j2 --tagstring "{/.}" $Minions ::: ../params/*

# rm ? seems to not delete some files in parallel
parallel -j1 --tagstring "{/.}" $Remove ::: ../params/*

popd


# vim can read compressed text files
pushd "${place}"
find  . -path '*/logs/*' -name 'log-*.log' | parallel  -j${NUM_JOBS} --tagstring "{/.}" "tar -cvzf  {}.tar.gz -C {//} {/}"
find  . -path '*/logs/*' -name 'log-*.log' -delete

find results/ -name 'stats-df-no-channelling-better' | parallel -j${NUM_JOBS} --tagstring "{/.}" "tar -czf  {}.tar.gz -C {//} {/}"
find results/ -name 'stats-df-no-channelling-better' -type d -exec rm -r {} \;

tar -c param_gen | pigz -c -p${NUM_JOBS_M} > param_gen.tar.gz
rm -rf param_gen

popd
