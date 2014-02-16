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

NUM_JOBS_M=$((NUM_JOBS/2))
export NUM_JOBS_M

Params=$( cat <<EOF
echo "taring eprime-param" &&
find . -maxdepth 1 -name '*{/.}*eprime-param' \
	| tar -czf ${Tar_dir}/{/.}.params.tar.gz --files-from -
EOF
)

Stats=$( cat <<EOF
echo "taring stats" &&
find . -maxdepth 1 -name '*{/.}*' ! \( -name "*.minion" -o -name "*.eprime-param" \) \
	|  tar -czf ${Tar_dir}/{/.}.stats.tar.gz --files-from -
EOF
)

Minions=$( cat <<EOF
echo "taring minions" &&
find . -name '*{/.}*' \( -name "*.minion" -or -name "*.minion.aux" \) \
	| tar -c  --files-from - \
	| pigz -c -p${NUM_JOBS_M} > ${Tar_dir}/{/.}.minions.tar.gz
EOF
)

Remove=$( cat <<EOF
echo "removing" &&
	find . -maxdepth 1 -name '*{/.}*' ! \( -name 'p-{/.}*.finished' \) -delete
EOF
)


parallel -j"${NUM_JOBS}" --tagstring "finding finished {/.}" 'find . -maxdepth 1 -name "*{/.}*.zfinished " > p-{/.}.finished' ::: ../params/*
find . -name 'p-*.finished' -empty -delete

parallel -j"${NUM_JOBS}" --tagstring "{/.}" "$Params" ::: ../params/*

parallel -j"${NUM_JOBS}" --tagstring "{/.}" "$Stats" ::: ../params/*

parallel -j2 --tagstring "{/.}" "$Minions" ::: ../params/*
parallel -j"${NUM_JOBS}" --tagstring "{/.}" "$Remove" ::: ../params/*

popd


# vim can read compressed text files

pushd "${place}"
find  . -path '*/logs/*' -name 'log-*.log' | parallel  -j"${NUM_JOBS}" --tagstring "{/.}" "tar -czf  {}.tar.gz -C {//} {/}"
find  . -path '*/logs/*' -name 'log-*.log' -delete


if [ -d "param_gen" ]; then
	tar -c "param_gen" | pigz -c -p"${NUM_JOBS}" > param_gen.tar.gz
	rm -rf "param_gen"
fi

if [ -d "smac-output" ]; then
	tar -c "smac-output" | pigz -c -p"${NUM_JOBS}" > smac-output.tar.gz
	rm -rf "smac-output"
fi

if [ -d "params/tmp" ]; then
	pushd params
	tar -c "tmp" | pigz -c -p"${NUM_JOBS}" > tmp.tar.gz
	rm -rf "tmp"
	popd
fi

tar -c "stats-${mode}" | pigz -c -p"${NUM_JOBS}" > "stats-${mode}.tar.gz"
rm -rf "stats-${mode}"

popd
