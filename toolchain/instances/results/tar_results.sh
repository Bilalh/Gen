#!/bin/bash
set -o nounset

# Tar files so that I don't have too many files
# and for convenience since they grouped by param

place=${1}
mode=${2}

if [  -d "${place}/results_${mode}" ]; then
	pushd "${place}/results_${mode}"
else
	echo "${place}/results_${mode} does not exist from $PWD"
	exit 2
fi


Tar_dir="_tar"
mkdir -p ${Tar_dir}

#shellcheck disable=2153
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
find . -maxdepth 1 -name '*{/.}*' ! \( -name "*.minion" -o -name "*.eprime-param" -o -name "*.errors" \) \
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
	find . -maxdepth 1 -name '*{/.}*' ! \( -name '*.finished' -o -name "*.errors" \) -delete
EOF
)


parallel -j"${NUM_JOBS}" --tagstring "{/.}" "$Params" ::: ../_params/*

parallel -j"${NUM_JOBS}" --tagstring "{/.}" "$Stats" ::: ../_params/*

parallel -j2 --tagstring "{/.}" "$Minions" ::: ../_params/*
parallel -j"${NUM_JOBS}" --tagstring "{/.}" "$Remove" ::: ../_params/*

popd


# vim can read compressed text files

pushd "${place}"
find ..  -maxdepth 1 -name '*.output' | parallel  -j"${NUM_JOBS}" --tagstring "{/.}" "tar -czf  {}.tar.gz -C {//} {/}"
find ..  -maxdepth 1 -name '*.output' -delete


if [ -d "_param_gen" ]; then
	tar -c "_param_gen" | pigz -c -p"${NUM_JOBS}" > param_gen.tar.gz
	rm -rf "_param_gen"
fi

if [ -d "smac-output" ]; then
	tar -c "smac-output" | pigz -c -p"${NUM_JOBS}" > smac-output.tar.gz
	rm -rf "smac-output"
fi


tar -c "stats_${mode}" | pigz -c -p"${NUM_JOBS}" > "stats_${mode}.tar.gz"
rm -rf "stats_${mode}"

popd
