#!/bin/bash
set -o nounset

if [ $# -lt 3 ]; then
	echo "$0  base_dir out_name  results_dir"
	echo "All filepaths are made relative to base_dir"
	exit 0
fi

exp_dir=$1
out=$2
results=$3


./create_csv.py "${NUM_JOBS:-8}" "${exp_dir}/${out}/all.csv" "${exp_dir}"  \
	`find "${results}" -type d -name results -print | parallel -j"${NUM_JOBS:-8}" "echo {}/.."`