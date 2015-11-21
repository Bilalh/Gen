#!/bin/bash
set -o nounset

# Get the knapsack essence params, base on *names.txt
# Find Function1D version from the tar files
# use sed to convert to domain params

base_dir="/Users/bilalh/Desktop/Experiment"
ks_dir="${base_dir}/azure/12h_KS"

out="${base_dir}/_data_azure/ks_params"
sat_names="${base_dir}/_data_azure/ks_sat_names.txt"
unsat_names="${base_dir}/_data_azure/ks_unsat_names.txt"

out_sat="${out}/sat"
out_unsat="${out}/unsat"

mkdir -p "${out_sat}"
mkdir -p "${out_unsat}"

function gather_params(){

names="${1}"
out_dir="${2}"

while read line; do
	echo "Getting param and converting to a dominion-param $line"
	tar_file="$(find "${ks_dir}" -type f -name "${line}.params.tar.gz")"
	tar --extract -C "${out_dir}" --file="$tar_file" "./0001*.eprime-param"
	essence_param="$(find "${ks_dir}" -type f -name "${line}.param")"
	cp "${essence_param}" "${out_dir}/${line}.essence-param"

sed -E 's/; int\([0-9].*\)//g' "${out_dir}/0001-${line}.eprime-param" \
	| sed -e 's/be/=/g' \
		-e 's/_Function1D//g'  \
		-e 's/letting n/letting num_entries/g' \
		-e 's/letting totalWeight/letting n/g' \
		-e 's/language ESSENCE.*//g' \
	> "${out_dir}/0001-${line}.dominion-param"

done < "${names}"

}

gather_params ${sat_names} ${out_sat}
gather_params ${unsat_names} ${out_unsat}
