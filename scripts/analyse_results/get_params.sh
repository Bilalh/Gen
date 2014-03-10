#!/bin/bash
set -o nounset

base_dir="${1}"
base_dir="/Users/bilalh/Desktop/Experiment"
from_dir="${base_dir}/${2}/params"

cd "${base_dir}"


function gather_params(){

local essence_name="${1}"
local names="${2}"
local out_dir="${3}"

mkdir -p "${out_dir}"

while read line line_outdir mode eprimes ; do
	echo "Getting param $line"

	local data_dir="${out_dir}/${line}-data"
	mkdir "${data_dir}"

	local essence_param="$(find "${line_outdir}" -type f  -path "*${essence_name}*" -name "${line}.param")"
	cp "${essence_param}" "${out_dir}/${line}.essence-param"

	essence_dir="${base_dir}/${line_outdir}/../../../essences/${essence_name}"
	if [  ! -d  "${essence_dir}" ]; then
		essence_dir="${base_dir}/${line_outdir}/../../../../essences/${essence_name}"
	fi

	cp "${essence_dir}/${essence_name}.essence" "${out_dir}/${essence_name}.essence"


	local stat_tar="$(find "${line_outdir}" -type f -path "*${essence_name}*"  -name "${line}.stats.tar.gz")"
	local param_tar="$(find "${line_outdir}" -type f -path "*${essence_name}*"  -name "${line}.params.tar.gz")"
	local minion_tar="$(find "${line_outdir}" -type f -path "*${essence_name}*"  -name "${line}.minions.tar.gz")"

	echo "dasdsd ${mode} $eprimes"
	IFS=,
	for eprime in ${eprimes}; do
		echo "doing ${eprime}"
		tar --extract -C "${data_dir}" --file="$stat_tar" "./${eprime}*.minion-solution"
		tar --extract -C "${data_dir}" --file="$minion_tar" "./${eprime}*.minion.aux"
		# tar --extract -C "${data_dir}" --file="$stat_tar" "./${eprime}*.minion.aux"
		tar --extract -C "${data_dir}" --file="$param_tar" "./${eprime}*.eprime-param"

		cp "${essence_dir}/${essence_name}-${mode}/${eprime}.eprime" "${data_dir}/${eprime}.eprime"
		cp "${essence_dir}/${essence_name}-${mode}/${eprime}.eprime.logs" "${data_dir}/${eprime}.eprime.logs"


	done
	unset IFS

	find "${data_dir}" -size 0 -delete


done < "${names}"

}

export -f gather_params



gather_params "prob038-steel" "${from_dir}/prob038-steel/sat_names2.txt"  "${from_dir}/prob038-steel/sat"
gather_params "prob038-steel" "${from_dir}/prob038-steel/unsat_names2.txt"  "${from_dir}/prob038-steel/unsat"
