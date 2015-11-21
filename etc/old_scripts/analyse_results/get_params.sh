#!/bin/bash
set -o nounset

base_dir="${1}"
from_dir="${base_dir}/${2}"

export base_dir
export from_dir

cd "${base_dir}"


function gather_params(){

local essence_name="${1}"
local names="${2}"
local out_dir="${3}"

mkdir -p "${out_dir}"

while read line line_outdir mode method eprimes ; do
	echo "Getting param $line"

	local data_dir="${out_dir}/${line}-data"
	mkdir -p "${data_dir}"

	local essence_param="$(find "${line_outdir}" -type f  -path "*${essence_name}*" -name "${line}.param")"
	cp "${essence_param}" "${out_dir}/${line}.essence-param"

	essence_dir="${base_dir}/${line_outdir}/../../../essences/${essence_name}"
	if [  ! -d  "${essence_dir}" ]; then
		essence_dir="${base_dir}/${line_outdir}/../../../../essences/${essence_name}"
	fi

	cp "${essence_dir}/${essence_name}.essence" "${out_dir}/${essence_name}.essence"

	echo "${line}" >> "${out_dir}/${method}.txt"

	if [ -n "${ESSENCE_PARAM_ONLY:-}"  ]; then
	 continue
	fi

	local stat_tar="$(find "${line_outdir}" -type f -path "*${essence_name}*"  -name "${line}.stats.tar.gz")"
	local param_tar="$(find "${line_outdir}" -type f -path "*${essence_name}*"  -name "${line}.params.tar.gz")"
	local minion_tar="$(find "${line_outdir}" -type f -path "*${essence_name}*"  -name "${line}.minions.tar.gz")"

	function process(){
		eprime="$1"
		job_id="$2"
		echo "staring"
		tar --extract -C "${data_dir}" --file="$stat_tar" "./${eprime}*.minion-solution"
		tar --extract -C "${data_dir}" --file="$minion_tar" "./${eprime}*.minion.aux"
		# tar --extract -C "${data_dir}" --file="$stat_tar" "./${eprime}*.minion.aux"
		tar --extract -C "${data_dir}" --file="$param_tar" "./${eprime}*.eprime-param"

		cp "${essence_dir}/${essence_name}-${mode}/${eprime}.eprime" "${data_dir}/${eprime}.eprime"
		cp "${essence_dir}/${essence_name}-${mode}/${eprime}.eprime.logs" "${data_dir}/${eprime}.eprime.logs"

		find "${data_dir}" -size 0 -delete

		pushd "${data_dir}"

		if [[  -z "${NO_UP:-}" && ${job_id} -lt 10 &&  -f "${eprime}-${line}".minion-solution ]]; then
			echo "up "
			savilerow -mode ReadSolution -in-eprime "${eprime}".eprime -minion-sol-file "${eprime}-${line}".minion-solution \
				 -in-param "${eprime}-${line}".eprime-param -out-solution "${eprime}-${line}".eprime-solution \
				 -out-minion "${eprime}-${line}".minion

			conjure                                                             \
			    --mode translateSolution                                        \
			    --in-essence            ../"${essence_name}".essence            \
			    --in-essence-param      ../"${line}".essence-param              \
			    --in-eprime             "${eprime}".eprime                      \
			    --in-eprime-param       "${eprime}-${line}".eprime-param        \
			    --in-eprime-solution    "${eprime}-${line}".eprime-solution     \
			    --out-essence-solution   "${eprime}-${line}".solution
		fi

		popd
	}
	export essence_name
	export essence_dir
	export mode
	export line

	export data_dir
	export stat_tar
	export minion_tar
	export param_tar

	export -f process

 	parallel  --tagstring "{} {#}" -j"${NUM_JOBS:-4}" "process {} {#}; " ::: $(IFS=,; for i in $eprimes; do echo $i; done)


done < "${names}"

}

export -f gather_params


parallel --tagstring "{/.}" -j3 "gather_params  \$( basename {//} ) {}  {//}/{/.}  " \
	:::  `ls "${from_dir}"/*/*names2.txt`

