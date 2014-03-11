#!/bin/bash
# Bilal Syed Hussain

# file path to ourself, to allow sourcing
__convenience_fp=$0

# print the command the run it
function echoing(){
    echo "$@"
    "$@"
}

# refine params then solve them on all eprimes
function refine_run(){
	if [ $# -lt 3 ]; then
		echo "$0 [all] essence ::: eprime+ ::: param+ "
	else
		local __all_solution=0
		if [ "$1" = "all" ]; then
			__all_solution=1
			shift
		fi
		export __all_solution

		local essence=$1
		shift

		parallel --keep-order --tagstring  "{2/.} {1/.}" -j${NUM_JOBS:-4} \
			"source ${__convenience_fp}; __refine_par_func {2} {1} ${essence}" "$@"
	fi
}


function __refine_par_func(){
	local param=$1
	local eprime=$2
	local essence=$3

	local eprime_base=`basename ${eprime}`
	local param_base=`basename ${param}`
	local out_eparam="${eprime_base%.*}-${param_base%.*}.eprime-param"
	local eprime_solution="${eprime_base%.*}-${param_base%.*}.eprime-solution"

	refine_param "$@" && \
	[ -f ${out_eparam} ] && \
	solve_eprime_param ${eprime} ${out_eparam} && \
	[ -f $( ls ${eprime_solution}* | head -n 1 ) ]
	up_eprime ${eprime_solution}  ${param}  ${essence} ${eprime}
}


function refine_param(){
    local param=$1
	local eprime=$2
	local essence=$3

	local param_base=`basename ${param}`
	local eprime_base=`basename ${eprime}`
	local out_param="${eprime_base%.*}-${param_base%.*}.eprime-param"

	echoing conjure --mode refineParam --in-eprime ${eprime} --in-essence-param ${param} --in-essence ${essence} --out-eprime-param ${out_param}

}

function solve_eprime_param(){
	local eprime=$1
	local eparam=$2

	local param_base=`basename ${eparam}`
	local out_minion="${param_base%.*}.minion"
	local out_solution="${param_base%.*}.eprime-solution"

	rm ${out_solution}*

	extra=""
	if [ ${__all_solution} -eq 1 ]; then
		extra='-all-solutions'
	fi

	echoing savilerow -in-eprime ${eprime} -in-param ${eparam} -run-minion minion -out-minion ${out_minion} -out-solution ${out_solution} ${extra}

}

function up_eprime(){
	local eprime_solution=$1
	local param=$2
	local essence=$3
	local eprime=$4

	local eprime_solution_base=`basename ${eprime_solution}`
	local solution="${eprime_solution_base%.*}.solution"

	rm -f ${solution}*

	if [ ${__all_solution} -eq 1 ]; then
		cmd="
		sol=\$( echo '{}' | sed -e 's/eprime-solution/solution/g'  );
		conjure
			--mode translateSolution
			--in-eprime ${eprime}
			--in-eprime-param ${param}
			--in-essence ${essence}
			--in-eprime-solution {} \
			--in-essence-param ${param}
			--out-solution \${sol}
		&& head -n 10 \${sol}  | sed '1d' "

		echo $cmd
		parallel -j${NUM_JOBS_ALL_SOLS:-1} $cmd ::: ${eprime_solution}.*

	else

		echoing conjure \
			--mode translateSolution \
			--in-eprime ${eprime} \
			--in-eprime-param ${param} \
			--in-essence ${essence} \
			--in-eprime-solution ${eprime_solution} \
			--in-essence-param ${param} \
			--out-solution ${solution} \
		&& head -n 10 ${solution} | sed '1d'
	fi

}


if [ $ZSH_VERSION ]; then

function _solve_eprime_param(){
	_arguments \
		"1:eprimes:_files -g \*.eprime" \
		"2:params:_files -g \*.eprime-param"
}

compdef _solve_eprime_param solve_eprime_param


function _refine_param(){
	_arguments \
		"1:essence param:_files -g \*.param" \
		"2:eprime:_files -g \*.eprime" \
		"3:essence:_files -g \*.essence"
}

compdef _refine_param refine_param

function _refine_run(){
	_arguments \
		"1:essence:_files -g \*.essence"\
		"2:eprime:_files -g \*.eprime" \
		"*:essence param:_files -g \*.param"
}

compdef _refine_run refine_run


function _up_eprime(){
	_arguments \
		"1:eprime solution:_files -g \*.eprime-solution" \
		"2:essence param:_files -g \*.param" \
		"3:essence:_files -g \*.essence"
}


compdef _up_eprime up_eprime

fi