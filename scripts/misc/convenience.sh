#!/bin/bash
# Bilal Syed Hussain

# print the command the run it
function echoing(){
    echo "$@"
    "$@"
}

function refine_param(){
	set -o nounset
    param=$1
	eprime=$2
	essence=$3
	
	param_base=`basename ${param}`
	eprime_base=`basename ${eprime}`
	out_param="${eprime_base%.*}-${param_base%.*}.eprime-param"
	
	echoing conjure --mode refineParam --in-eprime ${eprime} --in-essence-param ${param} --in-essence ${essence} --out-eprime-param ${out_param}
	
}

function solve_eprime_param(){
	set -o nounset
	eprime=$1
	eparam=$2
	
	param_base=`basename ${eparam}`
	out_minion="${param_base%.*}.minion"
	out_solution="${param_base%.*}.eprime-solution"	
	
	echoing savilerow -in-eprime ${eprime} -in-param ${eparam} -run-minion minion -out-minion ${out_minion} -out-solution ${out_solution}

}

function up_eprime(){
	set -o nounset
	eprime_solution=$1
	param=$2
	essence=$3
	
	eprime_solution_base=`basename ${eprime_solution}`
	solution="${eprime_solution_base%.*}.solution"
	
	eprime_part="$( cut -d '-' -f 1 <<< "${eprime_solution_base%.*}" )"
	
	eprime="${eprime_part}.eprime"
	parm="${eprime_solution_base%.*}.eprime-param"
	
	echoing conjure \
		--mode translateSolution \
		--in-eprime ${eprime} \
		--in-eprime-param ${param} \
		--in-essence ${essence} \
		--in-eprime-solution ${eprime_solution} \
		--in-essence-param ${param} \
		--out-solution ${solution} \
	&& head -n 10 ${solution}
	
}

function refine_run(){
	set -o nounset
    param=$1
	eprime=$2
	essence=$3
	
	param_base=`basename ${param}`
	eprime_base=`basename ${eprime}`
	out_eparam="${eprime_base%.*}-${param_base%.*}.eprime-param"
	eprime_solution="${eprime_base%.*}-${param_base%.*}.eprime-solution"
	
	refine_param $@ && \
	solve_eprime_param ${eprime} ${out_eparam} && \
	up_eprime ${eprime_solution}  ${param}  ${essence}
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
compdef _refine_param refine_run


function _up_eprime(){
	_arguments \
		"1:eprime solution:_files -g \*.eprime-solution" \
		"2:essence param:_files -g \*.param" \
		"3:essence:_files -g \*.essence"
}

compdef _up_eprime up_eprime

fi